use ropey::Rope;
use tower_lsp::jsonrpc;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use itertools::Itertools;

use codespan_reporting::diagnostic::{Severity, LabelStyle};
use codespan_reporting::files::SimpleFile;
use circom_structure::error_definition::Report;
use circom_structure::file_definition::FileLibrary;
use circom_structure::template_data::TemplateData;
use circom_structure::function_data::FunctionData;
use circom_structure::abstract_syntax_tree::ast;

use std::fmt;
use std::sync::Mutex;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::helpers;

// TODO: need somehow to translate hover index to index after removal of comments

enum FileLibrarySource {
    ProgramArchive(ProgramArchive),
    FileLibrary(FileLibrary)
}

enum CommentParserState {
    Outside,
    MaybeInside,
    JustEntered,
    Inside,
    MaybeOutside
}

#[derive(Clone, Copy)]
enum DefinitionData<'a> {
    Template(&'a TemplateData),
    Function(&'a FunctionData)
}

enum StatementOrExpression<'a> {
    Statement(&'a ast::Statement),
    Expression(&'a ast::Expression),
    Contender(TokenType)
}

#[derive(Debug)]
enum TokenType {
    Variable(u32),
    Signal,
    Component,
    Defintion(DefintionType),
    Tag
}

#[derive(Debug)]
enum DefintionType {
    Template,
    Function
}

struct TokenInfo {
    name: String,
    token_type: TokenType,
}

impl TokenInfo {
    fn description(&self) -> String {
        format!("{}: {:?}", self.name, self.token_type)
    }
}

struct TextDocumentItem {
    uri: Url,
    text: String,
    version: Option<i32>,
}

// circom's program archive doesn't implement debug
struct ProgramArchive {
    inner: circom_structure::program_archive::ProgramArchive
}
impl ProgramArchive {
    pub fn new(inner: circom_structure::program_archive::ProgramArchive) -> ProgramArchive {
        ProgramArchive {
            inner
        }
    }
}

impl fmt::Debug for ProgramArchive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ProgramArchive")
         .finish()
    }
}

#[derive(Debug)]
struct DocumentData {
    content: Rope,
    archive: Option<ProgramArchive>,
}

#[derive(Debug)]
pub struct Backend {
    client: Client,
    document_map: Mutex<RefCell<HashMap<Url, DocumentData>>>,
}

impl Backend {
    pub fn new(client: Client) -> Backend { 
        Backend { 
            client,
            document_map: Mutex::new(RefCell::new(HashMap::new())),
        }
    }

    async fn on_change(&self, params: TextDocumentItem, publish_diagnostics: bool) {
        let path = helpers::uri_to_string(&params.uri);
        let rope = Rope::from_str(&params.text);

        // do not compute archive if publish_diagnostics flag is not set, this
        // is done to prevent from tons of calls to the circom compiler
        //
        // also, as of now circom's parser function doesn't seperate the file library creation
        // logic from the parseing, so it's impossible to run the parser on an intermediate buffer
        let archive = if publish_diagnostics {
            let (reports, file_library_source) = match circom_parser::run_parser(
                path, 
                "2.1.5",  // TODO: figure what this version number actually does
                vec![],  // TODO: add linked library support
            ) {
                Ok((mut archive, mut reports)) => {
                    let mut type_reports = match circom_type_checker::check_types::check_types(&mut archive) {
                        Ok(type_reports) => type_reports,
                        Err(type_reports) => type_reports
                    };
                    reports.append(&mut type_reports);
                    (reports, FileLibrarySource::ProgramArchive(ProgramArchive::new(archive)))
                },
                Err((file_library, reports)) => (reports, FileLibrarySource::FileLibrary(file_library))
            };

            let file_library = match &file_library_source {
                FileLibrarySource::ProgramArchive(archive) => &archive.inner.file_library,
                FileLibrarySource::FileLibrary(file_library) => &file_library
            };

            let diagnostics: Vec<_> = reports
                .into_iter()
                .map(|x| Self::report_to_diagnostic(x, &file_library, &params.uri))
                .collect();

            let mut main_file_diags = Vec::new();
            let mut other_files_diags: HashMap<_, Vec<_>> = HashMap::new();

            for (diagnostic, uri) in diagnostics {
                if uri == params.uri {
                    main_file_diags.push(diagnostic);
                } else if other_files_diags.contains_key(&uri) {
                    other_files_diags.get_mut(&uri).expect("other_files_diag should contain uri").push(diagnostic);
                } else {
                    other_files_diags.insert(uri, vec![diagnostic]);
                }
            }

            if let Some(other_files_error) = Self::other_files_diagnostic(&other_files_diags) {
                main_file_diags.push(other_files_error);
            };

            // publish main errors
            self.client
                .publish_diagnostics(params.uri.clone(), main_file_diags, params.version)
                .await;

            // publish errors in other files
            for (uri, diags) in other_files_diags {
                self.client
                    .publish_diagnostics(uri, diags, params.version)
                    .await;
            }

            match file_library_source {
                FileLibrarySource::ProgramArchive(x) => Some(x),
                _ => None
            }
        } else {
            None
        };

        let document_map = self.document_map.lock().expect("document map mutex poisened");
        // if new archive computed succesfully, insert it.
        // otherwise, use old archive
        let archive = match archive {
            Some(new_archive) => Some(new_archive),
            None => match document_map.borrow_mut().remove(&params.uri).map(|x| x.archive) {
                Some(existing_archive) => existing_archive,
                None => None
            }
        };

        let document = DocumentData {
            content: rope,
            archive
        };

        document_map.borrow_mut().insert(params.uri, document);
    }

    // file_library is needed to decide in what file does the report occurs
    fn report_to_diagnostic(report: Report, file_library: &FileLibrary, main_uri: &Url) -> (Diagnostic, Url) {
        let diagnostic = report.to_diagnostic();

        let label = diagnostic.labels.into_iter().reduce(|cur, a| {
            match (cur.style, a.style) {
                (LabelStyle::Primary, LabelStyle::Secondary) => cur,
                (LabelStyle::Secondary, LabelStyle::Primary) => a,
                _ => if cur.range.start < a.range.start {
                    cur
                } else {
                    a
                }
            }
        });

        let (url, range) = match label {
            Some(label) => { 
                let simple_file = file_library
                    .to_storage()
                    .get(label.file_id)
                    .expect("invalid file_id from label");

                let uri = helpers::string_to_uri(simple_file.name());
                let rope = Rope::from_str(simple_file.source());

                (uri, Range {
                    start: helpers::char_to_position(&rope, label.range.start).expect("valid label range start"),
                    end: helpers::char_to_position(&rope, label.range.end).expect("valid label range end")
                })
            },
            None => (main_uri.clone(), Range {
                start: Position { line: 0, character: 0 },
                end: Position { line: 0, character: 0 }
            })
        };

        let severity = match diagnostic.severity {
            Severity::Bug | Severity::Error => DiagnosticSeverity::ERROR,
            Severity::Warning => DiagnosticSeverity::WARNING,
            Severity::Help => DiagnosticSeverity::HINT,
            Severity::Note => DiagnosticSeverity::INFORMATION
        };

        let message = diagnostic.message;

        (Diagnostic {
            range,
            severity: Some(severity),
            code: None,
            code_description: None,
            source: Some(String::from("circom_lsp")),
            message,
            related_information: None,
            tags: None,
            data: None
        }, url)
    }

    fn other_files_diagnostic(other_files_diags: &HashMap<Url, Vec<Diagnostic>>) -> Option<Diagnostic> {
        let locations: String = other_files_diags
            .keys()
            .map(|uri| {
                format!("\n{}", helpers::uri_to_string(uri))
            })
            .collect();

        if locations.is_empty() {
            None
        } else {
            Some(Diagnostic {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0
                    },
                    end: Position {
                        line: 0,
                        character: 0
                    },
                },
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some(String::from("circom_lsp")),
                message: format!("errors found in the following files:{}", locations),
                related_information: None,
                tags: None,
                data: None
            })
        }
    }

    fn find_ast_node(start: usize, word: &str, file_id: usize, archive: &ProgramArchive) -> Option<TokenInfo> {
        let mut statements_or_expressions = archive.inner.functions.values()
            .filter_map(|x| {
                if x.get_file_id() == file_id {
                    Some(StatementOrExpression::Statement(x.get_body()))
                } else {
                    None
                }
            }).chain(archive.inner.templates.values()
            .filter_map(|x| {
                if x.get_file_id() == file_id {
                    Some(StatementOrExpression::Statement(x.get_body()))
                } else {
                    None
                }
            }))
            .collect::<Vec<_>>();

        loop {
            let statement_or_expression = statements_or_expressions.pop()?;
            
            match Backend::iterate_statement_or_expression(start, word, statement_or_expression, archive) {
                Ok(token_type) => break Some(TokenInfo {
                    name: word.to_owned(),
                    token_type
                }),
                Err(mut add_to_stack) => {
                    statements_or_expressions.append(&mut add_to_stack);
                }
            }
        }
    }

    fn find_definition_type(word: &str, archive: &ProgramArchive) -> DefintionType {
        let as_template = archive.inner.templates
            .values()
            .any(|x| x.get_name() == word);
        let as_function = archive.inner.functions
            .values()
            .any(|x| x.get_name() == word);

        if as_template {
            DefintionType::Template
        } else if as_function {
            DefintionType::Function
        } else {
            panic!("template_or_function unreachable!")
        }
    }

    fn iterate_statement_or_expression<'a>(
        start: usize, 
        word: &str, 
        statement_or_expression: StatementOrExpression<'a>,
        archive: &ProgramArchive
    ) -> Result<TokenType, Vec<StatementOrExpression<'a>>> {
        let mut statements_or_expressions = Vec::new();
        match statement_or_expression {
            StatementOrExpression::Contender(token_type) => {
                return Ok(token_type)
            },
            StatementOrExpression::Statement(statement) => match statement {
                ast::Statement::IfThenElse { 
                    meta: ast::Meta { start: s_start, end: s_end, .. }, 
                    cond, 
                    if_case, 
                    else_case 
                } => {
                    if *s_start <= start && start <= *s_end {
                        statements_or_expressions.push(StatementOrExpression::Expression(cond));
                        statements_or_expressions.push(StatementOrExpression::Statement(&if_case));
                        if let Some(else_case) = else_case {
                            statements_or_expressions.push(StatementOrExpression::Statement(&else_case));
                        }
                    }
                },
                ast::Statement::While { 
                    meta: ast::Meta { start: s_start, end: s_end, ..}, 
                    cond, 
                    stmt 
                } => {
                    if *s_start <= start && start <= *s_end {
                        statements_or_expressions.push(StatementOrExpression::Expression(cond));
                        statements_or_expressions.push(StatementOrExpression::Statement(&stmt));
                    }
                },
                ast::Statement::Return { 
                    meta: ast::Meta { start: s_start, end: s_end, ..}, 
                    value
                } => {
                    if *s_start <= start && start <= *s_end {
                        statements_or_expressions.push(StatementOrExpression::Expression(value));
                    }
                },
                ast::Statement::InitializationBlock { 
                    meta: ast::Meta { start: s_start, end: s_end, ..}, 
                    initializations,
                    ..
                } => {
                    if *s_start <= start && start <= *s_end {
                        statements_or_expressions.append(&mut initializations.into_iter()
                            .map(|x| StatementOrExpression::Statement(x))
                            .collect()
                        );
                    }
                },
                ast::Statement::Declaration { 
                    meta: ast::Meta { start: s_start, end: s_end, ..}, 
                    xtype,
                    name,
                    ..
                } => {
                    if *s_start <= start && start <= *s_end {
                        if name == word {
                            return Ok(match xtype {
                                ast::VariableType::Var => TokenType::Variable(1),
                                ast::VariableType::Signal(..) => TokenType::Signal,
                                ast::VariableType::Component | ast::VariableType::AnonymousComponent => TokenType::Component
                            });
                        }
                    }
                },
                // TODO: take into account access
                ast::Statement::Substitution { 
                    meta: ast::Meta { start: s_start, end: s_end, ..}, 
                    var,
                    op,
                    rhe,
                    ..
                } => {
                    if *s_start <= start && start <= *s_end {
                        // order matters here
                        if var == word {
                            let token_type = match op {
                                // ast::AssignOp::AssignVar => TokenType::Variable(2),
                                ast::AssignOp::AssignVar => match rhe {
                                    ast::Expression::AnonymousComp { .. } => TokenType::Component,
                                    ast::Expression::Call { id, .. } => match Backend::find_definition_type(id, archive) {
                                        DefintionType::Template => TokenType::Component,
                                        DefintionType::Function => TokenType::Variable(4)
                                    }
                                    _ => TokenType::Variable(2)
                                },
                                ast::AssignOp::AssignSignal | ast::AssignOp::AssignConstraintSignal => TokenType::Signal
                            };
                            statements_or_expressions.push(StatementOrExpression::Contender(token_type));
                        }
                        statements_or_expressions.push(StatementOrExpression::Expression(rhe));
                    }
                },
                ast::Statement::MultSubstitution { 
                    meta: ast::Meta { start: s_start, end: s_end, ..}, 
                    lhe,
                    rhe,
                    ..
                } => {
                    if *s_start <= start && start <= *s_end {
                        statements_or_expressions.push(StatementOrExpression::Expression(lhe));
                        statements_or_expressions.push(StatementOrExpression::Expression(rhe));
                    }
                },
                ast::Statement::UnderscoreSubstitution { 
                    meta: ast::Meta { start: s_start, end: s_end, ..}, 
                    rhe,
                    ..
                } => {
                    if *s_start <= start && start <= *s_end {
                        statements_or_expressions.push(StatementOrExpression::Expression(rhe));
                    }
                },
                ast::Statement::ConstraintEquality { 
                    meta: ast::Meta { start: s_start, end: s_end, ..}, 
                    lhe,
                    rhe,
                    ..
                } => {
                    if *s_start <= start && start <= *s_end {
                        statements_or_expressions.push(StatementOrExpression::Expression(lhe));
                        statements_or_expressions.push(StatementOrExpression::Expression(rhe));
                    }
                },
                ast::Statement::LogCall { 
                    meta: ast::Meta { start: s_start, end: s_end, ..}, 
                    args
                } => {
                    if *s_start <= start && start <= *s_end {
                        let mut args_processed = args.into_iter()
                            .filter_map(|x| {
                                match x {
                                    ast::LogArgument::LogExp(exp) => Some(StatementOrExpression::Expression(exp)),
                                    _ => None
                                }
                            })
                            .collect();

                        statements_or_expressions.append(&mut args_processed);
                    }
                },
                ast::Statement::Block {
                    meta: ast::Meta { start: s_start, end: s_end, .. },
                    stmts
                } => {
                    if *s_start <= start && start <= *s_end {
                        statements_or_expressions.append(&mut stmts.into_iter()
                            .map(|x| StatementOrExpression::Statement(x))
                            .collect()
                        );
                    }
                },
                ast::Statement::Assert { 
                    meta: ast::Meta { start: s_start, end: s_end, .. }, 
                    arg 
                } => {
                    if *s_start <= start && start <= *s_end {
                        statements_or_expressions.push(StatementOrExpression::Expression(arg));
                    }
                }
            },
            StatementOrExpression::Expression(expression) => match expression {
                ast::Expression::InfixOp { 
                    meta: ast::Meta { start: s_start, end: s_end, .. }, 
                    lhe,
                    rhe,
                    ..
                } => {
                    if *s_start <= start && start <= *s_end {
                        statements_or_expressions.push(StatementOrExpression::Expression(lhe));
                        statements_or_expressions.push(StatementOrExpression::Expression(rhe));
                    }
                },
                ast::Expression::PrefixOp { 
                    meta: ast::Meta { start: s_start, end: s_end, .. }, 
                    rhe,
                    ..
                } => {
                    if *s_start <= start && start <= *s_end {
                        statements_or_expressions.push(StatementOrExpression::Expression(rhe));
                    }
                },
                ast::Expression::InlineSwitchOp { 
                    meta: ast::Meta { start: s_start, end: s_end, .. }, 
                    cond,
                    if_true,
                    if_false
                } => {
                    if *s_start <= start && start <= *s_end {
                        statements_or_expressions.push(StatementOrExpression::Expression(cond));
                        statements_or_expressions.push(StatementOrExpression::Expression(if_true));
                        statements_or_expressions.push(StatementOrExpression::Expression(if_false));
                    }
                },
                ast::Expression::ParallelOp { 
                    meta: ast::Meta { start: s_start, end: s_end, .. }, 
                    rhe
                } => {
                    if *s_start <= start && start <= *s_end {
                        statements_or_expressions.push(StatementOrExpression::Expression(rhe));
                    }
                },
                // TODO: Support access
                ast::Expression::Variable { 
                    meta,
                    name,
                    ..
                } => {
                    let ast::Meta { start: s_start, end: s_end, .. } = meta;
                    if *s_start <= start && start <= *s_end {
                        // TODO: get_reduces_to panics by default, make it so it can print custom error
                        // message depending on where reduction failed
                        let type_reduction = meta.get_type_knowledge().get_reduces_to();
                        if word == name {
                            return Ok(match type_reduction {
                                ast::TypeReduction::Variable => TokenType::Variable(3),
                                ast::TypeReduction::Component => TokenType::Component,
                                ast::TypeReduction::Signal => TokenType::Signal,
                                ast::TypeReduction::Tag => TokenType::Tag
                            });
                        }
                    }
                },
                ast::Expression::Call { 
                    meta: ast::Meta { start: s_start, end: s_end, .. }, 
                    id,
                    args
                } => {
                    if *s_start <= start && start <= *s_end {
                        // order matters here
                        if id == word {
                            statements_or_expressions.push(
                                StatementOrExpression::Contender(
                                    TokenType::Defintion(Backend::find_definition_type(word, archive))
                                )
                            );
                        }

                        let mut args_processed = args.into_iter()
                            .map(|x| StatementOrExpression::Expression(x))
                            .collect();

                        statements_or_expressions.append(&mut args_processed);
                    }
                },
                ast::Expression::AnonymousComp { 
                    meta: ast::Meta { start: s_start, end: s_end, .. }, 
                    id,
                    params,
                    signals,
                    ..
                } => {
                    if *s_start <= start && start <= *s_end {
                        // order matters here
                        if id == word {
                            statements_or_expressions.push(
                                StatementOrExpression::Contender(
                                    TokenType::Defintion(Backend::find_definition_type(word, archive))
                                )
                            );
                        }

                        let mut params_processed = params.into_iter()
                            .map(|x| StatementOrExpression::Expression(x))
                            .collect();
                        let mut signals_processed = signals.into_iter()
                            .map(|x| StatementOrExpression::Expression(x))
                            .collect();

                        statements_or_expressions.append(&mut params_processed);
                        statements_or_expressions.append(&mut signals_processed);
                    }
                },
                ast::Expression::ArrayInLine { 
                    meta: ast::Meta { start: s_start, end: s_end, .. }, 
                    values
                } => {
                    if *s_start <= start && start <= *s_end {
                        let mut values_processed = values.into_iter()
                            .map(|x| StatementOrExpression::Expression(x))
                            .collect();

                        statements_or_expressions.append(&mut values_processed);
                    }
                },
                ast::Expression::Tuple { 
                    meta: ast::Meta { start: s_start, end: s_end, .. }, 
                    values
                } => {
                    if *s_start <= start && start <= *s_end {
                        let mut values_processed = values.into_iter()
                            .map(|x| StatementOrExpression::Expression(x))
                            .collect();

                        statements_or_expressions.append(&mut values_processed);
                    }
                },
                ast::Expression::UniformArray { 
                    meta: ast::Meta { start: s_start, end: s_end, .. }, 
                    value,
                    dimension
                } => {
                    if *s_start <= start && start <= *s_end {
                        statements_or_expressions.push(StatementOrExpression::Expression(dimension));
                        statements_or_expressions.push(StatementOrExpression::Expression(value));
                    }
                },
                _ => ()
            }
        }

        return Err(statements_or_expressions);
    }

    fn read_comment(content: &Rope, start: usize) -> Option<String> {
        Self::read_multi_singleline_comment(content, start)
            .or(Self::read_multiline_comment(content, start))
    }

    // start is the char index where the keyword starts.
    // example: if comment is produced for a function 'Main', the index would be the index of the
    // character 'M'
    fn read_multiline_comment(content: &Rope, start: usize) -> Option<String> {
        let mut current_idx = start;
        let mut current_state = CommentParserState::Outside;
        let mut start_idx = 0;
        let mut end_idx = 0;

        let mut iter = content.chars_at(start).reversed();
        while let Some(c) = iter.next() {
            match current_state {
                CommentParserState::Outside => match c {
                    '/' => current_state = CommentParserState::MaybeInside,
                    _ => ()
                },
                CommentParserState::MaybeInside => match c {
                    '*' => {
                        current_state = CommentParserState::JustEntered;
                        end_idx = current_idx;
                    },
                    '/' => (),
                    _ => current_state = CommentParserState::Outside
                },
                CommentParserState::JustEntered => match c {
                    '*' => end_idx = current_idx,
                    _ => current_state = CommentParserState::Inside
                }
                CommentParserState::Inside => match c {
                    '*' => {
                        current_state = CommentParserState::MaybeOutside;
                        start_idx = current_idx;
                    }
                    _ => ()
                },
                CommentParserState::MaybeOutside => match c {
                    '/' => {
                        let result = content
                            .slice(start_idx..end_idx-1)
                            .to_string()
                            .lines()
                            .map(|x| x.trim_matches(|c: char| c.is_whitespace() || c == '*'))
                            .intersperse("\n")
                            .collect::<String>()
                            .trim()
                            .to_string();

                        return if !result.is_empty() {
                            Some(result)
                        } else {
                            None
                        }
                    },
                    '*' => (),
                    _ => current_state = CommentParserState::Inside
                }
            }

            current_idx -= 1;
        }

        None
    }

    fn read_multi_singleline_comment(content: &Rope, start: usize) -> Option<String> {
        let mut current_line_idx = content.try_char_to_line(start).expect("char start index should be valid");
        let mut first_comment_line = 0;
        let mut last_comment_line = 0;
        let mut entered = false;

        while current_line_idx > 0 {
            current_line_idx -= 1;

            let line = content.line(current_line_idx);
            // not converting to &str because it might fail (due to the structue of rope), 
            // and allocating for each line is wasteful
            let is_line_comment = {
                if line.len_chars() < 2 {
                    false 
                } else {
                    line.char(0) == '/' && line.char(1) == '/'
                }
            };

            match (entered, is_line_comment) {
                (false, true) => {
                    entered = true;
                    last_comment_line = current_line_idx;
                },
                (true, false) => {
                    first_comment_line = current_line_idx;
                    break;
                },
                _ => ()
            }
        }

        if entered {
            let start = content.line_to_char(first_comment_line);
            let end = content.line_to_char(last_comment_line + 1);
            let result = content
                .slice(start..end)
                .to_string()
                .lines()
                .map(|x| x.trim_matches(|c: char| c.is_whitespace() || c == '/'))
                .intersperse("\n")
                .collect::<String>()
                .trim()
                .to_string();

            Some(result)
        } else {
            None
        }
    }

    fn definition_summary(defintion_data: DefinitionData, file_library: &FileLibrary) -> String {
        let (file_id, definition_name, type_as_name) = match defintion_data {
            DefinitionData::Template(data) => {
                (data.get_file_id(), data.get_name(), "template")
            },
            DefinitionData::Function(data) => {
                (data.get_file_id(), data.get_name(), "function")
            },
        };
        let file_name = file_library.to_storage().get(file_id).expect("function data should have valid file id").name();

        format!("{} \"{}\" found in {}", type_as_name, definition_name, file_name)
    }

    fn definition_start<'a>(defintion_data: DefinitionData, file_library: &'a FileLibrary) -> (&'a SimpleFile<String, String>, usize) {
        let (file_id, start) = match defintion_data {
            DefinitionData::Template(data) => {
                (data.get_file_id(), data.get_param_location().start)
            },
            DefinitionData::Function(data) => {
                (data.get_file_id(), data.get_param_location().start)
            },
        };

        (file_library.to_storage().get(file_id).expect("file_id of definition should be valid"), start)
    }

    fn find_definition<'a>(name: &str, archive: &'a ProgramArchive) -> Option<DefinitionData<'a>> {
        if let Some(template_data) = archive.inner.templates.get(name) {
            Some(DefinitionData::Template(template_data))
        } else if let Some(function_data) = archive.inner.functions.get(name) {
            Some(DefinitionData::Function(function_data))
        } else {
            None
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "Opened!")
            .await;

        self.on_change(
            TextDocumentItem { 
                uri: params.text_document.uri,
                text: params.text_document.text,
                version: Some(params.text_document.version)
            },
            true
        ).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(
            TextDocumentItem {
                uri: params.text_document.uri,
                text: params.content_changes[0].text.clone(),
                version: Some(params.text_document.version)
            },
            false
        ).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let result = {
            let document_map = self.document_map.lock().expect("document_map mutex poisened");
            let document_map = document_map.borrow();
            document_map.get(&params.text_document.uri).map(|x| x.content.to_string())
        };

        match result {
            Some(document) => {
                self.on_change(
                    TextDocumentItem {
                        uri: params.text_document.uri,
                        text: document,
                        version: None
                    },
                    true
                ).await;
            },
            None => ()
        };
    }

    async fn hover(&self, params: HoverParams) -> jsonrpc::Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let document_map = self.document_map.lock().expect("document_map mutex poisened");
        let document_map = document_map.borrow();
        let document_data = document_map.get(&uri).expect("document map should have uri on hover");

        // find what word is selected
        let Ok(Some((pos, word))) = helpers::find_word(&document_data.content, params.text_document_position_params.position) else {
            return Ok(None);
        };

        let Some(archive) = &document_data.archive else {
            return Ok(Some(helpers::simple_hover(String::from("Could not find information (are there any compilation errors?)"))))
        };
        // let file_library = &archive.inner.file_library;

        // TODO: A better way to relate between URI and file_id
        if let Some(token_info) = Backend::find_ast_node(pos, &word, archive.inner.file_id_main, archive) {
            return Ok(Some(helpers::simple_hover(token_info.description())));
        }

        Ok(None)
        /*
        let Some(defintion_data) = Backend::find_definition(&word, &archive) else {
            return Ok(None);
        };

        let (file, start) = Backend::definition_start(defintion_data, file_library);
        let source = file.source();
        let rope = Rope::from_str(source);

        Ok(Backend::read_comment(&rope, start)
            .or_else(|| Some(Backend::definition_summary(defintion_data, file_library)))
            .map(|x| helpers::simple_hover(x)))
        */
    }

    async fn goto_definition(&self, params: GotoDefinitionParams) -> jsonrpc::Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let document_map = self.document_map.lock().expect("document_map mutex poisened");
        let document_map = document_map.borrow();
        let document_data = document_map.get(&uri).expect("document map should have uri on hover");

        // find what word is selected
        let Ok(Some((_, word))) = helpers::find_word(&document_data.content, params.text_document_position_params.position) else {
            return Ok(None);
        };

        let Some(archive) = &document_data.archive else {
            return Ok(None)
        };
        let file_library = &archive.inner.file_library;

        let Some(defintion_data) = Backend::find_definition(&word, &archive) else {
            return Ok(None);
        };

        let (file, start) = Backend::definition_start(defintion_data, file_library);
        let source = file.source();
        let definition_uri = helpers::string_to_uri(file.name()); 
        let rope = Rope::from_str(source);

        // currently, the only way to get the position of a defintion in circom
        // is to get the param position from TemplateData / FunctionData
        //
        // this means that start points at the start of a function/template params, not the start
        // of the function name - which makes the selection of the function name more tedious
        //
        // for now, simply disregard since using the start also as the end is safe and still
        // provides jumping capabilities
        let end = start /* + word.len() */;

        Ok(
            Some(
                GotoDefinitionResponse::Scalar(
                    Location {
                        uri: definition_uri,
                        range: Range {
                            start: helpers::char_to_position(&rope, start).expect("word start should be valid"),
                            end: helpers::char_to_position(&rope, end).expect("word end should be valid")
                        }
                    }
                )
            )
        )
    }
}
