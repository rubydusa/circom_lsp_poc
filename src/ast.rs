use ropey::Rope;
use tower_lsp::lsp_types;

use circom_structure::abstract_syntax_tree::ast;
use circom_structure::file_definition::FileLibrary;
use circom_structure::function_data::FunctionData;
use circom_structure::template_data::TemplateData;
use codespan_reporting::files::SimpleFile;

use num_traits::cast::ToPrimitive;

use std::fmt;

use crate::parse;
use crate::wrappers::*;

pub enum Contender<'a> {
    StatementOrExpression(StatementOrExpression<'a>),
    Contender(TokenInfo),
}

#[derive(Clone, Copy)]
pub enum StatementOrExpression<'a> {
    Statement(&'a ast::Statement),
    Expression(&'a ast::Expression),
}

#[derive(Debug)]
pub enum TokenType {
    Variable(Access),
    Signal(Access),
    Component(Access),
    Defintion(DefinitionType),
}

#[derive(Debug)]
pub enum DefinitionType {
    Template,
    Function,
}

#[derive(Debug)]
pub struct Access(Vec<Option<AccessType>>);

#[derive(Debug)]
pub enum AccessType {
    Num(u32),
    Var(String),
}

#[derive(Clone, Copy)]
pub enum DefinitionData<'a> {
    Template(&'a TemplateData),
    Function(&'a FunctionData),
}

pub struct Scope<'a> {
    body: StatementOrExpression<'a>,
    params: &'a Vec<String>,
    params_location: std::ops::Range<usize>,
}

impl<'a> Scope<'a> {
    fn new(defintion_data: DefinitionData<'a>) -> Scope<'a> {
        let (body, params, params_location) = match defintion_data {
            DefinitionData::Template(x) => (
                StatementOrExpression::Statement(x.get_body()),
                x.get_name_of_params(),
                x.get_param_location(),
            ),
            DefinitionData::Function(x) => (
                StatementOrExpression::Statement(x.get_body()),
                x.get_name_of_params(),
                x.get_param_location(),
            ),
        };

        Scope {
            body,
            params,
            params_location,
        }
    }
}

pub struct TokenInfo {
    name: String,
    token_type: TokenType,
    location: lsp_types::Range,
    declaration_location: lsp_types::Range,
    docs: Option<String>,
}

impl TokenInfo {
    pub fn new(
        name: String,
        scope: &Scope,
        meta: &ast::Meta,
        archive: &ProgramArchive,
        document: &Rope,
    ) -> TokenInfo {
        let location = parse::char_range_to_position_range(document, meta.start..meta.end)
            .expect("unmatching document");
        let docs = get_docs(&name, archive);
        let (token_type, declaration_location) =
            find_declaration(&name, scope, archive).expect(&format!("token should exist in scope: {}", name));
        let declaration_location =
            parse::char_range_to_position_range(document, declaration_location)
                .expect("unmatching document");

        TokenInfo {
            name,
            token_type,
            declaration_location,
            location,
            docs,
        }
    }

    pub fn try_new_defintion(
        name: String,
        archive: &ProgramArchive,
        document: &Rope,
        start: usize,
    ) -> Option<TokenInfo> {
        find_definition_type(&name, archive).map(|token_type| {
            let location =
                parse::char_range_to_position_range(document, start..(start + name.len()))
                    .expect("unmatching document");
            let docs = get_docs(&name, archive);

            TokenInfo {
                name,
                token_type: TokenType::Defintion(token_type),
                declaration_location: location,
                location,
                docs,
            }
        })
    }

    pub fn to_hover(&self) -> lsp_types::Hover {
        let range = Some(self.location.clone());
        let contents =
            lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(format!("{}", &self)));

        lsp_types::Hover { range, contents }
    }
}

pub fn find_token(
    start: usize,
    word: &str,
    file_id: usize,
    archive: &ProgramArchive,
) -> Option<TokenInfo> {
    let document = Rope::from_str(
        archive
            .inner
            .file_library
            .to_storage()
            .get(file_id)
            .expect("invalid file_id in find_token")
            .source(),
    );

    let defintions = archive
        .inner
        .functions
        .values()
        .filter_map(|x| {
            if x.get_file_id() == file_id {
                Some(Scope::new(DefinitionData::Function(x)))
            } else {
                None
            }
        })
        .chain(
            archive
                .inner
                .templates
                .values()
                .filter_map(|x| {
                    if x.get_file_id() == file_id {
                        Some(Scope::new(DefinitionData::Template(x)))
                    } else {
                        None
                    }
                }),
        )
        .collect::<Vec<_>>();

    /*
    defintions.push(StatementOrExpression::Expression(
        &archive.inner.initial_template_call,
    ));
    */

    let mut result = None;
    'main: for definition in defintions {
        let mut contenders = vec![Contender::StatementOrExpression(definition.body)];
        'inner: loop {
            let Some(statement_or_expression) = contenders.pop() else {
                break 'inner;
            };

            match iterate_contender(
                start,
                word,
                statement_or_expression,
                &definition,
                archive,
                &document,
            ) {
                Ok(token_info) => {
                    result = Some(token_info);
                    break 'main;
                }
                Err(mut add_to_stack) => {
                    contenders.append(&mut add_to_stack);
                }
            }
        }
    }

    result.or_else(|| TokenInfo::try_new_defintion(word.to_owned(), archive, &document, start))
}

fn iterate_contender<'a>(
    start: usize,
    word: &str,
    contender: Contender<'a>,
    scope: &Scope,
    archive: &ProgramArchive,
    document: &Rope,
) -> Result<TokenInfo, Vec<Contender<'a>>> {
    match contender {
        Contender::Contender(token_info) => Ok(token_info),
        Contender::StatementOrExpression(statement_or_expression) => {
            let mut contenders = Vec::new();
            let meta = get_meta(statement_or_expression);
            if !(meta.start <= start && start <= meta.end) {
                return Err(vec![]);
            }

            let is_match = match statement_or_expression {
                StatementOrExpression::Statement(statement) => match statement {
                    ast::Statement::Declaration { name, .. } => name == word,
                    ast::Statement::Substitution { var, .. } => {
                        // order matters here
                        if var == word {
                            contenders.push(Contender::Contender(TokenInfo::new(
                                word.to_owned(),
                                scope,
                                meta,
                                archive,
                                document,
                            )));
                        }
                        false
                    }
                    _ => false,
                },
                StatementOrExpression::Expression(expression) => match expression {
                    ast::Expression::Variable { name, .. } => {
                        // TODO (maybe): exception for template and function params?
                        word == name
                    }
                    ast::Expression::Call { id, .. } => {
                        // order matters here
                        if id == word {
                            contenders.push(Contender::Contender(TokenInfo::new(
                                word.to_owned(),
                                scope,
                                meta,
                                archive,
                                document,
                            )));
                        }
                        false
                    }
                    ast::Expression::AnonymousComp { id, .. } => {
                        // order matters here
                        if id == word {
                            contenders.push(Contender::Contender(TokenInfo::new(
                                word.to_owned(),
                                scope,
                                meta,
                                archive,
                                document,
                            )));
                        }
                        false
                    }
                    _ => false,
                },
            };

            contenders.append(
                &mut get_next_statements_or_expression(statement_or_expression)
                    .into_iter()
                    .map(Contender::StatementOrExpression)
                    .collect(),
            );

            if is_match {
                Ok(TokenInfo::new(
                    word.to_owned(),
                    scope,
                    meta,
                    archive,
                    document,
                ))
            } else {
                Err(contenders)
            }
        }
    }
}

fn get_docs(name: &str, archive: &ProgramArchive) -> Option<String> {
    find_definition(name, archive)
        .map(|x| {
            let (file, start) = definition_location(x, &archive.inner.file_library);
            let content = Rope::from_str(file.source());

            parse::read_comment(&content, start)
        })
        .flatten()
}

fn find_definition_type(name: &str, archive: &ProgramArchive) -> Option<DefinitionType> {
    find_definition(name, archive).map(|x| match x {
        DefinitionData::Template(_) => DefinitionType::Template,
        DefinitionData::Function(_) => DefinitionType::Function,
    })
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

fn definition_location<'a>(
    defintion_data: DefinitionData,
    file_library: &'a FileLibrary,
) -> (&'a SimpleFile<String, String>, usize) {
    let (file_id, start) = match defintion_data {
        DefinitionData::Template(data) => (data.get_file_id(), data.get_param_location().start),
        DefinitionData::Function(data) => (data.get_file_id(), data.get_param_location().start),
    };

    (
        file_library
            .to_storage()
            .get(file_id)
            .expect("file_id of definition should be valid"),
        start,
    )
}

fn find_declaration(symbol: &str, scope: &Scope, archive: &ProgramArchive) -> Option<(TokenType, std::ops::Range<usize>)> {
    let mut statements_or_expressions = vec![scope.body];
    let result = loop {
        let Some(statement_or_expression) = statements_or_expressions.pop() else {
            break None
        };

        let result = match statement_or_expression {
            StatementOrExpression::Statement(x) => match x {
                ast::Statement::Declaration {
                    meta,
                    xtype,
                    name,
                    dimensions,
                    ..
                } => {
                    if symbol != name {
                        continue;
                    }
                    let access = Access(
                        dimensions
                            .into_iter()
                            .map(|e| match e {
                                ast::Expression::Number(_, big_int) => Some(AccessType::Num(
                                    big_int
                                        .to_u32()
                                        .expect("signal array length shouldnt be big"),
                                )),
                                ast::Expression::Variable { name, .. } => {
                                    Some(AccessType::Var(name.to_owned()))
                                }
                                _ => None,
                            })
                            .collect(),
                    );

                    let range = meta.start..meta.end;

                    Some(match xtype {
                        ast::VariableType::Var => (TokenType::Variable(access), range),
                        ast::VariableType::Signal(..) => (TokenType::Signal(access), range),
                        ast::VariableType::Component => (TokenType::Component(access), range),
                        ast::VariableType::AnonymousComponent => {
                            (TokenType::Component(access), range)
                        }
                    })
                }
                _ => None,
            },
            _ => None,
        };

        if result.is_some() {
            break result;
        } else {
            statements_or_expressions.append(&mut get_next_statements_or_expression(
                statement_or_expression,
            ))
        }
    };

    result.or_else(|| {
        if scope.params.contains(&symbol.to_string()) {
            Some((
                TokenType::Variable(Access(vec![])),
                scope.params_location.clone(),
            ))
        } else {
            None
        }
    }).or_else(|| {
        find_definition_type(symbol, archive).map(|x| {
            (TokenType::Defintion(x), scope.params_location.clone())
        })
    })
}

fn get_next_statements_or_expression(
    statement_or_expression: StatementOrExpression,
) -> Vec<StatementOrExpression> {
    let mut statements_or_expressions = Vec::new();

    match statement_or_expression {
        StatementOrExpression::Statement(statement) => match statement {
            ast::Statement::IfThenElse {
                cond,
                if_case,
                else_case,
                ..
            } => {
                statements_or_expressions.push(StatementOrExpression::Expression(cond));
                statements_or_expressions.push(StatementOrExpression::Statement(&if_case));
                if let Some(else_case) = else_case {
                    statements_or_expressions.push(StatementOrExpression::Statement(&else_case));
                }
            }
            ast::Statement::While { cond, stmt, .. } => {
                statements_or_expressions.push(StatementOrExpression::Expression(cond));
                statements_or_expressions.push(StatementOrExpression::Statement(&stmt));
            }
            ast::Statement::Return { value, .. } => {
                statements_or_expressions.push(StatementOrExpression::Expression(value));
            }
            ast::Statement::InitializationBlock {
                initializations, ..
            } => {
                statements_or_expressions.append(
                    &mut initializations
                        .into_iter()
                        .map(|x| StatementOrExpression::Statement(x))
                        .collect(),
                );
            }
            ast::Statement::Declaration { dimensions, .. } => {
                statements_or_expressions.append(
                    &mut dimensions
                        .into_iter()
                        .map(|x| StatementOrExpression::Expression(x))
                        .collect(),
                );
            }
            ast::Statement::Substitution { rhe, access, .. } => {
                statements_or_expressions.append(
                    &mut access
                        .into_iter()
                        .filter_map(|x| match x {
                            ast::Access::ArrayAccess(e) => {
                                Some(StatementOrExpression::Expression(e))
                            }
                            _ => None,
                        })
                        .collect(),
                );
                statements_or_expressions.push(StatementOrExpression::Expression(rhe));
            }
            ast::Statement::MultSubstitution { lhe, rhe, .. } => {
                statements_or_expressions.push(StatementOrExpression::Expression(lhe));
                statements_or_expressions.push(StatementOrExpression::Expression(rhe));
            }
            ast::Statement::UnderscoreSubstitution { rhe, .. } => {
                statements_or_expressions.push(StatementOrExpression::Expression(rhe));
            }
            ast::Statement::ConstraintEquality { lhe, rhe, .. } => {
                statements_or_expressions.push(StatementOrExpression::Expression(lhe));
                statements_or_expressions.push(StatementOrExpression::Expression(rhe));
            }
            ast::Statement::LogCall { args, .. } => {
                let mut args_processed = args
                    .into_iter()
                    .filter_map(|x| match x {
                        ast::LogArgument::LogExp(exp) => {
                            Some(StatementOrExpression::Expression(exp))
                        }
                        _ => None,
                    })
                    .collect();

                statements_or_expressions.append(&mut args_processed);
            }
            ast::Statement::Block { stmts, .. } => {
                statements_or_expressions.append(
                    &mut stmts
                        .into_iter()
                        .map(|x| StatementOrExpression::Statement(x))
                        .collect(),
                );
            }
            ast::Statement::Assert { arg, .. } => {
                statements_or_expressions.push(StatementOrExpression::Expression(arg));
            }
        },
        StatementOrExpression::Expression(expression) => match expression {
            ast::Expression::InfixOp { lhe, rhe, .. } => {
                statements_or_expressions.push(StatementOrExpression::Expression(lhe));
                statements_or_expressions.push(StatementOrExpression::Expression(rhe));
            }
            ast::Expression::PrefixOp { rhe, .. } => {
                statements_or_expressions.push(StatementOrExpression::Expression(rhe));
            }
            ast::Expression::InlineSwitchOp {
                cond,
                if_true,
                if_false,
                ..
            } => {
                statements_or_expressions.push(StatementOrExpression::Expression(cond));
                statements_or_expressions.push(StatementOrExpression::Expression(if_true));
                statements_or_expressions.push(StatementOrExpression::Expression(if_false));
            }
            ast::Expression::ParallelOp { rhe, .. } => {
                statements_or_expressions.push(StatementOrExpression::Expression(rhe));
            }
            ast::Expression::Variable { access, .. } => {
                statements_or_expressions.append(
                    &mut access
                        .into_iter()
                        .filter_map(|x| match x {
                            ast::Access::ArrayAccess(e) => {
                                Some(StatementOrExpression::Expression(e))
                            }
                            _ => None,
                        })
                        .collect(),
                );
            }
            ast::Expression::Call { args, .. } => {
                let mut args_processed = args
                    .into_iter()
                    .map(|x| StatementOrExpression::Expression(x))
                    .collect();

                statements_or_expressions.append(&mut args_processed);
            }
            ast::Expression::AnonymousComp {
                params, signals, ..
            } => {
                let mut params_processed = params
                    .into_iter()
                    .map(|x| StatementOrExpression::Expression(x))
                    .collect();
                let mut signals_processed = signals
                    .into_iter()
                    .map(|x| StatementOrExpression::Expression(x))
                    .collect();

                statements_or_expressions.append(&mut params_processed);
                statements_or_expressions.append(&mut signals_processed);
            }
            ast::Expression::ArrayInLine { values, .. } => {
                let mut values_processed = values
                    .into_iter()
                    .map(|x| StatementOrExpression::Expression(x))
                    .collect();

                statements_or_expressions.append(&mut values_processed);
            }
            ast::Expression::Tuple { values, .. } => {
                let mut values_processed = values
                    .into_iter()
                    .map(|x| StatementOrExpression::Expression(x))
                    .collect();

                statements_or_expressions.append(&mut values_processed);
            }
            ast::Expression::UniformArray {
                value, dimension, ..
            } => {
                statements_or_expressions.push(StatementOrExpression::Expression(dimension));
                statements_or_expressions.push(StatementOrExpression::Expression(value));
            }
            _ => (),
        },
    };

    statements_or_expressions
}

fn get_meta<'a>(statement_or_expression: StatementOrExpression<'a>) -> &'a ast::Meta {
    match statement_or_expression {
        StatementOrExpression::Statement(x) => match x {
            ast::Statement::IfThenElse { meta, .. } => meta,
            ast::Statement::While { meta, .. } => meta,
            ast::Statement::Return { meta, .. } => meta,
            ast::Statement::InitializationBlock { meta, .. } => meta,
            ast::Statement::Declaration { meta, .. } => meta,
            ast::Statement::Substitution { meta, .. } => meta,
            ast::Statement::MultSubstitution { meta, .. } => meta,
            ast::Statement::UnderscoreSubstitution { meta, .. } => meta,
            ast::Statement::ConstraintEquality { meta, .. } => meta,
            ast::Statement::LogCall { meta, .. } => meta,
            ast::Statement::Block { meta, .. } => meta,
            ast::Statement::Assert { meta, .. } => meta,
        },
        StatementOrExpression::Expression(x) => match x {
            ast::Expression::InfixOp { meta, .. } => meta,
            ast::Expression::PrefixOp { meta, .. } => meta,
            ast::Expression::InlineSwitchOp { meta, .. } => meta,
            ast::Expression::ParallelOp { meta, .. } => meta,
            ast::Expression::Variable { meta, .. } => meta,
            ast::Expression::Number(meta, _) => meta,
            ast::Expression::Call { meta, .. } => meta,
            ast::Expression::AnonymousComp { meta, .. } => meta,
            ast::Expression::ArrayInLine { meta, .. } => meta,
            ast::Expression::Tuple { meta, .. } => meta,
            ast::Expression::UniformArray { meta, .. } => meta,
        },
    }
}

impl fmt::Display for TokenInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(docs) = &self.docs {
            write!(f, "{}: {}\n---\n{}", self.name, self.token_type, docs)
        } else {
            write!(f, "{}: {}", self.name, self.token_type)
        }
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenType::Variable(access) => write!(f, "Variable{}", access),
            TokenType::Signal(access) => write!(f, "Signal{}", access),
            TokenType::Component(access) => write!(f, "Component{}", access),
            TokenType::Defintion(defintion_type) => write!(f, "{}", defintion_type),
        }
    }
}

impl fmt::Display for DefinitionType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DefinitionType::Template => write!(f, "Template"),
            DefinitionType::Function => write!(f, "Function"),
        }
    }
}

impl fmt::Display for Access {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0.is_empty() {
            write!(f, "")
        } else {
            let access = self
                .0
                .iter()
                .map(|x| match x {
                    Some(x) => format!(r"\[{}\]", x),
                    None => r"\[\_\]".to_string(),
                })
                .collect::<String>();

            write!(f, "{}", access)
        }
    }
}

impl fmt::Display for AccessType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AccessType::Num(x) => write!(f, "{}", x),
            AccessType::Var(x) => write!(f, "{}", x),
        }
    }
}
