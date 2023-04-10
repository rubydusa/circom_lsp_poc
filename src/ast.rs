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
    Tag,
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

pub struct TokenInfo {
    name: String,
    token_type: TokenType,
    location: lsp_types::Range,
    docs: Option<String>,
}

impl TokenInfo {
    pub fn new(
        name: String,
        token_type: TokenType,
        meta: &ast::Meta,
        archive: &ProgramArchive,
        document: &Rope,
    ) -> TokenInfo {
        let location = parse::char_range_to_position_range(document, meta.start..meta.end)
            .expect("unmatching document");
        let docs = get_docs(&name, archive);

        TokenInfo {
            name,
            token_type,
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
                location,
                docs,
            }
        })
    }

    pub fn to_hover(&self) -> lsp_types::Hover {
        let range = Some(self.location.clone());
        let contents = lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value: format!("{}", &self),
        });

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

    let mut statements_or_expressions = archive
        .inner
        .functions
        .values()
        .filter_map(|x| {
            if x.get_file_id() == file_id {
                Some(Contender::StatementOrExpression(
                    StatementOrExpression::Statement(x.get_body()),
                ))
            } else {
                None
            }
        })
        .chain(archive.inner.templates.values().filter_map(|x| {
            if x.get_file_id() == file_id {
                Some(Contender::StatementOrExpression(
                    StatementOrExpression::Statement(x.get_body()),
                ))
            } else {
                None
            }
        }))
        .collect::<Vec<_>>();

    statements_or_expressions.push(Contender::StatementOrExpression(
        StatementOrExpression::Expression(&archive.inner.initial_template_call),
    ));

    let result = loop {
        let Some(statement_or_expression) = statements_or_expressions.pop() else {
            break None
        };

        match iterate_contender(start, word, statement_or_expression, archive, &document) {
            Ok(token_info) => {
                break Some(token_info);
            }
            Err(mut add_to_stack) => {
                statements_or_expressions.append(&mut add_to_stack);
            }
        }
    };

    result.or_else(|| TokenInfo::try_new_defintion(word.to_owned(), archive, &document, start))
}

fn iterate_contender<'a>(
    start: usize,
    word: &str,
    contender: Contender<'a>,
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

            let token_type = match statement_or_expression {
                StatementOrExpression::Statement(statement) => match statement {
                    ast::Statement::IfThenElse {
                        cond,
                        if_case,
                        else_case,
                        ..
                    } => {
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(cond),
                        ));
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Statement(&if_case),
                        ));
                        if let Some(else_case) = else_case {
                            contenders.push(Contender::StatementOrExpression(
                                StatementOrExpression::Statement(&else_case),
                            ));
                        }
                        None
                    }
                    ast::Statement::While { cond, stmt, .. } => {
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(cond),
                        ));
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Statement(&stmt),
                        ));
                        None
                    }
                    ast::Statement::Return { value, .. } => {
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(value),
                        ));
                        None
                    }
                    ast::Statement::InitializationBlock {
                        initializations, ..
                    } => {
                        contenders.append(
                            &mut initializations
                                .into_iter()
                                .map(|x| {
                                    Contender::StatementOrExpression(
                                        StatementOrExpression::Statement(x),
                                    )
                                })
                                .collect(),
                        );
                        None
                    }
                    ast::Statement::Declaration {
                        xtype,
                        name,
                        dimensions,
                        ..
                    } => {
                        if name == word {
                            let access = Access(
                                dimensions
                                    .into_iter()
                                    .map(|x| match x {
                                        ast::Expression::Number(_, big_int) => {
                                            Some(AccessType::Num(
                                                big_int
                                                    .to_u32()
                                                    .expect("signal array length shouldnt be big"),
                                            ))
                                        }
                                        ast::Expression::Variable { name, .. } => {
                                            Some(AccessType::Var(name.to_owned()))
                                        }
                                        _ => None,
                                    })
                                    .collect(),
                            );

                            Some(match xtype {
                                ast::VariableType::Var => TokenType::Variable(access),
                                ast::VariableType::Signal(..) => TokenType::Signal(access),
                                ast::VariableType::Component
                                | ast::VariableType::AnonymousComponent => {
                                    TokenType::Component(access)
                                }
                            })
                        } else {
                            None
                        }
                    }
                    ast::Statement::Substitution {
                        var,
                        op,
                        rhe,
                        access,
                        ..
                    } => {
                        // order matters here
                        if var == word {
                            let access =
                                Access(
                                    access
                                        .into_iter()
                                        .take_while(|x| match x {
                                            ast::Access::ArrayAccess(_) => true,
                                            _ => false,
                                        })
                                        .map(|x| match x {
                                            ast::Access::ArrayAccess(e) => match e {
                                                ast::Expression::Number(_, big_int) => {
                                                    Some(AccessType::Num(big_int.to_u32().expect(
                                                        "signal array length shouldnt be big",
                                                    )))
                                                }
                                                ast::Expression::Variable { name, .. } => {
                                                    Some(AccessType::Var(name.to_owned()))
                                                }
                                                _ => None,
                                            },
                                            _ => unreachable!(),
                                        })
                                        .collect(),
                                );
                            let token_type = match op {
                                ast::AssignOp::AssignVar => match rhe {
                                    ast::Expression::AnonymousComp { .. } => {
                                        TokenType::Component(access)
                                    }
                                    ast::Expression::Call { id, .. } => {
                                        match find_definition_type(id, archive)
                                            .expect("call should have valid defintion")
                                        {
                                            DefinitionType::Template => {
                                                TokenType::Component(access)
                                            }
                                            DefinitionType::Function => TokenType::Variable(access),
                                        }
                                    }
                                    _ => TokenType::Variable(access),
                                },
                                ast::AssignOp::AssignSignal
                                | ast::AssignOp::AssignConstraintSignal => {
                                    TokenType::Signal(access)
                                }
                            };
                            contenders.push(Contender::Contender(TokenInfo::new(
                                word.to_owned(),
                                token_type,
                                meta,
                                archive,
                                document,
                            )));
                        }
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(rhe),
                        ));
                        None
                    }
                    ast::Statement::MultSubstitution { lhe, rhe, .. } => {
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(lhe),
                        ));
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(rhe),
                        ));
                        None
                    }
                    ast::Statement::UnderscoreSubstitution { rhe, .. } => {
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(rhe),
                        ));
                        None
                    }
                    ast::Statement::ConstraintEquality { lhe, rhe, .. } => {
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(lhe),
                        ));
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(rhe),
                        ));
                        None
                    }
                    ast::Statement::LogCall { args, .. } => {
                        let mut args_processed = args
                            .into_iter()
                            .filter_map(|x| match x {
                                ast::LogArgument::LogExp(exp) => {
                                    Some(Contender::StatementOrExpression(
                                        StatementOrExpression::Expression(exp),
                                    ))
                                }
                                _ => None,
                            })
                            .collect();

                        contenders.append(&mut args_processed);
                        None
                    }
                    ast::Statement::Block { stmts, .. } => {
                        contenders.append(
                            &mut stmts
                                .into_iter()
                                .map(|x| {
                                    Contender::StatementOrExpression(
                                        StatementOrExpression::Statement(x),
                                    )
                                })
                                .collect(),
                        );
                        None
                    }
                    ast::Statement::Assert { arg, .. } => {
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(arg),
                        ));
                        None
                    }
                },
                StatementOrExpression::Expression(expression) => match expression {
                    ast::Expression::InfixOp { lhe, rhe, .. } => {
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(lhe),
                        ));
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(rhe),
                        ));
                        None
                    }
                    ast::Expression::PrefixOp { rhe, .. } => {
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(rhe),
                        ));
                        None
                    }
                    ast::Expression::InlineSwitchOp {
                        cond,
                        if_true,
                        if_false,
                        ..
                    } => {
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(cond),
                        ));
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(if_true),
                        ));
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(if_false),
                        ));
                        None
                    }
                    ast::Expression::ParallelOp { rhe, .. } => {
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(rhe),
                        ));
                        None
                    }
                    ast::Expression::Variable { name, access, .. } => {
                        // TODO: get_reduces_to panics by default, make it so it can print custom error
                        // message depending on where reduction failed
                        let type_reduction = meta.get_type_knowledge().get_reduces_to();
                        let access = Access(
                            access
                                .into_iter()
                                .take_while(|x| match x {
                                    ast::Access::ArrayAccess(_) => true,
                                    _ => false,
                                })
                                .map(|x| match x {
                                    ast::Access::ArrayAccess(e) => match e {
                                        ast::Expression::Number(_, big_int) => {
                                            Some(AccessType::Num(
                                                big_int
                                                    .to_u32()
                                                    .expect("signal array length shouldnt be big"),
                                            ))
                                        }
                                        ast::Expression::Variable { name, .. } => {
                                            Some(AccessType::Var(name.to_owned()))
                                        }
                                        _ => None,
                                    },
                                    _ => unreachable!(),
                                })
                                .collect(),
                        );

                        if word == name {
                            Some(match type_reduction {
                                ast::TypeReduction::Variable => TokenType::Variable(access),
                                ast::TypeReduction::Component => TokenType::Component(access),
                                ast::TypeReduction::Signal => TokenType::Signal(access),
                                ast::TypeReduction::Tag => TokenType::Tag,
                            })
                        } else {
                            None
                        }
                    }
                    ast::Expression::Call { id, args, .. } => {
                        // order matters here
                        if id == word {
                            contenders.push(Contender::Contender(TokenInfo::new(
                                word.to_owned(),
                                TokenType::Defintion(
                                    find_definition_type(word, archive)
                                        .expect("call should have valid defintion"),
                                ),
                                meta,
                                archive,
                                document,
                            )));
                        }

                        let mut args_processed = args
                            .into_iter()
                            .map(|x| {
                                Contender::StatementOrExpression(StatementOrExpression::Expression(
                                    x,
                                ))
                            })
                            .collect();

                        contenders.append(&mut args_processed);
                        None
                    }
                    ast::Expression::AnonymousComp {
                        id,
                        params,
                        signals,
                        ..
                    } => {
                        // order matters here
                        if id == word {
                            contenders.push(Contender::Contender(TokenInfo::new(
                                word.to_owned(),
                                TokenType::Defintion(
                                    find_definition_type(word, archive)
                                        .expect("call should have valid definiton"),
                                ),
                                meta,
                                archive,
                                document,
                            )));
                        }

                        let mut params_processed = params
                            .into_iter()
                            .map(|x| {
                                Contender::StatementOrExpression(StatementOrExpression::Expression(
                                    x,
                                ))
                            })
                            .collect();
                        let mut signals_processed = signals
                            .into_iter()
                            .map(|x| {
                                Contender::StatementOrExpression(StatementOrExpression::Expression(
                                    x,
                                ))
                            })
                            .collect();

                        contenders.append(&mut params_processed);
                        contenders.append(&mut signals_processed);
                        None
                    }
                    ast::Expression::ArrayInLine { values, .. } => {
                        let mut values_processed = values
                            .into_iter()
                            .map(|x| {
                                Contender::StatementOrExpression(StatementOrExpression::Expression(
                                    x,
                                ))
                            })
                            .collect();

                        contenders.append(&mut values_processed);
                        None
                    }
                    ast::Expression::Tuple { values, .. } => {
                        let mut values_processed = values
                            .into_iter()
                            .map(|x| {
                                Contender::StatementOrExpression(StatementOrExpression::Expression(
                                    x,
                                ))
                            })
                            .collect();

                        contenders.append(&mut values_processed);
                        None
                    }
                    ast::Expression::UniformArray {
                        value, dimension, ..
                    } => {
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(dimension),
                        ));
                        contenders.push(Contender::StatementOrExpression(
                            StatementOrExpression::Expression(value),
                        ));
                        None
                    }
                    _ => None,
                },
            };

            if let Some(token_type) = token_type {
                Ok(TokenInfo::new(
                    word.to_owned(),
                    token_type,
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
            TokenType::Tag => write!(f, "Tag"),
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
            let as_str_vector = self
                .0
                .iter()
                .map(|x| match x {
                    Some(x) => format!("{}", x),
                    None => "_".to_string(),
                })
                .collect::<Vec<_>>();

            // TODO: do not rely on debug representation of vectors
            write!(f, "{:?}", as_str_vector)
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
