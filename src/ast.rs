use circom_structure::abstract_syntax_tree::ast;
use circom_structure::template_data::TemplateData;
use circom_structure::function_data::FunctionData;
use circom_structure::file_definition::FileLibrary;
use codespan_reporting::files::SimpleFile;

use num_traits::cast::ToPrimitive;

use crate::wrappers::*;

pub enum Contender<'a> {
    StatementOrExpression(StatementOrExpression<'a>),
    Contender(TokenType)
}

#[derive(Clone, Copy)]
pub enum StatementOrExpression<'a> {
    Statement(&'a ast::Statement),
    Expression(&'a ast::Expression)
}

#[derive(Debug)]
pub enum TokenType {
    Variable(Vec<Option<AccessType>>),
    Signal(Vec<Option<AccessType>>),
    Component(Vec<Option<AccessType>>),
    Defintion(DefintionType),
    Tag
}

#[derive(Debug)]
pub enum DefintionType {
    Template,
    Function
}

#[derive(Debug)]
pub enum AccessType {
    Num(u32),
    Var(String),
}

pub struct TokenInfo {
    name: String,
    token_type: TokenType,
}

impl TokenInfo {
    pub fn description(&self) -> String {
        format!("{}: {:?}", self.name, self.token_type)
    }
}

pub fn find_token(start: usize, word: &str, file_id: usize, archive: &ProgramArchive) -> Option<TokenInfo> {
    let mut statements_or_expressions = archive.inner.functions.values()
        .filter_map(|x| {
            if x.get_file_id() == file_id {
                Some(Contender::StatementOrExpression(StatementOrExpression::Statement(x.get_body())))
            } else {
                None
            }
        }).chain(archive.inner.templates.values()
                 .filter_map(|x| {
                     if x.get_file_id() == file_id {
                         Some(Contender::StatementOrExpression(StatementOrExpression::Statement(x.get_body())))
                     } else {
                         None
                     }
                 }))
    .collect::<Vec<_>>();

    statements_or_expressions.push(Contender::StatementOrExpression(StatementOrExpression::Expression(&archive.inner.initial_template_call)));

    loop {
        let statement_or_expression = statements_or_expressions.pop()?;

        match iterate_contender(start, word, statement_or_expression, archive) {
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

fn iterate_contender<'a>(
    start: usize, 
    word: &str, 
    contender: Contender<'a>,
    archive: &ProgramArchive
    ) -> Result<TokenType, Vec<Contender<'a>>> {
    match contender {
        Contender::Contender(token_type) => {
            Ok(token_type)
        },
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
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(cond)));
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Statement(&if_case)));
                        if let Some(else_case) = else_case {
                            contenders.push(Contender::StatementOrExpression(StatementOrExpression::Statement(&else_case)));
                        }
                        None
                    },
                    ast::Statement::While { 
                        cond, 
                        stmt,
                        ..
                    } => {
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(cond)));
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Statement(&stmt)));
                        None
                    },
                    ast::Statement::Return { 
                        value,
                        ..
                    } => {
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(value)));
                        None
                    },
                    ast::Statement::InitializationBlock { 
                        initializations,
                        ..
                    } => {
                        contenders.append(&mut initializations.into_iter()
                                          .map(|x| Contender::StatementOrExpression(StatementOrExpression::Statement(x)))
                                          .collect()
                                         );
                        None
                    },
                    ast::Statement::Declaration { 
                        xtype,
                        name,
                        dimensions,
                        ..
                    } => {
                        if name == word {
                            let access = dimensions.into_iter().map(|x| {
                                match x {
                                    ast::Expression::Number(_, big_int) => Some(AccessType::Num(big_int
                                                                                                .to_u32()
                                                                                                .expect("signal array length shouldnt be big")
                                                                                               )),
                                                                                               ast::Expression::Variable { name, .. } => Some(AccessType::Var(name.to_owned())),
                                                                                               _ => None
                                }
                            }).collect();

                            Some(match xtype {
                                ast::VariableType::Var => TokenType::Variable(access),
                                ast::VariableType::Signal(..) => TokenType::Signal(access),
                                ast::VariableType::Component | ast::VariableType::AnonymousComponent => TokenType::Component(access)
                            })
                        } else {
                            None
                        }
                    },
                    ast::Statement::Substitution { 
                        var,
                        op,
                        rhe,
                        access,
                        ..
                    } => {
                        // order matters here
                        if var == word {
                            let access = access.into_iter()
                                .take_while(|x| {
                                    match x {
                                        ast::Access::ArrayAccess(_) => true,
                                        _ => false
                                    }
                                })
                            .map(|x| {
                                match x {
                                    ast::Access::ArrayAccess(e) => match e {
                                        ast::Expression::Number(_, big_int) => Some(AccessType::Num(big_int
                                                                                                    .to_u32()
                                                                                                    .expect("signal array length shouldnt be big"))),
                                                                                                    ast::Expression::Variable { name, .. } => Some(AccessType::Var(name.to_owned())),
                                                                                                    _ => None
                                    },
                                    _ => unreachable!()
                                }
                            })
                            .collect();
                            let token_type = match op {
                                ast::AssignOp::AssignVar => match rhe {
                                    ast::Expression::AnonymousComp { .. } => TokenType::Component(access),
                                    ast::Expression::Call { id, .. } => match find_definition_type(id, archive) {
                                        DefintionType::Template => TokenType::Component(access),
                                        DefintionType::Function => TokenType::Variable(access)
                                    }
                                    _ => TokenType::Variable(access)
                                },
                                ast::AssignOp::AssignSignal | ast::AssignOp::AssignConstraintSignal => TokenType::Signal(access)
                            };
                            contenders.push(Contender::Contender(token_type));
                        }
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(rhe)));
                        None
                    },
                    ast::Statement::MultSubstitution { 
                        lhe,
                        rhe,
                        ..
                    } => {
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(lhe)));
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(rhe)));
                        None
                    },
                    ast::Statement::UnderscoreSubstitution { 
                        rhe,
                        ..
                    } => {
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(rhe)));
                        None
                    },
                    ast::Statement::ConstraintEquality { 
                        lhe,
                        rhe,
                        ..
                    } => {
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(lhe)));
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(rhe)));
                        None
                    },
                    ast::Statement::LogCall { 
                        args,
                        ..
                    } => {
                        let mut args_processed = args.into_iter()
                            .filter_map(|x| {
                                match x {
                                    ast::LogArgument::LogExp(exp) => Some(Contender::StatementOrExpression(StatementOrExpression::Expression(exp))),
                                    _ => None
                                }
                            })
                        .collect();

                        contenders.append(&mut args_processed);
                        None
                    },
                    ast::Statement::Block {
                        stmts,
                        ..
                    } => {
                        contenders.append(&mut stmts.into_iter()
                                          .map(|x| Contender::StatementOrExpression(StatementOrExpression::Statement(x)))
                                          .collect()
                                         );
                        None
                    },
                    ast::Statement::Assert { 
                        meta, 
                        arg 
                    } => {
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(arg)));
                        None
                    }
                },
                StatementOrExpression::Expression(expression) => match expression {
                    ast::Expression::InfixOp { 
                        lhe,
                        rhe,
                        ..
                    } => {
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(lhe)));
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(rhe)));
                        None
                    },
                    ast::Expression::PrefixOp { 
                        rhe,
                        ..
                    } => {
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(rhe)));
                        None
                    },
                    ast::Expression::InlineSwitchOp { 
                        cond,
                        if_true,
                        if_false,
                        ..
                    } => {
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(cond)));
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(if_true)));
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(if_false)));
                        None
                    },
                    ast::Expression::ParallelOp { 
                        rhe,
                        ..
                    } => {
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(rhe)));
                        None
                    },
                    ast::Expression::Variable { 
                        name,
                        access,
                        ..
                    } => {
                        // TODO: get_reduces_to panics by default, make it so it can print custom error
                        // message depending on where reduction failed
                        let type_reduction = meta.get_type_knowledge().get_reduces_to();
                        let access = access.into_iter()
                            .take_while(|x| {
                                match x {
                                    ast::Access::ArrayAccess(_) => true,
                                    _ => false
                                }
                            })
                        .map(|x| {
                            match x {
                                ast::Access::ArrayAccess(e) => match e {
                                    ast::Expression::Number(_, big_int) => Some(AccessType::Num(big_int
                                                                                                .to_u32()
                                                                                                .expect("signal array length shouldnt be big"))),
                                                                                                ast::Expression::Variable { name, .. } => Some(AccessType::Var(name.to_owned())),
                                                                                                _ => None
                                },
                                _ => unreachable!()
                            }
                        })
                        .collect();

                        if word == name {
                            Some(match type_reduction {
                                ast::TypeReduction::Variable => TokenType::Variable(access),
                                ast::TypeReduction::Component => TokenType::Component(access),
                                ast::TypeReduction::Signal => TokenType::Signal(access),
                                ast::TypeReduction::Tag => TokenType::Tag
                            })
                        } else {
                            None
                        }
                    },
                    ast::Expression::Call { 
                        id,
                        args,
                        ..
                    } => {
                        // order matters here
                        if id == word {
                            contenders.push(
                                Contender::Contender(
                                    TokenType::Defintion(find_definition_type(word, archive))
                                    )
                                );
                        }

                        let mut args_processed = args.into_iter()
                            .map(|x| Contender::StatementOrExpression(StatementOrExpression::Expression(x)))
                            .collect();

                        contenders.append(&mut args_processed);
                        None
                    },
                    ast::Expression::AnonymousComp { 
                        id,
                        params,
                        signals,
                        ..
                    } => {
                        // order matters here
                        if id == word {
                            contenders.push(
                                Contender::Contender(
                                    TokenType::Defintion(find_definition_type(word, archive)))
                                );
                        }

                        let mut params_processed = params.into_iter()
                            .map(|x| Contender::StatementOrExpression(StatementOrExpression::Expression(x)))
                            .collect();
                        let mut signals_processed = signals.into_iter()
                            .map(|x| Contender::StatementOrExpression(StatementOrExpression::Expression(x)))
                            .collect();

                        contenders.append(&mut params_processed);
                        contenders.append(&mut signals_processed);
                        None
                    },
                    ast::Expression::ArrayInLine { 
                        values,
                        ..
                    } => {
                        let mut values_processed = values.into_iter()
                            .map(|x| Contender::StatementOrExpression(StatementOrExpression::Expression(x)))
                            .collect();

                        contenders.append(&mut values_processed);
                        None
                    },
                    ast::Expression::Tuple { 
                        values,
                        ..
                    } => {
                        let mut values_processed = values.into_iter()
                            .map(|x| Contender::StatementOrExpression(StatementOrExpression::Expression(x)))
                            .collect();

                        contenders.append(&mut values_processed);
                        None
                    },
                    ast::Expression::UniformArray { 
                        value,
                        dimension,
                        ..
                    } => {
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(dimension)));
                        contenders.push(Contender::StatementOrExpression(StatementOrExpression::Expression(value)));
                        None
                    },
                    _ => None
                }
            };

            if let Some(token_type) = token_type {
                Ok(token_type)
            } else {
                Err(contenders)
            }
        }
    }
}

fn get_meta<'a>(statement_or_expression: StatementOrExpression<'a>) -> &'a ast::Meta {
    match statement_or_expression {
        StatementOrExpression::Statement(x) => match x {
            ast::Statement::IfThenElse { 
                meta,
                ..
            } => meta,
            ast::Statement::While { 
                meta,
                ..
            } => meta,
            ast::Statement::Return { 
                meta,
                ..
            } => meta,
            ast::Statement::InitializationBlock { 
                meta,
                ..
            } => meta,
            ast::Statement::Declaration { 
                meta,
                ..
            } => meta,
            ast::Statement::Substitution { 
                meta,
                ..
            } => meta,
            ast::Statement::MultSubstitution { 
                meta,
                ..
            } => meta,
            ast::Statement::UnderscoreSubstitution { 
                meta,
                ..
            } => meta,
            ast::Statement::ConstraintEquality { 
                meta,
                ..
            } => meta,
            ast::Statement::LogCall { 
                meta,
                ..
            } => meta,
            ast::Statement::Block {
                meta,
                ..
            } => meta,
            ast::Statement::Assert { 
                meta,
                ..
            } => meta 
        },
        StatementOrExpression::Expression(x) => match x {
            ast::Expression::InfixOp { 
                meta,
                ..
            } => meta,
            ast::Expression::PrefixOp { 
                meta,
                ..
            } => meta,
            ast::Expression::InlineSwitchOp { 
                meta,
                ..
            } => meta,
            ast::Expression::ParallelOp { 
                meta,
                ..
            } => meta,
            ast::Expression::Variable { 
                meta,
                ..
            } => meta,
            ast::Expression::Number(meta, _) => meta,
            ast::Expression::Call { 
                meta,
                ..
            } => meta,
            ast::Expression::AnonymousComp { 
                meta,
                ..
            } => meta,
            ast::Expression::ArrayInLine { 
                meta,
                ..
            } => meta,
            ast::Expression::Tuple { 
                meta,
                ..
            } => meta,
            ast::Expression::UniformArray { 
                meta,
                ..
            } => meta,
        }
    }
}

#[derive(Clone, Copy)]
pub enum DefinitionData<'a> {
    Template(&'a TemplateData),
    Function(&'a FunctionData)
}

pub fn definition_location<'a>(
    defintion_data: DefinitionData, 
    file_library: &'a FileLibrary
    ) -> (&'a SimpleFile<String, String>, usize) {
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

pub fn find_definition<'a>(name: &str, archive: &'a ProgramArchive) -> Option<DefinitionData<'a>> {
    if let Some(template_data) = archive.inner.templates.get(name) {
        Some(DefinitionData::Template(template_data))
    } else if let Some(function_data) = archive.inner.functions.get(name) {
        Some(DefinitionData::Function(function_data))
    } else {
        None
    }
}
