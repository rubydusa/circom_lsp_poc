use circom_structure::abstract_syntax_tree::ast;
use crate::backend::ProgramArchive;
use num_traits::cast::ToPrimitive;

pub enum ASTNode<'a> {
    Statement(&'a ast::Statement),
    Expression(&'a ast::Expression),
    Contender(TokenType)
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

pub fn find_ast_node(start: usize, word: &str, file_id: usize, archive: &ProgramArchive) -> Option<TokenInfo> {
    let mut statements_or_expressions = archive.inner.functions.values()
        .filter_map(|x| {
            if x.get_file_id() == file_id {
                Some(ASTNode::Statement(x.get_body()))
            } else {
                None
            }
        }).chain(archive.inner.templates.values()
                 .filter_map(|x| {
                     if x.get_file_id() == file_id {
                         Some(ASTNode::Statement(x.get_body()))
                     } else {
                         None
                     }
                 }))
    .collect::<Vec<_>>();

    statements_or_expressions.push(ASTNode::Expression(&archive.inner.initial_template_call));

    loop {
        let statement_or_expression = statements_or_expressions.pop()?;

        match iterate_statement_or_expression(start, word, statement_or_expression, archive) {
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
    statement_or_expression: ASTNode<'a>,
    archive: &ProgramArchive
    ) -> Result<TokenType, Vec<ASTNode<'a>>> {
    let mut statements_or_expressions = Vec::new();
    match statement_or_expression {
        ASTNode::Contender(token_type) => {
            return Ok(token_type)
        },
        ASTNode::Statement(statement) => match statement {
            ast::Statement::IfThenElse { 
                meta: ast::Meta { start: s_start, end: s_end, .. }, 
                cond, 
                if_case, 
                else_case 
            } => {
                if *s_start <= start && start <= *s_end {
                    statements_or_expressions.push(ASTNode::Expression(cond));
                    statements_or_expressions.push(ASTNode::Statement(&if_case));
                    if let Some(else_case) = else_case {
                        statements_or_expressions.push(ASTNode::Statement(&else_case));
                    }
                }
            },
            ast::Statement::While { 
                meta: ast::Meta { start: s_start, end: s_end, ..}, 
                cond, 
                stmt 
            } => {
                if *s_start <= start && start <= *s_end {
                    statements_or_expressions.push(ASTNode::Expression(cond));
                    statements_or_expressions.push(ASTNode::Statement(&stmt));
                }
            },
            ast::Statement::Return { 
                meta: ast::Meta { start: s_start, end: s_end, ..}, 
                value
            } => {
                if *s_start <= start && start <= *s_end {
                    statements_or_expressions.push(ASTNode::Expression(value));
                }
            },
            ast::Statement::InitializationBlock { 
                meta: ast::Meta { start: s_start, end: s_end, ..}, 
                initializations,
                ..
            } => {
                if *s_start <= start && start <= *s_end {
                    statements_or_expressions.append(&mut initializations.into_iter()
                                                     .map(|x| ASTNode::Statement(x))
                                                     .collect()
                                                    );
                }
            },
            ast::Statement::Declaration { 
                meta: ast::Meta { start: s_start, end: s_end, ..}, 
                xtype,
                name,
                dimensions,
                ..
            } => {
                if *s_start <= start && start <= *s_end {
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

                        return Ok(match xtype {
                            ast::VariableType::Var => TokenType::Variable(access),
                            ast::VariableType::Signal(..) => TokenType::Signal(access),
                            ast::VariableType::Component | ast::VariableType::AnonymousComponent => TokenType::Component(access)
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
                access,
                ..
            } => {
                if *s_start <= start && start <= *s_end {
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
                        statements_or_expressions.push(ASTNode::Contender(token_type));
                    }
                    statements_or_expressions.push(ASTNode::Expression(rhe));
                }
            },
            ast::Statement::MultSubstitution { 
                meta: ast::Meta { start: s_start, end: s_end, ..}, 
                lhe,
                rhe,
                ..
            } => {
                if *s_start <= start && start <= *s_end {
                    statements_or_expressions.push(ASTNode::Expression(lhe));
                    statements_or_expressions.push(ASTNode::Expression(rhe));
                }
            },
            ast::Statement::UnderscoreSubstitution { 
                meta: ast::Meta { start: s_start, end: s_end, ..}, 
                rhe,
                ..
            } => {
                if *s_start <= start && start <= *s_end {
                    statements_or_expressions.push(ASTNode::Expression(rhe));
                }
            },
            ast::Statement::ConstraintEquality { 
                meta: ast::Meta { start: s_start, end: s_end, ..}, 
                lhe,
                rhe,
                ..
            } => {
                if *s_start <= start && start <= *s_end {
                    statements_or_expressions.push(ASTNode::Expression(lhe));
                    statements_or_expressions.push(ASTNode::Expression(rhe));
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
                                ast::LogArgument::LogExp(exp) => Some(ASTNode::Expression(exp)),
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
                                                     .map(|x| ASTNode::Statement(x))
                                                     .collect()
                                                    );
                }
            },
            ast::Statement::Assert { 
                meta: ast::Meta { start: s_start, end: s_end, .. }, 
                arg 
            } => {
                if *s_start <= start && start <= *s_end {
                    statements_or_expressions.push(ASTNode::Expression(arg));
                }
            }
        },
        ASTNode::Expression(expression) => match expression {
            ast::Expression::InfixOp { 
                meta: ast::Meta { start: s_start, end: s_end, .. }, 
                lhe,
                rhe,
                ..
            } => {
                if *s_start <= start && start <= *s_end {
                    statements_or_expressions.push(ASTNode::Expression(lhe));
                    statements_or_expressions.push(ASTNode::Expression(rhe));
                }
            },
            ast::Expression::PrefixOp { 
                meta: ast::Meta { start: s_start, end: s_end, .. }, 
                rhe,
                ..
            } => {
                if *s_start <= start && start <= *s_end {
                    statements_or_expressions.push(ASTNode::Expression(rhe));
                }
            },
            ast::Expression::InlineSwitchOp { 
                meta: ast::Meta { start: s_start, end: s_end, .. }, 
                cond,
                if_true,
                if_false
            } => {
                if *s_start <= start && start <= *s_end {
                    statements_or_expressions.push(ASTNode::Expression(cond));
                    statements_or_expressions.push(ASTNode::Expression(if_true));
                    statements_or_expressions.push(ASTNode::Expression(if_false));
                }
            },
            ast::Expression::ParallelOp { 
                meta: ast::Meta { start: s_start, end: s_end, .. }, 
                rhe
            } => {
                if *s_start <= start && start <= *s_end {
                    statements_or_expressions.push(ASTNode::Expression(rhe));
                }
            },
            // TODO: Support access
            ast::Expression::Variable { 
                meta,
                name,
                access,
                ..
            } => {
                let ast::Meta { start: s_start, end: s_end, .. } = meta;
                if *s_start <= start && start <= *s_end {
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
                        return Ok(match type_reduction {
                            ast::TypeReduction::Variable => TokenType::Variable(access),
                            ast::TypeReduction::Component => TokenType::Component(access),
                            ast::TypeReduction::Signal => TokenType::Signal(access),
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
                            ASTNode::Contender(
                                TokenType::Defintion(find_definition_type(word, archive))
                                )
                            );
                    }

                    let mut args_processed = args.into_iter()
                        .map(|x| ASTNode::Expression(x))
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
                            ASTNode::Contender(
                                TokenType::Defintion(find_definition_type(word, archive)))
                            );
                    }

                    let mut params_processed = params.into_iter()
                        .map(|x| ASTNode::Expression(x))
                        .collect();
                    let mut signals_processed = signals.into_iter()
                        .map(|x| ASTNode::Expression(x))
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
                        .map(|x| ASTNode::Expression(x))
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
                        .map(|x| ASTNode::Expression(x))
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
                    statements_or_expressions.push(ASTNode::Expression(dimension));
                    statements_or_expressions.push(ASTNode::Expression(value));
                }
            },
                _ => ()
        }
    }

    return Err(statements_or_expressions);
}
