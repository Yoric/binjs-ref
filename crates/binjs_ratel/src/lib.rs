//! An attempt at converting between Ratel and BinAST.
//!
//! The resulting AST doesn't contain scope information.

extern crate binjs_shared;
extern crate binjs_es6;
extern crate ratel;

use binjs_shared::{ IdentifierName, SharedString };

use std::collections::HashSet;

pub enum Error {
    InvalidForInit,
}

pub trait FromRatel<T> {
    fn transcode(&self, all_strings: &mut HashSet<SharedString>) -> Result<T, Error>;
}

// ---- From ratel

// ---- Combinators
impl<T, U> FromRatel<T> for Box<U> where U: FromRatel<T> {
    fn transcode(&self, all_strings: &mut HashSet<SharedString>) -> Result<T, Error> {
        Ok(self.as_ref().transcode(all_strings)?)
    }
}

impl<T, U> FromRatel<Vec<T>> for Vec<U> where U: FromRatel<T> {
    fn transcode(&self, all_strings: &mut HashSet<SharedString>) -> Result<Vec<T>, Error> {
        let mut destination = Vec::with_capacity(self.len());
        for source in self {
            destination.push(source.transcode(all_strings)?);
        }
        Ok(destination)
    }
}

impl<T, U> FromRatel<Option<T>> for Option<U> where U: FromRatel<T> {
    fn transcode(&self, all_strings: &mut HashSet<SharedString>) -> Result<Option<T>, Error> {
        match self {
            None => Ok(None),
            Some(v) => {
                let transcoded = v.transcode(all_strings)?;
                Ok(Some(transcoded))
            }
        }
    }
}

impl FromRatel<SharedString> for ratel::owned_slice::OwnedSlice {
    /// Avoid needless string duplication.
    fn transcode(&self, all_strings: &mut HashSet<SharedString>) -> Result<SharedString, Error> {
        if let Some(shared) = all_strings.get(self.as_str()) {
            return Ok(shared.clone())
        }
        let shared = SharedString::from_string(self.to_string());
        all_strings.insert(shared.clone());
        Ok(shared)
    }
}

impl FromRatel<IdentifierName> for ratel::owned_slice::OwnedSlice {
    /// Avoid needless string duplication.
    fn transcode(&self, all_strings: &mut HashSet<SharedString>) -> Result<IdentifierName, Error> {
        let as_shared_string : SharedString = self.transcode(all_strings)?;
        let as_identifier_name = IdentifierName(as_shared_string);
        Ok(as_identifier_name)
    }
}

impl FromRatel<binjs_es6::ast::BindingIdentifier> for ratel::owned_slice::OwnedSlice {
    /// Avoid needless string duplication.
    fn transcode(&self, all_strings: &mut HashSet<SharedString>) -> Result<binjs_es6::ast::BindingIdentifier, Error> {
        let name : IdentifierName = self.transcode(all_strings)?;
        Ok(binjs_es6::ast::BindingIdentifier {
            name
        })
    }
}

impl FromRatel<(Vec<binjs_es6::ast::Directive>, Vec<binjs_es6::ast::Statement>)> for Vec<ratel::grammar::Statement> {
    fn transcode(&self, all_strings: &mut HashSet<SharedString>) -> Result<(Vec<binjs_es6::ast::Directive>, Vec<binjs_es6::ast::Statement>), Error> {
        let mut directives = vec![];
        let mut statements = Vec::with_capacity(self.len());
        'per_statement: for statement in self {
            if statements.len() == 0 {
                // This may still be a directive.
                if let ratel::grammar::Statement::Expression {
                    value: ratel::grammar::Expression::Literal(
                        ratel::grammar::Value::String(ref string)
                    )
                 } = *statement {
                     let raw_value = string.transcode(all_strings)?;
                     directives.push(binjs_es6::ast::Directive {
                         raw_value
                     });
                     continue 'per_statement;
                }
            }

            let statement = FromRatel::<binjs_es6::ast::Statement>::transcode(statement, all_strings)?;
            statements.push(statement);
        }

        Ok((directives, statements))
    }
}

impl FromRatel<binjs_es6::ast::Script> for ratel::grammar::Program {
    fn transcode(&self, all_strings: &mut HashSet<SharedString>) -> Result<binjs_es6::ast::Script, Error> {
        let (directives, statements) = self.body.transcode(all_strings)?;
        Ok(binjs_es6::ast::Script {
            directives,
            statements,
            ..Default::default()
        })
    }
}

impl FromRatel<binjs_es6::ast::VariableDeclarationKind> for ratel::grammar::VariableDeclarationKind {
    fn transcode(&self, _all_strings: &mut HashSet<SharedString>) -> Result<binjs_es6::ast::VariableDeclarationKind, Error> {
        match *self {
            ratel::grammar::VariableDeclarationKind::Let => Ok(binjs_es6::ast::VariableDeclarationKind::Let),
            ratel::grammar::VariableDeclarationKind::Const => Ok(binjs_es6::ast::VariableDeclarationKind::Const),
            ratel::grammar::VariableDeclarationKind::Var => Ok(binjs_es6::ast::VariableDeclarationKind::Var),
        }
    }
}

impl FromRatel<binjs_es6::ast::Statement> for ratel::grammar::Statement {
    fn transcode(&self, all_strings: &mut HashSet<SharedString>) -> Result<binjs_es6::ast::Statement, Error> {
        match *self {
            ratel::grammar::Statement::Block { ref body } => {
                let mut statements = Vec::with_capacity(body.len());
                for source in body {
                    let statement = FromRatel::<binjs_es6::ast::Statement>::transcode(source, all_strings)?;
                    statements.push(statement);
                }
                Ok(binjs_es6::ast::Statement::Block(Box::new(binjs_es6::ast::Block {
                    statements,
                    ..binjs_es6::ast::Block::default()
                })))
            },
            ratel::grammar::Statement::Break { ref label } => {
                let label = label.transcode(all_strings)?;
                Ok(binjs_es6::ast::Statement::BreakStatement(
                    Box::new(binjs_es6::ast::BreakStatement {
                        label
                    })
                ))
            }
            ratel::grammar::Statement::Empty => Ok(binjs_es6::ast::Statement::EmptyStatement(
                Box::new(binjs_es6::ast::EmptyStatement {})
            )),
            ratel::grammar::Statement::Expression { ref value } => {
                let expression = value.transcode(all_strings)?;
                Ok(binjs_es6::ast::Statement::ExpressionStatement(Box::new(expression)))
            },
            ratel::grammar::Statement::Function { ref name, ref params, ref body } => {
                let name = name.transcode(all_strings)?;
                let length = params.len() as u32;
                let (directives, body) = body.transcode(all_strings)?;
                let params = params.transcode(all_strings)?;
                let contents = binjs_es6::ast::FunctionOrMethodContents {
                    params,
                    body,
                    ..Default::default()
                };
                Ok(binjs_es6::ast::Statement::EagerFunctionDeclaration(Box::new(
                    binjs_es6::ast::EagerFunctionDeclaration {
                        name,
                        length,
                        directives,
                        contents,
                        ..Default::default()
                    }
                )))
            },
            ratel::grammar::Statement::If { ref test, ref consequent, ref alternate } => {
                let test = test.transcode(all_strings)?;
                let consequent = consequent.transcode(all_strings)?;
                let alternate = alternate.transcode(all_strings)?;
                Ok(binjs_es6::ast::Statement::IfStatement(Box::new(
                    binjs_es6::ast::IfStatement {
                        test,
                        consequent,
                        alternate
                    }
                )))
            },
            ratel::grammar::Statement::For { ref init, ref test, ref update, ref body } => {
                let init = match *init {
                    None => None,
                    Some(ref content) =>
                        match content.as_ref() {
                            &ratel::grammar::Statement::Expression { ref value } => {
                                let expression : binjs_es6::ast::Expression = value.transcode(all_strings)?;
                                Some(expression.into())
                            }
                            &ratel::grammar::Statement::VariableDeclaration { ref kind, ref declarators } => {
                                let kind = kind.transcode(all_strings)?;
                                let declarators = declarators.transcode(all_strings)?;
                                Some(binjs_es6::ast::VariableDeclarationOrExpression::VariableDeclaration(
                                    Box::new(binjs_es6::ast::VariableDeclaration {
                                        kind,
                                        declarators,
                                    }.into())
                                ))
                            }
                            _ => return Err(Error::InvalidForInit)
                        }
                };
                let test = test.transcode(all_strings)?;
                let update = update.transcode(all_strings)?;
                let body = body.transcode(all_strings)?;
                Ok(binjs_es6::ast::Statement::ForStatement(Box::new(
                    binjs_es6::ast::ForStatement {
                        init,
                        test,
                        update,
                        body
                    }
                )))
            },
            ratel::grammar::Statement::Labeled { ref label, ref body } => {
                let label = FromRatel::<SharedString>::transcode(label, all_strings)?;
                let body = FromRatel::<binjs_es6::ast::Statement>::transcode(body.as_ref(), all_strings)?;
                Ok(binjs_es6::ast::Statement::LabelledStatement(
                    Box::new(binjs_es6::ast::LabelledStatement {
                        body,
                        label,
                    })
                ))
            }
            ratel::grammar::Statement::Return { ref value } => {
                let expression = value.transcode(all_strings)?;
                Ok(binjs_es6::ast::Statement::ReturnStatement(
                    Box::new(binjs_es6::ast::ReturnStatement {
                        expression
                    })
                ))
            }
            ratel::grammar::Statement::VariableDeclaration { ref kind, ref declarators } => {
                let kind = kind.transcode(all_strings)?;
                let declarators = declarators.transcode(all_strings)?;
                Ok(binjs_es6::ast::Statement::VariableDeclaration(
                    Box::new(binjs_es6::ast::VariableDeclaration {
                        kind,
                        declarators,
                    })
                ))
            }
            ratel::grammar::Statement::While { ref test, ref body } => {
                let test = test.transcode(all_strings)?;
                let body = body.transcode(all_strings)?;
                Ok(binjs_es6::ast::Statement::WhileStatement(Box::new(
                    binjs_es6::ast::WhileStatement {
                        test,
                        body,
                    }
                )))
            },
            _ => unimplemented!()
        }
    }
}

impl FromRatel<binjs_es6::ast::FormalParameters> for Vec<ratel::grammar::Parameter> {
    fn transcode(&self, _all_strings: &mut HashSet<SharedString>) -> Result<binjs_es6::ast::FormalParameters, Error> {
        unimplemented!()
    }
}

impl FromRatel<binjs_es6::ast::VariableDeclarator> for ratel::grammar::VariableDeclarator {
    fn transcode(&self, _all_strings: &mut HashSet<SharedString>) -> Result<binjs_es6::ast::VariableDeclarator, Error> {
        unimplemented!()
    }
}

impl FromRatel<binjs_es6::ast::Expression> for ratel::grammar::Expression {
    fn transcode(&self, _all_strings: &mut HashSet<SharedString>) -> Result<binjs_es6::ast::Expression, Error> {
        unimplemented!()
    }
}

impl FromRatel<binjs_es6::ast::ExpressionStatement> for ratel::grammar::Expression {
    fn transcode(&self, all_strings: &mut HashSet<SharedString>) -> Result<binjs_es6::ast::ExpressionStatement, Error> {
        let expression = FromRatel::<binjs_es6::ast::Expression>::transcode(self, all_strings)?;
        Ok(binjs_es6::ast::ExpressionStatement { expression })
    }
}

// ---- To ratel