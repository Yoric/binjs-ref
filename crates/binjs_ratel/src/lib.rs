//! An attempt at converting between Ratel and BinAST.
//!
//! The resulting AST doesn't contain scope information.

extern crate binjs_shared;
extern crate binjs_es6;
extern crate ratel;

use binjs_shared::SharedString;

use std::collections::HashSet;

pub enum Error {}

pub trait FromRatel<T> {
    fn transcode(&self, all_strings: &mut HashSet<SharedString>) -> Result<T, Error>;
}

// ---- From ratel

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

impl FromRatel<binjs_es6::ast::Script> for ratel::grammar::Program {
    fn transcode(&self, all_strings: &mut HashSet<SharedString>) -> Result<binjs_es6::ast::Script, Error> {
        let mut directives = vec![];
        let mut statements = Vec::with_capacity(self.body.len());
        'per_statement: for statement in &self.body {
            if statements.len() == 0 {
                // This may still be a directive.
                if let ratel::grammar::Statement::Expression {
                    value: ratel::grammar::Expression::Literal(
                        ratel::grammar::Value::String(ref string)
                    )
                 } = *statement {
                     directives.push(binjs_es6::ast::Directive {
                         raw_value: SharedString::from_string(string.to_string())
                     });
                     continue 'per_statement;
                }
            }

            let statement = FromRatel::<binjs_es6::ast::Statement>::transcode(statement, all_strings)?;
            statements.push(statement);
        }

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
            ratel::grammar::Statement::Empty => Ok(binjs_es6::ast::Statement::EmptyStatement(
                Box::new(binjs_es6::ast::EmptyStatement {})
            )),
            ratel::grammar::Statement::Expression { ref value } => {
                let expression = value.transcode(all_strings)?;
                Ok(binjs_es6::ast::Statement::ExpressionStatement(Box::new(expression)))
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
            ratel::grammar::Statement::Break { ref label } => {
                let label = label.transcode(all_strings)?;
                Ok(binjs_es6::ast::Statement::BreakStatement(
                    Box::new(binjs_es6::ast::BreakStatement {
                        label
                    })
                ))
            }
            _ => unimplemented!()
        }
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