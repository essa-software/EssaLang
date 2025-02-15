use core::str;
use std::{
    io::{BufRead, Cursor, Read, Write},
    ops::Range,
    str::{Chars, Utf8Error},
};

use crate::lexer;

#[derive(Debug)]
pub enum Type {
    Simple(String),
}

#[derive(Debug)]
pub struct FunctionArg {
    pub param: Option<String>, // for kwargs
    pub value: Expression,
}

#[derive(Debug)]
pub enum Expression {
    Call {
        function: String, // TODO: arbitrary expressions
        args: Vec<FunctionArg>,
    },
    BoolLiteral {
        value: bool,
    },
    IntLiteral {
        value: u64,
    },
    StringLiteral {
        value: String,
    },
    Name(String),
}

#[derive(Debug)]
pub enum Statement {
    VarDecl {
        mut_: bool,
        type_: Type,
        name: String,
        init_value: Option<Expression>,
    },
    Expression(Expression),
    Return(Option<Expression>),
    Block(Vec<Statement>),
    If {
        condition: Expression,
        then_block: Box<Statement>,
        // TODO: else
    },
}

#[derive(Debug)]
pub enum Declaration {
    FunctionImpl {
        name: String,
        return_type: Option<Type>,
        body: Statement,
    },
}

#[derive(Debug)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

////
pub struct ParseError {
    pub message: String,
    pub range: Range<usize>,
}

pub struct Parser<'a> {
    input: &'a [u8],
    iter: lexer::TokenIterator<'a>,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a [u8]) -> Result<Self, Utf8Error> {
        Ok(Self {
            input,
            iter: lexer::TokenIterator::new(input)?,
            errors: vec![],
        })
    }

    // Consume next token, and return error if it doesn't match
    // the predicate
    pub fn expect_msg(
        &mut self,
        predicate: impl Fn(&lexer::TokenType) -> bool,
        more_info: &str,
    ) -> Option<lexer::Token> {
        let next = self.iter.next();
        if let Some(next) = next {
            if predicate(&next.type_) {
                return Some(next);
            } else {
                self.errors.push(ParseError {
                    message: if more_info.is_empty() {
                        "Unexpected token".into()
                    } else {
                        format!("Expected {}", more_info)
                    },
                    range: next.range.clone(),
                });
            }
        }
        None
    }

    pub fn expect(
        &mut self,
        predicate: impl Fn(&lexer::TokenType) -> bool,
    ) -> Option<lexer::Token> {
        self.expect_msg(predicate, "")
    }

    // var-decl ::= "let" name ":" type "=" expression ";"
    pub fn consume_var_decl(&mut self) -> Option<Statement> {
        // "let"
        let _ = self.expect(|t| matches!(t, lexer::TokenType::KeywordLet));

        // name
        let name = self.expect_msg(|t| matches!(t, lexer::TokenType::Name(_)), "variable name")?;

        // ":"
        let _ = self.expect(|t| matches!(t, lexer::TokenType::Colon));

        // type
        let type_ = self.consume_type()?;

        // "="
        let _ = self.expect(|t| matches!(t, lexer::TokenType::OpEquals));

        // expression
        let init_value = self.consume_expression()?;

        // ";"
        let _ = self.expect(|t| matches!(t, lexer::TokenType::Semicolon));

        Some(Statement::VarDecl {
            mut_: false,
            name: name.slice_str(&self.input).unwrap().into(),
            type_,
            init_value: Some(init_value),
        })
    }

    pub fn consume_block(&mut self) -> Statement {
        // "{"
        let _ = self.expect(|t| matches!(t, lexer::TokenType::CurlyOpen));

        // statements
        let mut statements = vec![];
        loop {
            let next = self.iter.clone().next();
            if let Some(next) = next {
                if matches!(next.type_, lexer::TokenType::CurlyClose) {
                    break;
                }
            } else {
                break;
            }

            let statement = self.consume_statement();
            if let Some(statement) = statement {
                statements.push(statement);
            } else {
                break;
            }
        }

        // "}"
        let _ = self.expect(|t| matches!(t, lexer::TokenType::CurlyClose));

        Statement::Block(statements)
    }

    // `(expression ",")* expression ","?` delimited by token matched by `is_end` (it is not consumed)
    // note: trailing "," IS allowed here
    pub fn consume_comma_separated_expression_list(
        &mut self,
        is_end: impl Fn(lexer::TokenType) -> bool,
    ) -> Vec<Expression> {
        let mut expressions = vec![];
        if let Some(a) = self.iter.clone().next() {
            if is_end(a.type_) {
                return expressions;
            }
        }
        loop {
            let expr = self.consume_expression();
            if let Some(expr) = expr {
                expressions.push(expr);
            } else {
                break;
            }

            let next = {
                if let Some(a) = self.iter.clone().next() {
                    a
                } else {
                    break;
                }
            };
            if is_end(next.type_) {
                break;
            } else {
                let _ = self.expect(|t| matches!(t, lexer::TokenType::Comma));
            }
        }
        expressions
    }

    pub fn consume_expression(&mut self) -> Option<Expression> {
        let next = self.iter.clone().next()?;
        match next.type_ {
            lexer::TokenType::Name(name) => {
                // consume
                let _ = self.iter.next();

                // it might be also a function call
                let next = self.iter.clone().next()?;
                if let lexer::TokenType::ParenOpen = next.type_ {
                    let _ = self.iter.next(); // consume "("
                    let args = self.consume_comma_separated_expression_list(|t| {
                        matches!(t, lexer::TokenType::ParenClose)
                    });
                    let _ = self.expect(|t| matches!(t, lexer::TokenType::ParenClose));
                    Some(Expression::Call {
                        function: name,
                        args: args
                            .into_iter()
                            .map(|arg| FunctionArg {
                                param: None,
                                value: arg,
                            })
                            .collect(),
                    })
                } else {
                    Some(Expression::Name(name))
                }
            }
            lexer::TokenType::Integer(text) => {
                let _ = self.iter.next();
                Some(Expression::IntLiteral { value: text })
            }
            lexer::TokenType::KeywordTrue => {
                let _ = self.iter.next();
                Some(Expression::BoolLiteral { value: true })
            }
            lexer::TokenType::KeywordFalse => {
                let _ = self.iter.next();
                Some(Expression::BoolLiteral { value: false })
            }
            lexer::TokenType::StringLiteral(text) => {
                let _ = self.iter.next();
                Some(Expression::StringLiteral { value: text })
            }
            _ => {
                self.errors.push(ParseError {
                    message: "Expected expression".into(),
                    range: next.range.clone(),
                });
                let _ = self.iter.next(); //whatever
                None
            }
        }
    }

    pub fn consume_expression_statement(&mut self) -> Option<Statement> {
        let expr = self.consume_expression()?;
        let _ = self.expect(|t| matches!(t, lexer::TokenType::Semicolon));
        Some(Statement::Expression(expr))
    }

    pub fn consume_return_statement(&mut self) -> Option<Statement> {
        // "return"
        let _ = self.expect(|t| matches!(t, lexer::TokenType::KeywordReturn));

        // early ';' for "return;"
        if let Some(next) = self.iter.clone().next() {
            if matches!(next.type_, lexer::TokenType::Semicolon) {
                let _ = self.iter.next();
                return Some(Statement::Return(None));
            }
        }

        // expression
        let expr = self.consume_expression()?;

        // ";"
        let _ = self.expect_msg(
            |t| matches!(t, lexer::TokenType::Semicolon),
            "';' after return",
        );

        Some(Statement::Return(Some(expr)))
    }

    // if-statement ::= "if" "(" expression ")" block [ "else" block ]
    pub fn consume_if_statement(&mut self) -> Option<Statement> {
        // "if"
        let _ = self.expect(|t| matches!(t, lexer::TokenType::KeywordIf))?;

        // "("
        let _ = self.expect(|t| matches!(t, lexer::TokenType::ParenOpen))?;

        // expression
        let condition = self.consume_expression()?;

        // ")"
        let _ = self.expect(|t| matches!(t, lexer::TokenType::ParenClose))?;

        // block
        let then_block = self.consume_block();

        // TODO: else

        Some(Statement::If {
            condition,
            then_block: Box::new(then_block),
        })
    }

    // statement ::= var-decl | expression ';' | block | return-statement | if-statement
    pub fn consume_statement(&mut self) -> Option<Statement> {
        let next = self.iter.clone().next()?;
        // eprintln!("  next token in consume_statement: {:?}", next);
        match next.type_ {
            lexer::TokenType::CurlyOpen => Some(self.consume_block()),
            lexer::TokenType::CurlyClose => panic!("somebody didn't consume '}}'"),
            lexer::TokenType::KeywordLet => self.consume_var_decl(),
            lexer::TokenType::KeywordReturn => self.consume_return_statement(),
            lexer::TokenType::KeywordIf => self.consume_if_statement(),
            _ => self.consume_expression_statement(),
        }
    }

    pub fn consume_type(&mut self) -> Option<Type> {
        let next = self.iter.clone().next()?;
        match next.type_ {
            lexer::TokenType::Name(name) => {
                let _ = self.iter.next();
                Some(Type::Simple(name))
            }
            _ => {
                self.errors.push(ParseError {
                    message: "Expected type".into(),
                    range: next.range.clone(),
                });
                let _ = self.iter.next(); //whatever
                None
            }
        }
    }

    // function-impl ::= "func" name "(" ")" [ ":" type ] "{" statement ... "}"
    // (TODO: args, return value)
    pub fn consume_function_impl(&mut self) -> Option<Declaration> {
        // "func"
        let _ = self.iter.next();

        // name
        let name = self.expect_msg(|t| matches!(t, lexer::TokenType::Name(_)), "function name")?;

        // "("
        let _ = self.expect(|t| matches!(t, lexer::TokenType::ParenOpen))?;

        // ")"
        let _ = self.expect(|t| matches!(t, lexer::TokenType::ParenClose))?;

        // return type
        // TODO: parse_type
        let colon = self
            .iter
            .clone()
            .next()
            .filter(|t| matches!(t.type_, lexer::TokenType::Colon));
        let return_type = if let Some(_) = colon {
            let _ = self.iter.next();
            self.consume_type()
        } else {
            None
        };

        let body = self.consume_block();

        Some(Declaration::FunctionImpl {
            name: name.slice_str(&self.input).unwrap().into(),
            return_type,
            body,
        })
    }

    pub fn consume_declaration(&mut self) -> Option<Declaration> {
        let next = self.iter.clone().next()?;
        eprintln!("next token in consume_declaration: {:?}", next);
        match next.type_ {
            lexer::TokenType::KeywordFunc => self.consume_function_impl(),
            _ => {
                self.errors.push(ParseError {
                    message: "Expected declaration".into(),
                    range: next.range.clone(),
                });
                let _ = self.iter.next(); //whatever
                None
            }
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut declarations = vec![];
        loop {
            if self.iter.clone().next().is_none() {
                break;
            }
            let decl = self.consume_declaration();
            if let Some(decl) = decl {
                declarations.push(decl);
            }
        }

        // print errors
        for error in &self.errors {
            eprintln!("Error:{:?}: {}", error.range, error.message);
        }

        Program { declarations }
    }
}
