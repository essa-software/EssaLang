use core::str;
use std::{
    collections::LinkedList,
    io::{BufRead, Cursor, Read, Write},
    ops::Range,
    str::{Chars, Utf8Error},
};

use crate::{error::CompilationError, lexer};

#[derive(Debug)]
pub enum Type {
    Simple(String),
}

#[derive(Debug)]
pub struct FunctionArg {
    pub param: Option<String>, // for kwargs
    pub value: ExpressionNode,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    CmpEquals,    // ==
    CmpNotEquals, // !=
    CmpLess,      // <
    CmpLessEq,    // <=
    CmpGreater,   // >
    CmpGreaterEq, // >=

    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %

    Assignment, // =
    AssAdd,     // +=
    AssSub,     // -=
    AssMul,     // *=
    AssDiv,     // /=
    AssMod,     // %=
}

pub enum BinOpClass {
    Multiplicative,
    Additive,
    Comparison,
    Assignment,
}

impl BinaryOp {
    pub fn from_token_type(token_type: lexer::TokenType) -> Option<Self> {
        match token_type {
            lexer::TokenType::OpEqualsEquals => Some(Self::CmpEquals),
            lexer::TokenType::OpExlmEquals => Some(Self::CmpNotEquals),
            lexer::TokenType::OpLess => Some(Self::CmpLess),
            lexer::TokenType::OpLessEquals => Some(Self::CmpLessEq),
            lexer::TokenType::OpGreater => Some(Self::CmpGreater),
            lexer::TokenType::OpGreaterEquals => Some(Self::CmpGreaterEq),

            lexer::TokenType::OpPlus => Some(Self::Add),
            lexer::TokenType::OpMinus => Some(Self::Sub),
            lexer::TokenType::OpAsterisk => Some(Self::Mul),
            lexer::TokenType::OpSlash => Some(Self::Div),
            lexer::TokenType::OpPercent => Some(Self::Mod),

            lexer::TokenType::OpEquals => Some(Self::Assignment),
            lexer::TokenType::OpPlusEquals => Some(Self::AssAdd),
            lexer::TokenType::OpMinusEquals => Some(Self::AssSub),
            lexer::TokenType::OpAsteriskEquals => Some(Self::AssMul),
            lexer::TokenType::OpSlashEquals => Some(Self::AssDiv),
            lexer::TokenType::OpPercentEquals => Some(Self::AssMod),

            _ => None,
        }
    }

    pub fn class(&self) -> BinOpClass {
        match self {
            Self::CmpEquals
            | Self::CmpNotEquals
            | Self::CmpLess
            | Self::CmpLessEq
            | Self::CmpGreater
            | Self::CmpGreaterEq => BinOpClass::Comparison,
            Self::Add | Self::Sub => BinOpClass::Additive,
            Self::Mul | Self::Div | Self::Mod => BinOpClass::Multiplicative,
            Self::Assignment
            | Self::AssAdd
            | Self::AssSub
            | Self::AssMul
            | Self::AssDiv
            | Self::AssMod => BinOpClass::Assignment,
        }
    }

    // greater numbers will be evaluated first
    fn precedence(&self) -> u8 {
        match self.class() {
            BinOpClass::Assignment => 1,
            BinOpClass::Comparison => 2,
            BinOpClass::Additive => 3,
            BinOpClass::Multiplicative => 4,
        }
    }

    pub fn mangle(&self) -> &'static str {
        match self {
            Self::CmpEquals => "cmpeq",
            Self::CmpNotEquals => "cmpneq",
            Self::CmpLess => "cmplt",
            Self::CmpLessEq => "cmplte",
            Self::CmpGreater => "cmpgt",
            Self::CmpGreaterEq => "cmpgte",
            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mul => "mul",
            Self::Div => "div",
            Self::Mod => "mod",
            Self::Assignment => "ass",
            Self::AssAdd => "assadd",
            Self::AssSub => "asssub",
            Self::AssMul => "assmul",
            Self::AssDiv => "assdiv",
            Self::AssMod => "assmod",
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Call {
        function: Box<ExpressionNode>,
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
    BinaryOp {
        op: BinaryOp,
        op_range: Range<usize>,
        left: Box<ExpressionNode>,
        right: Box<ExpressionNode>,
    },
    Name(String),
}

#[derive(Debug)]
pub struct ExpressionNode {
    pub expression: Expression,
    pub range: Range<usize>,
}

#[derive(Debug)]
pub enum Statement {
    VarDecl {
        mut_: bool,
        type_: Option<Type>,
        name: String,
        init_value: Option<ExpressionNode>,
    },
    Expression(ExpressionNode),
    Return(Option<ExpressionNode>),
    Block(Vec<Statement>),
    If {
        condition: ExpressionNode,
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

pub struct Parser<'a> {
    input: &'a [u8],
    iter: lexer::TokenIterator<'a>,
    errors: Vec<CompilationError>,
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
                self.errors.push(CompilationError::new(
                    if more_info.is_empty() {
                        "Unexpected token".into()
                    } else {
                        format!("Expected {}", more_info)
                    },
                    next.range.clone(),
                ));
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

    // var-decl ::= "let" name [":" type] "=" expression ";"
    pub fn consume_var_decl(&mut self) -> Option<Statement> {
        // "let"
        let let_or_mut = self.expect(|t| {
            matches!(
                t,
                lexer::TokenType::KeywordLet | lexer::TokenType::KeywordMut
            )
        })?;
        let is_mut = matches!(let_or_mut.type_, lexer::TokenType::KeywordMut);

        // name
        let name = self.expect_msg(|t| matches!(t, lexer::TokenType::Name(_)), "variable name")?;

        // type (optional)
        let type_ = self.iter.clone().next().and_then(|next| {
            if matches!(next.type_, lexer::TokenType::Colon) {
                // ":"
                let _ = self.expect(|t| matches!(t, lexer::TokenType::Colon));

                // type
                Some(self.consume_type()?)
            } else {
                None
            }
        });

        // "="
        let _ = self.expect(|t| matches!(t, lexer::TokenType::OpEquals));

        // expression
        let init_value = self.consume_expression()?;

        // ";"
        let _ = self.expect(|t| matches!(t, lexer::TokenType::Semicolon));

        Some(Statement::VarDecl {
            mut_: is_mut,
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
    ) -> Vec<ExpressionNode> {
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

    pub fn consume_primary_expression(&mut self) -> Option<ExpressionNode> {
        let next = self.iter.next()?;
        match next.type_ {
            lexer::TokenType::Name(name) => Some(ExpressionNode {
                expression: Expression::Name(name),
                range: next.range,
            }),
            lexer::TokenType::Integer(text) => Some(ExpressionNode {
                expression: Expression::IntLiteral { value: text },
                range: next.range,
            }),
            lexer::TokenType::KeywordTrue => Some(ExpressionNode {
                expression: Expression::BoolLiteral { value: true },
                range: next.range,
            }),
            lexer::TokenType::KeywordFalse => Some(ExpressionNode {
                expression: Expression::BoolLiteral { value: false },
                range: next.range,
            }),
            lexer::TokenType::StringLiteral(text) => Some(ExpressionNode {
                expression: Expression::StringLiteral { value: text },
                range: next.range,
            }),
            _ => {
                self.errors.push(CompilationError::new(
                    "Expected expression".into(),
                    next.range.clone(),
                ));
                None
            }
        }
    }

    // [primary expression] [ some postfix operator ]*
    pub fn consume_postfix_expression(&mut self) -> Option<ExpressionNode> {
        let mut primary = self.consume_primary_expression()?;

        loop {
            let next = self.iter.clone().next()?;
            match next.type_ {
                lexer::TokenType::ParenOpen => {
                    let _ = self.iter.next(); // consume "("
                    let args = self.consume_comma_separated_expression_list(|t| {
                        matches!(t, lexer::TokenType::ParenClose)
                    });
                    let end = self.expect(|t| matches!(t, lexer::TokenType::ParenClose))?;
                    primary = ExpressionNode {
                        expression: Expression::Call {
                            function: Box::new(primary),
                            args: args
                                .into_iter()
                                .map(|arg| FunctionArg {
                                    param: None,
                                    value: arg,
                                })
                                .collect(),
                        },
                        range: next.range.start..end.range.end,
                    };
                }
                _ => return Some(primary),
            }
        }
    }

    // [Postfix Operator]* Postfix
    pub fn consume_expression(&mut self) -> Option<ExpressionNode> {
        // Read arguments and (binary) operators (first linearly)
        let mut args: Vec<ExpressionNode> = vec![];
        let mut ops: Vec<(BinaryOp, Range<usize>)> = vec![];

        // First expression
        args.push(self.consume_postfix_expression()?);
        loop {
            // Operator
            let next = self.iter.clone().next()?;
            let op = BinaryOp::from_token_type(next.type_);
            if let Some(op) = op {
                let _ = self.iter.next();
                ops.push((op, next.range));
                args.push(self.consume_postfix_expression()?);
            } else {
                break;
            }
        }

        assert_eq!(ops.len(), args.len() - 1);

        // Then, iteratively build a tree of expression, selecting
        // one with the highest precedence first
        while !ops.is_empty() {
            assert_eq!(ops.len(), args.len() - 1);

            // get operator with max precedence
            let (idx, _) = ops
                .iter()
                .enumerate()
                .max_by_key(|(_, (op, _))| op.precedence())
                .unwrap();

            // remove the operator
            let (op, range) = ops.remove(idx);

            // build a new binary expression
            let left = args.remove(idx);
            let right = args.remove(idx);
            let new_expr = ExpressionNode {
                range: left.range.start..right.range.end,
                expression: Expression::BinaryOp {
                    op,
                    op_range: range,
                    left: Box::new(left),
                    right: Box::new(right),
                },
            };
            args.insert(idx, new_expr);
        }

        args.into_iter().next()
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
            lexer::TokenType::KeywordLet | lexer::TokenType::KeywordMut => self.consume_var_decl(),
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
                self.errors.push(CompilationError::new(
                    "Expected type".into(),
                    next.range.clone(),
                ));
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
                self.errors.push(CompilationError::new(
                    "Expected declaration".into(),
                    next.range.clone(),
                ));
                let _ = self.iter.next(); //whatever
                None
            }
        }
    }

    pub fn parse(mut self) -> (Program, Vec<CompilationError>) {
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

        (Program { declarations }, self.errors)
    }
}
