use core::str;
use std::{collections::HashMap, ops::Range, path::PathBuf, str::Utf8Error};

use crate::{error::CompilationError, lexer, types};

#[derive(Debug)]
pub enum Type {
    Simple(String),
    SizedArray { value: Box<TypeNode>, size: u64 },
}

#[derive(Debug)]
pub struct TypeNode {
    pub type_: Type,
    pub range: Range<usize>,
}

#[derive(Debug, Clone)]
pub struct FunctionArg {
    pub param: Option<String>, // for kwargs
    pub value: ExpressionNode,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    Range, // ..

    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %

    CmpEquals,    // ==
    CmpNotEquals, // !=
    CmpLess,      // <
    CmpLessEq,    // <=
    CmpGreater,   // >
    CmpGreaterEq, // >=

    LogicalAnd, // &&
    LogicalOr,  // ||

    Assignment, // =
    AssAdd,     // +=
    AssSub,     // -=
    AssMul,     // *=
    AssDiv,     // /=
    AssMod,     // %=
}

pub enum BinOpClass {
    Range,
    Multiplicative,
    Additive,
    Comparison,
    Logical,
    Assignment,
}

impl BinaryOp {
    pub fn from_token_type(token_type: lexer::TokenType) -> Option<Self> {
        match token_type {
            lexer::TokenType::OpDotDot => Some(Self::Range),

            lexer::TokenType::OpPlus => Some(Self::Add),
            lexer::TokenType::OpMinus => Some(Self::Sub),
            lexer::TokenType::OpAsterisk => Some(Self::Mul),
            lexer::TokenType::OpSlash => Some(Self::Div),
            lexer::TokenType::OpPercent => Some(Self::Mod),

            lexer::TokenType::OpEqualsEquals => Some(Self::CmpEquals),
            lexer::TokenType::OpExlmEquals => Some(Self::CmpNotEquals),
            lexer::TokenType::OpLess => Some(Self::CmpLess),
            lexer::TokenType::OpLessEquals => Some(Self::CmpLessEq),
            lexer::TokenType::OpGreater => Some(Self::CmpGreater),
            lexer::TokenType::OpGreaterEquals => Some(Self::CmpGreaterEq),

            lexer::TokenType::OpAmpAmp => Some(Self::LogicalAnd),
            lexer::TokenType::OpPipePipe => Some(Self::LogicalOr),

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
            Self::LogicalAnd | Self::LogicalOr => BinOpClass::Logical,
            Self::Range => BinOpClass::Range,
        }
    }

    // greater numbers will be evaluated first
    fn precedence(&self) -> u8 {
        match self.class() {
            BinOpClass::Assignment => 1,
            BinOpClass::Logical => 2,
            BinOpClass::Comparison => 3,
            BinOpClass::Additive => 4,
            BinOpClass::Multiplicative => 5,
            BinOpClass::Range => 6,
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
            Self::Range => "range",
            Self::LogicalAnd => "and",
            Self::LogicalOr => "or",
        }
    }

    pub fn symbol(&self) -> &'static str {
        match self {
            Self::CmpEquals => "==",
            Self::CmpNotEquals => "!=",
            Self::CmpLess => "<",
            Self::CmpLessEq => "<=",
            Self::CmpGreater => ">",
            Self::CmpGreaterEq => ">=",
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
            Self::Assignment => "=",
            Self::AssAdd => "+=",
            Self::AssSub => "-=",
            Self::AssMul => "*=",
            Self::AssDiv => "/=",
            Self::AssMod => "%=",
            Self::Range => "..",
            Self::LogicalAnd => "&&",
            Self::LogicalOr => "||",
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Call {
        function: Box<ExpressionNode>,
        args: Vec<FunctionArg>,
    },
    Index {
        indexable: Box<ExpressionNode>,
        index: Box<ExpressionNode>,
    },
    MemberAccess {
        object: Box<ExpressionNode>,
        member: String,
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
    CharLiteral {
        value: char,
    },
    ArrayLiteral {
        values: Vec<ExpressionNode>,
    },
    StructLiteral {
        struct_name: types::ScopedName,
        fields: HashMap<String, ExpressionNode>,
    },
    BinaryOp {
        op: BinaryOp,
        op_range: Range<usize>,
        left: Box<ExpressionNode>,
        right: Box<ExpressionNode>,
    },
    Name(types::ScopedName),
    This,
    Dereference(Box<ExpressionNode>),
    Reference(Box<ExpressionNode>),
}

#[derive(Debug, Clone)]
pub struct ExpressionNode {
    pub expression: Expression,
    pub range: Range<usize>,
}

#[derive(Debug)]
pub enum Statement {
    VarDecl {
        mut_: bool,
        type_: Option<TypeNode>,
        name: String,
        init_value: Option<ExpressionNode>,
    },
    Expression(ExpressionNode),
    Return(Option<ExpressionNode>),
    Block(Vec<StatementNode>),
    For {
        it_var: String,
        iterable: ExpressionNode,
        body: Box<StatementNode>,
    },
    If {
        condition: ExpressionNode,
        then_block: Box<StatementNode>,
        else_block: Option<Box<StatementNode>>,
    },
    While {
        condition: ExpressionNode,
        body: Box<StatementNode>,
    },
    Break,
    Continue,
}

#[derive(Debug)]
pub struct StatementNode {
    pub statement: Statement,
    pub range: Range<usize>,
}

#[derive(Debug)]
pub struct FunctionParam {
    pub name: String,
    pub type_: TypeNode,
    pub range: Range<usize>,
}

#[derive(Debug)]
pub struct FieldDecl {
    pub name: String,
    pub type_: TypeNode,
    pub range: Range<usize>,
}

// Declarations

#[derive(Debug)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<FunctionParam>,
    pub return_type: Option<TypeNode>,
    pub body: Option<StatementNode>,
}

#[derive(Debug)]
pub struct Import {
    pub name: String,
}

#[derive(Debug)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<FieldDecl>,
    pub methods: Vec<FunctionDecl>,
}

// Module

#[derive(Debug)]
pub struct Module {
    pub source_path: PathBuf,
    pub imports: Vec<Import>,
    pub structs: Vec<Struct>,
    pub functions: Vec<FunctionDecl>,
}

////

pub struct Parser<'a> {
    input: &'a [u8],
    source_path: PathBuf,
    iter: lexer::TokenIterator<'a>,
    errors: Vec<CompilationError>,
}

impl<'a> Parser<'a> {
    pub fn new(source_path: PathBuf, input: &'a [u8]) -> Result<Self, Utf8Error> {
        Ok(Self {
            input,
            source_path,
            iter: lexer::TokenIterator::new(input)?,
            errors: vec![],
        })
    }

    fn peek(&self) -> Option<lexer::Token> {
        self.iter.clone().next()
    }

    fn consume(&mut self) -> Option<lexer::Token> {
        self.iter.next()
    }

    fn next_token_start(&self) -> usize {
        self.peek().map(|t| t.range.start).unwrap_or(0)
    }

    fn next_token_end(&self) -> usize {
        self.peek().map(|t| t.range.end).unwrap_or(0)
    }

    fn path(&self) -> PathBuf {
        self.source_path.clone()
    }

    // Consume next token, and return error if it doesn't match
    // the predicate
    pub fn expect(
        &mut self,
        predicate: impl Fn(&lexer::TokenType) -> bool,
        more_info: &str,
    ) -> Option<lexer::Token> {
        let next = self.peek();
        if let Some(next) = next {
            if predicate(&next.type_) {
                self.consume();
                return Some(next);
            } else {
                self.errors.push(CompilationError::new(
                    if more_info.is_empty() {
                        "Unexpected token".into()
                    } else {
                        format!("Expected {}", more_info)
                    },
                    next.range.clone(),
                    self.path(),
                ));
                None
            }
        } else {
            self.errors.push(CompilationError::new(
                if more_info.is_empty() {
                    "Unexpected EOF".into()
                } else {
                    format!("Expected {}", more_info)
                },
                self.input.len()..self.input.len() + 1,
                self.path(),
            ));
            None
        }
    }

    pub fn expect_no_msg(
        &mut self,
        predicate: impl Fn(&lexer::TokenType) -> bool,
    ) -> Option<lexer::Token> {
        self.expect(predicate, "")
    }

    pub fn expect_token(&mut self, expected: &lexer::TokenType) -> Option<lexer::Token> {
        self.expect(|t| t == expected, format!("{}", expected).as_str())
    }

    // var-decl ::= "let" name [":" type] "=" expression ";"
    pub fn consume_var_decl(&mut self) -> Option<StatementNode> {
        // "let"
        let let_or_mut = self.expect_no_msg(|t| {
            matches!(
                t,
                lexer::TokenType::KeywordLet | lexer::TokenType::KeywordMut
            )
        })?;
        let is_mut = matches!(let_or_mut.type_, lexer::TokenType::KeywordMut);

        // name
        let name = self.expect(|t| matches!(t, lexer::TokenType::Name(_)), "variable name")?;

        // type (optional)
        let type_ = self.peek().and_then(|next| {
            if matches!(next.type_, lexer::TokenType::Colon) {
                // ":"
                let _ = self.expect_token(&lexer::TokenType::Colon);

                // type
                Some(self.consume_type()?)
            } else {
                None
            }
        });

        // "="
        let _ = self.expect_token(&lexer::TokenType::OpEquals);

        // expression
        let init_value = self.consume_expression()?;

        // ";"
        let sem = self.expect_token(&lexer::TokenType::Semicolon)?;

        Some(StatementNode {
            statement: Statement::VarDecl {
                mut_: is_mut,
                name: name.slice_str(&self.input).unwrap().into(),
                type_,
                init_value: Some(init_value),
            },
            range: let_or_mut.range.start..sem.range.end,
        })
    }

    pub fn consume_block(&mut self) -> StatementNode {
        let start = self.next_token_start();

        // "{"
        let _ = self.expect_token(&lexer::TokenType::CurlyOpen);

        // statements
        let mut statements = vec![];
        loop {
            let next = self.peek();
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
        let end = self.next_token_end();
        let _ = self.expect(|t| matches!(t, lexer::TokenType::CurlyClose), "'}'");

        StatementNode {
            statement: Statement::Block(statements),
            range: start..end,
        }
    }

    // `(expression ",")* expression ","?` delimited by token matched by `is_end` (it is not consumed)
    // note: trailing "," IS allowed here
    pub fn consume_comma_separated_expression_list(
        &mut self,
        is_end: impl Fn(lexer::TokenType) -> bool,
    ) -> Vec<ExpressionNode> {
        let mut expressions = vec![];
        if let Some(a) = self.peek() {
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
                if let Some(a) = self.peek() {
                    a
                } else {
                    self.errors.push(CompilationError::new(
                        "EOF while reading expression list".into(),
                        self.input.len()..self.input.len() + 1,
                        self.path(),
                    ));
                    break;
                }
            };
            if is_end(next.type_) {
                break;
            } else {
                let _ = self.expect_token(&lexer::TokenType::Comma);
            }
        }
        expressions
    }

    // { field: value, ... }
    pub fn consume_struct_literal_without_name(
        &mut self,
        struct_name: types::ScopedName,
    ) -> Option<ExpressionNode> {
        // expect {
        let start_token = self.expect_token(&lexer::TokenType::CurlyOpen)?;
        let start = start_token.range.start;
        let mut fields: HashMap<String, ExpressionNode> = HashMap::new();
        loop {
            let next = self.peek()?;
            if matches!(next.type_, lexer::TokenType::CurlyClose) {
                break;
            }

            // field name
            let field_name_token =
                self.expect(|t| matches!(t, lexer::TokenType::Name(_)), "field name")?;
            let field_name = field_name_token.slice_str(&self.input).unwrap().into();

            // ":"
            let _ = self.expect_token(&lexer::TokenType::Colon);

            // value
            let value = self.consume_expression()?;

            fields.insert(field_name, value);

            let next = self.peek()?;
            if matches!(next.type_, lexer::TokenType::CurlyClose) {
                break;
            } else {
                let _ = self.expect_token(&lexer::TokenType::Comma);
            }
        }
        let end_token = self.expect_token(&lexer::TokenType::CurlyClose)?;
        let end = end_token.range.end;
        Some(ExpressionNode {
            expression: Expression::StructLiteral {
                struct_name,
                fields,
            },
            range: start..end,
        })
    }

    pub fn consume_primary_expression(&mut self) -> Option<ExpressionNode> {
        let next = self.consume()?;
        match next.type_ {
            lexer::TokenType::ParenOpen => {
                let in_expr = self.consume_expression();
                let _ = self.expect(|t| matches!(t, lexer::TokenType::ParenClose), "')'")?;
                in_expr
            }
            lexer::TokenType::BraceOpen => {
                let values = self.consume_comma_separated_expression_list(|t| {
                    matches!(t, lexer::TokenType::BraceClose)
                });
                let end = self.expect(
                    |t| matches!(t, lexer::TokenType::BraceClose),
                    "']' after array literal",
                )?;
                Some(ExpressionNode {
                    expression: Expression::ArrayLiteral { values },
                    range: next.range.start..end.range.end,
                })
            }
            lexer::TokenType::Name(name) => {
                let mut components = vec![name.into()];
                // Maybe there are more components (::)
                loop {
                    let next = self.peek();
                    if let Some(next) = next {
                        if matches!(next.type_, lexer::TokenType::ColonColon) {
                            let _ = self.consume();
                            let next = self.expect(
                                |t| matches!(t, lexer::TokenType::Name(_)),
                                "name after '::'",
                            )?;
                            components.push(next.slice_str(&self.input).unwrap().into());
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }

                let scoped_name = types::ScopedName::new(components);

                // Maybe this is a struct literal ({ field: value, ... })
                let maybe_curly = self.peek();
                if let Some(lexer::Token {
                    type_: lexer::TokenType::CurlyOpen,
                    ..
                }) = maybe_curly
                {
                    self.consume_struct_literal_without_name(scoped_name)
                } else {
                    Some(ExpressionNode {
                        expression: Expression::Name(scoped_name),
                        range: next.range,
                    })
                }
            }
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
            lexer::TokenType::KeywordThis => Some(ExpressionNode {
                expression: Expression::This,
                range: next.range,
            }),
            lexer::TokenType::StringLiteral(text) => Some(ExpressionNode {
                expression: Expression::StringLiteral { value: text },
                range: next.range,
            }),
            lexer::TokenType::CharLiteral(text) => {
                if text.len() != 1 {
                    self.errors.push(CompilationError::new(
                        "Char literal must be exactly one character long".into(),
                        next.range.clone(),
                        self.path(),
                    ));
                    return None;
                }
                Some(ExpressionNode {
                    expression: Expression::CharLiteral {
                        value: text.chars().next().unwrap(),
                    },
                    range: next.range,
                })
            }
            _ => {
                self.errors.push(CompilationError::new(
                    "Expected expression".into(),
                    next.range.clone(),
                    self.path(),
                ));
                None
            }
        }
    }

    // [primary expression] [ some postfix operator ]*
    pub fn consume_postfix_expression(&mut self) -> Option<ExpressionNode> {
        let mut primary = self.consume_primary_expression()?;

        loop {
            let next = self.peek()?;

            match next.type_ {
                lexer::TokenType::ParenOpen => {
                    let _ = self.consume(); // consume "("
                    let args = self.consume_comma_separated_expression_list(|t| {
                        matches!(t, lexer::TokenType::ParenClose)
                    });
                    let end = self.expect(
                        |t| matches!(t, lexer::TokenType::ParenClose),
                        "')' after argument list",
                    )?;
                    primary = ExpressionNode {
                        range: primary.range.start..end.range.end,
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
                    };
                }
                lexer::TokenType::BraceOpen => {
                    let _ = self.consume(); // consume "["
                    let index = self.consume_expression()?;
                    let end = self.expect(
                        |t| matches!(t, lexer::TokenType::BraceClose),
                        "']' after index",
                    )?;
                    primary = ExpressionNode {
                        range: primary.range.start..end.range.end,
                        expression: Expression::Index {
                            indexable: Box::new(primary),
                            index: Box::new(index),
                        },
                    };
                }
                lexer::TokenType::OpDot => {
                    // Member access: `.name`
                    let _ = self.consume(); // consume "."
                    let end = self.next_token_end();
                    let lexer::TokenType::Name(name) = self
                        .expect(|t| matches!(t, lexer::TokenType::Name(_)), "member name")?
                        .type_
                    else {
                        unreachable!();
                    };
                    primary = ExpressionNode {
                        range: primary.range.start..end,
                        expression: Expression::MemberAccess {
                            object: Box::new(primary),
                            member: name.into(),
                        },
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
            let next = self.peek()?;
            let op = BinaryOp::from_token_type(next.type_);
            if let Some(op) = op {
                let _ = self.consume();
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

            // get operator with max precedence (if equal, take the first one == left associative)
            let (idx, _) =
                ops.iter()
                    .enumerate()
                    .fold((0, 0), |(max_idx, max_prec), (idx, (op, _))| {
                        let prec = op.precedence();
                        if prec > max_prec {
                            (idx, prec)
                        } else {
                            (max_idx, max_prec)
                        }
                    });

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

    pub fn consume_expression_statement(&mut self) -> Option<StatementNode> {
        let expr = self.consume_expression()?;
        let end = self.next_token_end();
        let _ = self.expect(
            |t| matches!(t, lexer::TokenType::Semicolon),
            "';' after expression statement",
        );
        Some(StatementNode {
            range: expr.range.start..end,
            statement: Statement::Expression(expr),
        })
    }

    pub fn consume_return_statement(&mut self) -> Option<StatementNode> {
        let start = self.peek()?.range.start;

        // "return"
        let _ = self.expect_token(&lexer::TokenType::KeywordReturn);

        // early ';' for "return;"
        if let Some(next) = self.peek() {
            if matches!(next.type_, lexer::TokenType::Semicolon) {
                let end = self.consume().unwrap().range.end;
                return Some(StatementNode {
                    statement: Statement::Return(None),
                    range: start..end,
                });
            }
        }

        // expression
        let expr = self.consume_expression()?;

        // ";"
        let end = self.next_token_end();
        let _ = self.expect(
            |t| matches!(t, lexer::TokenType::Semicolon),
            "';' after return",
        );

        return Some(StatementNode {
            statement: Statement::Return(Some(expr)),
            range: start..end,
        });
    }

    // for-statement ::= "for" "(" "let" var-name "of" expression ")" block
    pub fn consume_for_statement(&mut self) -> Option<StatementNode> {
        // "for"
        let start = self
            .expect_no_msg(|t| matches!(t, lexer::TokenType::KeywordFor))?
            .range
            .start;

        // "("
        let _ = self.expect_token(&lexer::TokenType::ParenOpen);

        // "let"
        // TODO: distinguish let/mut
        let _ = self.expect_token(&lexer::TokenType::KeywordLet);

        // var name
        let it_var = self.expect(
            |t| matches!(t, lexer::TokenType::Name(_)),
            "iterator variable",
        )?;

        // "of"
        let _ = self.expect_token(&lexer::TokenType::KeywordOf);

        // iterable
        let iterable = self.consume_expression()?;

        // ")"
        let _ = self.expect_token(&lexer::TokenType::ParenClose);

        // block
        let body = self.consume_block();
        let end = body.range.end;

        Some(StatementNode {
            statement: Statement::For {
                it_var: it_var.slice_str(&self.input).unwrap().into(),
                iterable,
                body: Box::new(body),
            },
            range: start..end,
        })
    }

    // if-statement ::= "if" "(" expression ")" block [ "else" block ]
    pub fn consume_if_statement(&mut self) -> Option<StatementNode> {
        // "if"
        let start = self
            .expect_no_msg(|t| matches!(t, lexer::TokenType::KeywordIf))?
            .range
            .start;

        // "("
        let _ = self.expect_token(&lexer::TokenType::ParenOpen)?;

        // expression
        let condition = self.consume_expression()?;

        // ")"
        let _ = self.expect(|t| matches!(t, lexer::TokenType::ParenClose), "')'")?;

        // block
        let then_block = self.consume_block();

        // "else"
        let else_block = if let Some(next) = self.peek() {
            if matches!(next.type_, lexer::TokenType::KeywordElse) {
                let _ = self.consume(); // "else"

                // next might be another 'if' or a (final) block
                let next = self.peek()?;
                match next.type_ {
                    lexer::TokenType::KeywordIf => {
                        let else_block = self.consume_if_statement()?;
                        Some(else_block)
                    }
                    lexer::TokenType::CurlyOpen => {
                        let else_block = self.consume_block();
                        Some(else_block)
                    }
                    _ => {
                        self.errors.push(CompilationError::new(
                            "Expected 'else' block or another 'if'".into(),
                            next.range.clone(),
                            self.path(),
                        ));
                        None
                    }
                }
            } else {
                None
            }
        } else {
            None
        };

        let end = else_block
            .as_ref()
            .map(|b| b.range.end)
            .unwrap_or(then_block.range.end);

        Some(StatementNode {
            statement: Statement::If {
                condition,
                then_block: Box::new(then_block),
                else_block: else_block.map(Box::new),
            },
            range: start..end,
        })
    }

    // while-statement ::= "while" "(" expression ")" block
    pub fn consume_while_statement(&mut self) -> Option<StatementNode> {
        // "while"
        let start = self
            .expect_no_msg(|t| matches!(t, lexer::TokenType::KeywordWhile))?
            .range
            .start;

        // "("
        let _ = self.expect_token(&lexer::TokenType::ParenOpen)?;

        // expression
        let condition = self.consume_expression()?;

        // ")"
        let _ = self.expect(|t| matches!(t, lexer::TokenType::ParenClose), "')'")?;

        // block
        let body = self.consume_block();
        let end = body.range.end;

        Some(StatementNode {
            statement: Statement::While {
                condition,
                body: Box::new(body),
            },
            range: start..end,
        })
    }

    // statement ::= var-decl | expression ';' | block | return-statement | if-statement
    pub fn consume_statement(&mut self) -> Option<StatementNode> {
        let next = self.peek()?;
        match next.type_ {
            lexer::TokenType::CurlyClose => panic!("somebody didn't consume '}}'"),
            lexer::TokenType::CurlyOpen => Some(self.consume_block()),
            lexer::TokenType::KeywordBreak => {
                let range = self.consume()?.range;
                self.expect(|t| matches!(t, lexer::TokenType::Semicolon), "';'");
                Some(StatementNode {
                    statement: Statement::Break,
                    range,
                })
            }
            lexer::TokenType::KeywordContinue => {
                let range = self.consume()?.range;
                self.expect(|t| matches!(t, lexer::TokenType::Semicolon), "';'");
                Some(StatementNode {
                    statement: Statement::Continue,
                    range,
                })
            }
            lexer::TokenType::KeywordFor => self.consume_for_statement(),
            lexer::TokenType::KeywordIf => self.consume_if_statement(),
            lexer::TokenType::KeywordLet | lexer::TokenType::KeywordMut => self.consume_var_decl(),
            lexer::TokenType::KeywordReturn => self.consume_return_statement(),
            lexer::TokenType::KeywordWhile => self.consume_while_statement(),
            _ => self.consume_expression_statement(),
        }
    }

    pub fn consume_type(&mut self) -> Option<TypeNode> {
        let next = self.peek()?;
        match next.type_ {
            lexer::TokenType::BraceOpen => {
                // array ::= [size]type
                let _ = self.consume();
                let lexer::TokenType::Integer(size) = self
                    .expect(|t| matches!(t, lexer::TokenType::Integer(_)), "array size")?
                    .type_
                else {
                    unreachable!();
                };
                let _ = self.expect(|t| matches!(t, lexer::TokenType::BraceClose), "']'");
                let value = Box::new(self.consume_type()?);
                Some(TypeNode {
                    range: next.range.start..value.range.end,
                    type_: Type::SizedArray { value, size },
                })
            }
            lexer::TokenType::Name(name) => {
                let _ = self.consume();
                Some(TypeNode {
                    type_: Type::Simple(name),
                    range: next.range,
                })
            }
            _ => {
                self.errors.push(CompilationError::new(
                    "Expected type".into(),
                    next.range.clone(),
                    self.path(),
                ));
                let _ = self.consume(); //whatever
                None
            }
        }
    }

    // function-impl ::= "func" name "(" ")" [ ":" type ] "{" statement ... "}"
    // function-impl ::= "extern" "func" name "(" ")" [ ":" type ] ";" // no body
    pub fn consume_function_decl(&mut self) -> Option<FunctionDecl> {
        // maybe "extern"
        let extern_;
        if let Some(next) = self.peek() {
            if matches!(next.type_, lexer::TokenType::KeywordExtern) {
                let _ = self.consume();
                extern_ = true;
            } else {
                extern_ = false;
            }
        } else {
            extern_ = false;
        }

        // "func"
        let _ = self.consume();

        // name
        let name = self.expect(|t| matches!(t, lexer::TokenType::Name(_)), "function name")?;

        // "("
        let _ = self.expect_token(&lexer::TokenType::ParenOpen)?;

        // params
        let mut params = vec![];
        loop {
            // if next is ")", stop
            if let Some(next) = self.peek() {
                match next.type_ {
                    lexer::TokenType::ParenClose => {
                        let _ = self.consume();
                        break;
                    }
                    lexer::TokenType::KeywordThis => {
                        let _ = self.consume();
                        params.push(FunctionParam {
                            name: "this".into(),
                            type_: TypeNode {
                                type_: Type::Simple("Self".into()),
                                range: next.range.clone(),
                            },
                            range: next.range.clone(),
                        });
                    }
                    lexer::TokenType::Name(name) => {
                        let _ = self.consume();
                        let _ = self.expect_token(&lexer::TokenType::Colon);
                        let type_ = self.consume_type()?;
                        params.push(FunctionParam {
                            name: name.into(),
                            type_,
                            range: next.range.clone(),
                        });
                    }
                    _ => {
                        self.errors.push(CompilationError::new(
                            "Expected parameter name or ')'".into(),
                            next.range.clone(),
                            self.path(),
                        ));
                        let _ = self.consume(); //whatever
                        continue;
                    }
                }

                let next = self.peek()?;
                match next.type_ {
                    lexer::TokenType::Comma => {
                        let _ = self.consume();
                    }
                    lexer::TokenType::ParenClose => {}
                    _ => {
                        self.errors.push(CompilationError::new(
                            "Expected ',' or ')'".into(),
                            next.range.clone(),
                            self.path(),
                        ));
                        let _ = self.consume(); //whatever
                    }
                }
            } else {
                break;
            }
        }

        // return type
        let colon = self
            .iter
            .clone()
            .next()
            .filter(|t| matches!(t.type_, lexer::TokenType::Colon));
        let return_type = if let Some(_) = colon {
            let _ = self.consume();
            self.consume_type()
        } else {
            None
        };

        if extern_ {
            let _ = self.expect_token(&lexer::TokenType::Semicolon);
            Some(FunctionDecl {
                name: name.slice_str(&self.input).unwrap().into(),
                params,
                return_type,
                body: None,
            })
        } else {
            let body = self.consume_block();
            Some(FunctionDecl {
                name: name.slice_str(&self.input).unwrap().into(),
                params,
                return_type,
                body: Some(body),
            })
        }
    }

    // import-decl ::= "import" name
    pub fn consume_import_decl(&mut self) -> Option<Import> {
        // "import"
        let _ = self.consume();

        // name
        let name = self.expect(|t| matches!(t, lexer::TokenType::Name(_)), "import name")?;

        // ';'
        let _ = self.expect_token(&lexer::TokenType::Semicolon);

        Some(Import {
            name: name.slice_str(&self.input).unwrap().into(),
        })
    }

    // struct-decl ::= "struct" name "{" [name: type]* "}"
    pub fn consume_struct_decl(&mut self) -> Option<Struct> {
        // "struct"
        let _ = self.consume();

        // name
        let name = self.expect(|t| matches!(t, lexer::TokenType::Name(_)), "struct name")?;

        // "{"
        let _ = self.expect_token(&lexer::TokenType::CurlyOpen);

        // fields / methods
        let mut fields = vec![];
        let mut methods = vec![];
        loop {
            let next = self.peek();
            if let Some(next) = &next {
                if matches!(next.type_, lexer::TokenType::CurlyClose) {
                    break;
                }

                // maybe method?
                if matches!(next.type_, lexer::TokenType::KeywordFunc) {
                    let method = self.consume_function_decl()?;
                    methods.push(method);
                    continue;
                }
            } else {
                break;
            }

            // field name
            let name = self.expect(
                |t| matches!(t, lexer::TokenType::Name(_)),
                "field or method declaration",
            )?;

            // ":"
            let _ = self.expect_token(&lexer::TokenType::Colon);

            // type
            let type_ = self.consume_type()?;

            // ";"
            let _ = self.expect(|t| matches!(t, lexer::TokenType::Semicolon), "';'");

            fields.push(FieldDecl {
                name: name.slice_str(&self.input).unwrap().into(),
                type_,
                range: name.range.clone(),
            });
        }

        // "}"
        let _ = self.expect_token(&lexer::TokenType::CurlyClose);

        Some(Struct {
            name: name.slice_str(&self.input).unwrap().into(),
            fields,
            methods,
        })
    }

    pub fn consume_declaration(&mut self) {}

    pub fn parse(mut self) -> (Module, Vec<CompilationError>) {
        let mut imports = vec![];
        let mut structs = vec![];
        let mut functions = vec![];
        loop {
            let Some(next) = self.peek() else {
                break;
            };
            match next.type_ {
                lexer::TokenType::KeywordImport => {
                    if let Some(a) = self.consume_import_decl() {
                        imports.push(a);
                    }
                }
                lexer::TokenType::KeywordStruct => {
                    if let Some(a) = self.consume_struct_decl() {
                        structs.push(a);
                    }
                }
                lexer::TokenType::KeywordFunc | lexer::TokenType::KeywordExtern => {
                    if let Some(a) = self.consume_function_decl() {
                        functions.push(a);
                    }
                }
                _ => {
                    self.errors.push(CompilationError::new(
                        "Expected declaration".into(),
                        next.range.clone(),
                        self.path(),
                    ));
                    let _ = self.consume(); //whatever
                }
            }
        }

        (
            Module {
                source_path: self.source_path,
                imports,
                structs,
                functions,
            },
            self.errors,
        )
    }
}
