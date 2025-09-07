use crate::sema::*;

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    Block(Vec<Statement>),
    VarDecl {
        var: VarId,
        init: Option<Expression>,
    },
    Return(Option<Expression>),
    If {
        condition: Expression,
        then_block: Box<Statement>,
        else_block: Option<Box<Statement>>,
    },
    Loop(Box<Statement>),
    Break,
    Continue,
}
