pub enum Type {
    Simple(String),
}

pub struct FunctionArg {
    pub param: Option<String>, // for kwargs
    pub value: Expression,
}

pub enum Expression {
    Call {
        function: String, // TODO: arbitrary expressions
        args: Vec<FunctionArg>,
    },
    StringLiteral {
        value: String,
    },
}

pub enum Statement {
    Expression(Expression),
    Block(Vec<Statement>),
}

pub enum Declaration {
    FunctionImpl {
        name: String,
        return_type: Type,
        body: Statement,
    },
}

pub struct Program {
    pub declarations: Vec<Declaration>,
}
