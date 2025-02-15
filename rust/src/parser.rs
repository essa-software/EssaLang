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
    Name(String),
}

pub enum Statement {
    VarDecl {
        mut_: bool,
        type_: Type,
        name: String,
        init_value: Option<Expression>,
    },
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
