use crate::sema::*;

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    Block(Vec<Statement>),
    VarDecl {
        var: VarId,
        init: Option<Expression>,
    },
    Return(Expression),
    If {
        condition: Expression,
        then_block: Box<Statement>,
        else_block: Option<Box<Statement>>,
    },
    Loop(Box<Statement>),
    Break,
    Continue,
}

impl Statement {
    /// returns the first declared return type for a statement, or None
    /// if statement doesn't always return (i.e it falls through
    /// to a next statement)
    pub fn return_type(&self, program: &Program) -> Option<Type> {
        match self {
            Statement::Expression(_) => None,
            Statement::Block(statements) => {
                // block always returns iff any of the substatements always returns
                for statement in statements.iter().rev() {
                    if let Some(t) = statement.return_type(program) {
                        return Some(t);
                    }
                }
                None
            }
            Statement::VarDecl { var: _, init: _ } => None,
            Statement::Return(expression) => expression.type_(program),
            Statement::If {
                condition: _,
                then_block,
                else_block,
            } => {
                // `if` always returns iff both branches always return
                if let Some(else_block) = else_block {
                    let then_type = then_block.return_type(program);
                    let else_type = else_block.return_type(program);
                    if then_type.is_some() && else_type.is_some() {
                        then_type
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Statement::Loop(_) => {
                // for now, loop never always returns
                // but FIXME, in some cases it may be possible for a loop
                // to always return, e.g if we can prove it will run
                // at least once and the body always returns
                return None;
            }
            Statement::Break => None,
            Statement::Continue => None,
        }
    }
}
