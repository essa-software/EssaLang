use log::debug;

use crate::sema::*;

pub const RANGE_BEGIN: &str = "_begin";
pub const RANGE_END: &str = "_end";

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Call {
        function_id: Option<FunctionId>,
        arguments: HashMap<VarId, Expression>,
    },
    Index {
        indexable: Box<Expression>,
        index: Box<Expression>,
    },
    MemberAccess {
        object: Box<Expression>,
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
        value_type: Option<Type>,
        values: Vec<Expression>,
    },
    StructLiteral {
        struct_id: Option<StructId>,
        fields: HashMap<String, Expression>,
    },
    VarRef(Option<VarId>),
    BinaryOp {
        op: parser::BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Dereference {
        pointer: Box<Expression>,
    },
    Reference {
        value: Box<Expression>,
    },
    /// No-op that is required to make typechecker believe the `expr`
    /// is of type `to`. Not accessible to user code and potentially
    /// unsafe!
    ImplicitCast {
        to: Type,
        expr: Box<Expression>,
    },
}

#[derive(PartialEq, Eq)]
pub enum ValueType {
    LValue,      // can be read and written to directly
    ConstLValue, // can be read directly but not written to
    RValue,      // needs to be evaluated
}

impl Expression {
    pub fn type_(&self, program: &Program) -> Option<Type> {
        match self {
            Expression::Call { function_id, .. } => {
                program.get_function((*function_id)?).return_type.clone()
            }
            Expression::Index {
                indexable,
                index: _,
            } => {
                let indexable_type = indexable.type_(program)?;
                indexable_type.index_value_type(program)
            }
            Expression::MemberAccess { object, member } => {
                let object_type = object.type_(program)?;
                debug!(
                    "Member access on on object of type {:?}: '{}'",
                    object_type, member
                );
                match object_type {
                    Type::Struct { id } => {
                        let struct_ = program.get_struct(id);
                        struct_.resolve_field(member).and_then(|f| f.type_.clone())
                    }
                    Type::Primitive(Primitive::Range) => {
                        // Hacky special-case: range.begin, range.end
                        let tp = match member.as_str() {
                            RANGE_BEGIN | RANGE_END => Some(Type::Primitive(Primitive::USize)),
                            _ => None,
                        };
                        tp
                    }
                    _ => None,
                }
            }
            Expression::BoolLiteral { .. } => Some(Type::Primitive(Primitive::Bool)),
            Expression::IntLiteral { .. } => Some(Type::Primitive(Primitive::LiteralInt)),
            Expression::StringLiteral { .. } => Some(Type::Primitive(Primitive::StaticString)),
            Expression::VarRef(var_id) => var_id.and_then(|id| program.get_var(id).type_.clone()),
            Expression::BinaryOp { op, left, right } => match op.class() {
                BinOpClass::Comparison => Some(Type::Primitive(Primitive::Bool)),
                BinOpClass::Assignment => Some(Type::Primitive(Primitive::Void)),
                BinOpClass::Additive | BinOpClass::Multiplicative => {
                    // if one of left/right is literal int, return the other type
                    let left_type = left.type_(program)?;
                    let right_type = right.type_(program)?;
                    if left_type == Type::Primitive(Primitive::LiteralInt) {
                        Some(right_type)
                    } else if right_type == Type::Primitive(Primitive::LiteralInt) {
                        Some(left_type)
                    } else if left_type == right_type {
                        Some(left_type)
                    } else {
                        None
                    }
                }
                BinOpClass::Range => Some(Type::Primitive(Primitive::Range)),
                BinOpClass::Logical => Some(Type::Primitive(Primitive::Bool)),
            },
            Expression::ArrayLiteral { value_type, values } => {
                if values.is_empty() {
                    Some(Type::Primitive(Primitive::EmptyArray))
                } else {
                    Some(Type::Array {
                        inner: Box::new(value_type.clone()?),
                        size: values.len(),
                    })
                }
            }
            Expression::StructLiteral { struct_id, .. } => struct_id.map(|id| Type::Struct { id }),
            Expression::CharLiteral { value: _ } => Some(Type::Primitive(Primitive::Char)),
            Expression::Dereference { pointer } => {
                let pointer_type = pointer.type_(program)?;
                match pointer_type {
                    Type::RawReference { inner } => Some(*inner),
                    _ => None,
                }
            }
            Expression::Reference { value } => {
                let value_type = value.type_(program)?;
                Some(Type::RawReference {
                    inner: Box::new(value_type),
                })
            }
            Expression::ImplicitCast { to, expr: _ } => Some(to.clone()),
        }
    }

    /// Some expressions return general types which must not appear
    /// in codegen e.g LiteralInt. If we have some hint for conversion
    /// we can just use `type_()`. However there are some cases (e.g var
    /// decl without explicit type) where we don't have such hint, we
    /// then have to know _some_ specific repr type to use.
    ///
    /// This function allows this by always returning a codegen-
    /// representable type.
    ///
    /// The return value:
    /// - is Some if type_() is Some
    /// - type_() is always convertible to default_repr_type()
    pub fn default_repr_type(&self, program: &Program) -> Option<Type> {
        match self {
            Expression::IntLiteral { .. } => Some(Type::Primitive(Primitive::U32)),
            _ => self.type_(program),
        }
    }

    pub fn value_type(&self, program: &Program) -> ValueType {
        match self {
            Expression::VarRef(var_id) => var_id
                .map(|id| {
                    if program.get_var(id).mut_ {
                        ValueType::LValue
                    } else {
                        ValueType::ConstLValue
                    }
                })
                .unwrap_or(ValueType::LValue),
            Expression::Index {
                indexable,
                index: _,
            } => indexable.value_type(program),
            Expression::MemberAccess { object, member: _ } => object.value_type(program),
            Expression::Dereference { pointer } => pointer.value_type(program),
            _ => ValueType::RValue,
        }
    }

    pub fn can_be_discarded(&self, program: &Program) -> bool {
        match self {
            Expression::Call {
                function_id,
                arguments: _,
            } => match function_id {
                // only functions returning void
                Some(fid) => {
                    matches!(
                        {
                            if let Some(rt) = &program.get_function(*fid).return_type {
                                rt
                            } else {
                                return true;
                            }
                        },
                        Type::Primitive(Primitive::Void)
                    )
                }
                None => true,
            },
            Expression::BinaryOp {
                op,
                left: _,
                right: _,
            } if matches!(op.class(), BinOpClass::Assignment) => true,
            _ => false,
        }
    }
}
