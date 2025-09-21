use crate::sema::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Primitive {
    Void,
    /// Type of IntLiteral, implicitly convertible to any integer type
    LiteralInt,
    U32,
    USize,
    Bool,
    Char,
    String,
    StaticString,
    Range,
    EmptyArray,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Primitive(Primitive),
    Array {
        inner: Box<Type>,
        size: usize,
    },
    Slice {
        inner: Box<Type>,
        mut_elements: bool,
    },
    Struct {
        id: StructId,
    },
    RawReference {
        inner: Box<Type>,
    },
    Rc {
        inner: Box<Type>,
    },
}

impl Type {
    pub fn name(&self, program: &Program) -> String {
        match self {
            Type::Primitive(Primitive::Void) => "void".into(),
            Type::Primitive(Primitive::LiteralInt) => "<literal int>".into(),
            Type::Primitive(Primitive::U32) => "u32".into(),
            Type::Primitive(Primitive::USize) => "usize".into(),
            Type::Primitive(Primitive::Bool) => "bool".into(),
            Type::Primitive(Primitive::Char) => "char".into(),
            Type::Primitive(Primitive::String) => "string".into(),
            Type::Primitive(Primitive::StaticString) => "static_string".into(),
            Type::Primitive(Primitive::Range) => "range".into(),
            Type::Primitive(Primitive::EmptyArray) => "<empty array>".into(),
            Type::Array { inner, size } => {
                format!("[{}]{}", size, inner.name(program))
            }
            Type::Slice {
                inner,
                mut_elements,
            } => format!(
                "[]{}{}",
                if *mut_elements { "mut " } else { "" },
                inner.name(program)
            ),
            Type::Struct { id } => program.get_struct(*id).name.clone(),
            Type::RawReference { inner } => format!("raw &{}", inner.name(program)),
            Type::Rc { inner } => format!("&mut {}", inner.name(program)),
        }
    }

    pub fn mangle(&self, program: &Program) -> String {
        match self {
            Type::Primitive(Primitive::Void) => "void".into(),
            Type::Primitive(Primitive::LiteralInt) => "literal_int".into(),
            Type::Primitive(Primitive::U32) => "u32".into(),
            Type::Primitive(Primitive::USize) => "usize".into(),
            Type::Primitive(Primitive::Bool) => "bool".into(),
            Type::Primitive(Primitive::Char) => "char".into(),
            Type::Primitive(Primitive::String) => "string".into(),
            Type::Primitive(Primitive::StaticString) => "static_string".into(),
            Type::Primitive(Primitive::Range) => "range".into(),
            Type::Primitive(Primitive::EmptyArray) => "empty_array".into(),
            Type::Array { inner, size } => {
                format!("array_{}_{}", size, inner.mangle(program))
            }
            Type::Slice {
                inner,
                mut_elements,
            } => format!(
                "slice_{}_{}",
                if *mut_elements { "mut" } else { "const" },
                inner.mangle(program)
            ),
            Type::Struct { id } => {
                let struct_ = program.get_struct(*id);
                if struct_.is_extern {
                    struct_.name.clone()
                } else {
                    format!("struct_{}", struct_.name)
                }
            }
            Type::RawReference { inner } => format!("ref_{}", inner.mangle(program)),
            Type::Rc { inner } => format!("rc_{}", inner.mangle(program)),
        }
    }

    pub fn iter_value_type(&self, _program: &Program) -> Option<Type> {
        match self {
            Type::Primitive(Primitive::Range) => Some(Type::Primitive(Primitive::USize)),
            Type::Primitive(Primitive::StaticString) => Some(Type::Primitive(Primitive::Char)),
            Type::Array { inner, .. } => Some(*inner.clone()),
            _ => None,
        }
    }

    pub fn index_value_type(&self, _program: &Program) -> Option<Type> {
        // Currently all iterable types are also indexable.
        self.iter_value_type(_program)
    }

    pub fn is_copyable(&self, program: &Program) -> bool {
        match self {
            // All primitives (except string) are copyable
            Type::Primitive(p) => {
                if *p == Primitive::String {
                    return false;
                } else {
                    return true;
                }
            }
            // All structs (except ones with non-copyable fields or containing drop method) are copyable
            Type::Struct { id } => {
                let struct_ = program.get_struct(*id);
                if struct_.drop_method.is_some() {
                    return false;
                }
                for field in &struct_.fields {
                    if let Some(field_type) = &field.type_ {
                        if !field_type.is_copyable(program) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
                return true;
            }
            // All references are copyable
            Type::RawReference { .. } => true,
            // All Rc's are copyable - will increase refcount
            Type::Rc { .. } => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Type::Primitive(Primitive::U32) | Type::Primitive(Primitive::USize)
        )
    }
}
