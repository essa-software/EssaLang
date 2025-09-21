use crate::sema::*;

#[derive(Debug)]
pub struct Field {
    pub type_: Option<Type>,
    pub name: String,
}

#[derive(Debug)]
pub struct Struct {
    pub id: Option<StructId>,
    // Decl scope the struct is in
    pub parent_decl_scope: DeclScopeId,
    // Decl scope the struct's methods and nested structs are in
    pub struct_decl_scope: DeclScopeId,
    pub is_extern: bool,
    pub name: String,
    pub fields: Vec<Field>,
    pub methods: Vec<FunctionId>,

    // special methods
    pub drop_method: Option<FunctionId>,
}

impl Struct {
    pub fn resolve_field(&self, name: &str) -> Option<&Field> {
        self.fields.iter().find(|f| f.name == name)
    }

    /// true if the struct requires running something on drop:
    /// - contains __drop__()
    /// - OR contains field that require drop logic
    pub fn requires_drop_logic(&self, program: &Program) -> bool {
        self.drop_method.is_some()
            || self.fields.iter().any(|f| {
                if let Some(t) = &f.type_ {
                    t.requires_drop_logic(program)
                } else {
                    false
                }
            })
    }
}
