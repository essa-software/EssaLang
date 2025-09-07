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
}
