use crate::sema::{decl_scope::DeclScope, *};

#[derive(Debug)]
pub struct Module {
    id: ModuleId,
    functions: Vec<Function>,
    structs: Vec<Struct>,
    vars: Vec<Var>,
    scopes: Vec<Scope>,
    decl_scopes: Vec<DeclScope>,
    top_level_decl_scope: Option<DeclScopeId>,
    // Decl scopes from other modules which should be searched too
    pub(super) foreign_decl_scopes: Vec<DeclScopeId>,
}

impl Module {
    pub fn new(id: ModuleId) -> Self {
        let mut m = Module {
            id,
            functions: Vec::new(),
            structs: Vec::new(),
            vars: Vec::new(),
            scopes: Vec::new(),
            decl_scopes: Vec::new(),
            top_level_decl_scope: None,
            foreign_decl_scopes: Vec::new(),
        };
        m.top_level_decl_scope = Some(m.add_decl_scope());
        m
    }

    ////

    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.functions.iter()
    }

    pub fn get_function(&self, id: usize) -> &Function {
        self.functions.get(id).expect("Function not found")
    }

    pub fn get_function_mut(&mut self, id: usize) -> &mut Function {
        self.functions.get_mut(id).expect("Function not found")
    }

    pub fn add_function(&mut self, mut function: Function) -> FunctionId {
        let id = FunctionId(Id::new(self.id, self.functions.len()));
        function.id = Some(id);
        // add to decl scope
        self.get_decl_scope_mut(function.decl_scope.0.id_in_module())
            .add_function(function.name.clone(), id);
        self.functions.push(function);
        id
    }

    ////

    pub fn structs(&self) -> impl Iterator<Item = &Struct> {
        self.structs.iter()
    }

    pub fn get_struct(&self, id: usize) -> &Struct {
        self.structs.get(id).expect("Struct not found")
    }

    pub fn get_struct_mut(&mut self, id: usize) -> &mut Struct {
        self.structs.get_mut(id).expect("Struct not found")
    }

    pub fn add_struct(&mut self, mut struct_: Struct) -> StructId {
        let id = StructId(Id::new(self.id, self.structs.len()));
        struct_.id = Some(id);
        self.get_decl_scope_mut(struct_.parent_decl_scope.0.id_in_module())
            .add_struct(struct_.name.clone(), id);
        self.structs.push(struct_);
        id
    }

    ////

    pub fn get_var(&self, id: usize) -> &Var {
        self.vars.get(id).expect("Var not found")
    }

    pub fn add_var(&mut self, mut var: Var) -> VarId {
        let id = VarId(Id::new(self.id, self.vars.len()));
        var.id = Some(id);
        self.vars.push(var);
        id
    }

    ////

    pub fn get_scope(&self, id: usize) -> &Scope {
        self.scopes.get(id).expect("Scope not found")
    }

    pub fn get_scope_mut(&mut self, id: usize) -> &mut Scope {
        self.scopes.get_mut(id).expect("Scope not found")
    }

    pub fn add_scope(&mut self, mut scope: Scope) -> ScopeId {
        let id = ScopeId(Id::new(self.id, self.scopes.len()));
        scope.id = Some(id);
        self.scopes.push(scope);
        id
    }

    ////

    pub fn get_decl_scope(&self, id: usize) -> &DeclScope {
        self.decl_scopes.get(id).expect("DeclScope not found")
    }

    pub fn get_decl_scope_mut(&mut self, id: usize) -> &mut DeclScope {
        self.decl_scopes.get_mut(id).expect("DeclScope not found")
    }

    pub fn add_decl_scope(&mut self) -> DeclScopeId {
        let id = DeclScopeId(Id::new(self.id, self.decl_scopes.len()));
        self.decl_scopes.push(DeclScope::new(id));
        id
    }

    pub fn add_child_decl_scope(&mut self, name: String, parent: DeclScopeId) -> DeclScopeId {
        let id = self.add_decl_scope();
        let parent = self.get_decl_scope_mut(parent.0.id_in_module());
        parent.add_child_scope(name, id);
        id
    }

    pub fn top_level_decl_scope(&self) -> &DeclScope {
        self.get_decl_scope(self.top_level_decl_scope.unwrap().0.id_in_module())
    }

    pub fn top_level_decl_scope_mut(&mut self) -> &mut DeclScope {
        self.get_decl_scope_mut(self.top_level_decl_scope.unwrap().0.id_in_module())
    }

    // Copy declaration scopes etc, from a given module
    pub fn flat_import(&mut self, imported: DeclScopeId) {
        self.foreign_decl_scopes.push(imported);
    }
}
