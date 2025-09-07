use crate::sema::{decl_scope::DeclScope, *};

#[derive(Debug)]
pub struct Program {
    modules: Vec<Module>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            modules: Vec::new(),
        }
    }

    pub fn modules(&self) -> impl Iterator<Item = &Module> {
        self.modules.iter()
    }

    // add and return new module
    pub fn add_module(&mut self) -> (&mut Module, ModuleId) {
        let id = ModuleId(self.modules.len());
        let module = Module::new(id);
        self.modules.push(module);
        (self.modules.last_mut().unwrap(), id)
    }

    pub fn get_module(&self, id: ModuleId) -> &Module {
        self.modules.get(id.0).expect("Module not found")
    }

    pub fn get_module_mut(&mut self, id: ModuleId) -> &mut Module {
        self.modules.get_mut(id.0).expect("Module not found")
    }

    ////

    pub fn get_function(&self, id: FunctionId) -> &Function {
        self.modules
            .get(id.0.module_id().0)
            .expect("Module not found")
            .get_function(id.0.id_in_module())
    }

    pub fn get_struct(&self, id: StructId) -> &Struct {
        self.modules
            .get(id.0.module_id().0)
            .expect("Module not found")
            .get_struct(id.0.id_in_module())
    }

    pub fn get_var(&self, id: VarId) -> &Var {
        self.modules
            .get(id.0.module_id().0)
            .expect("Module not found")
            .get_var(id.0.id_in_module())
    }

    pub fn get_scope(&self, id: ScopeId) -> &Scope {
        self.modules
            .get(id.0.module_id().0)
            .expect("Module not found")
            .get_scope(id.0.id_in_module())
    }

    pub fn get_scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        let module = self
            .modules
            .get_mut(id.0.module_id().0)
            .expect("Module not found");
        module.get_scope_mut(id.0.id_in_module())
    }

    pub fn get_decl_scope(&self, id: DeclScopeId) -> &DeclScope {
        self.modules
            .get(id.0.module_id().0)
            .expect("Module not found")
            .get_decl_scope(id.0.id_in_module())
    }
}
