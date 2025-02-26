#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id {
    module: ModuleId,
    id: usize,
}

impl Id {
    pub fn new(module: ModuleId, id: usize) -> Self {
        Self { module, id }
    }

    pub fn mangle(&self) -> String {
        format!("{}_{}", self.module.0, self.id)
    }

    pub fn module_id(&self) -> ModuleId {
        self.module
    }

    pub fn id_in_module(&self) -> usize {
        self.id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub Id);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructId(pub Id);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DeclScopeId(pub Id);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub Id);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(pub Id);
