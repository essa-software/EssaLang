//// Declaration scope

use std::collections::HashMap;

use crate::types;

use super::{
    id::{DeclScopeId, FunctionId, StructId},
    Module, Program,
};

#[derive(Debug)]
pub struct DeclScope {
    id: DeclScopeId,
    child_scopes: HashMap<String, DeclScopeId>,
    functions: HashMap<String, FunctionId>,
    structs: HashMap<String, StructId>,
}

impl DeclScope {
    pub fn new(id: DeclScopeId) -> Self {
        DeclScope {
            id,
            child_scopes: HashMap::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
        }
    }

    ////

    pub fn id(&self) -> DeclScopeId {
        self.id
    }

    ////

    pub fn add_child_scope(&mut self, name: String, id: DeclScopeId) {
        self.child_scopes.insert(name, id);
    }

    pub fn lookup_child_scope(&self, name: &str) -> Option<DeclScopeId> {
        self.child_scopes.get(name).copied()
    }

    pub fn lookup_child_scope_object<'p>(
        &self,
        name: &str,
        program: &'p Program,
    ) -> Option<&'p DeclScope> {
        self.lookup_child_scope(name)
            .map(|id| program.get_decl_scope(id))
    }

    ////

    pub fn add_function(&mut self, name: String, id: FunctionId) {
        self.functions.insert(name, id);
    }

    pub fn lookup_function(&self, name: &str) -> Option<FunctionId> {
        self.functions.get(name).copied()
    }

    pub fn lookup_function_rec<'p>(
        &self,
        name: &types::ScopedName,
        program: &'p Program,
    ) -> Option<FunctionId> {
        match name.leaf() {
            Some(leaf) => self.lookup_function(&leaf),
            None => {
                let (root, rest) = name.split_root();
                self.lookup_child_scope_object(root, program)
                    .and_then(|scope| scope.lookup_function_rec(&rest, program))
            }
        }
    }

    ////

    pub fn add_struct(&mut self, name: String, id: StructId) {
        self.structs.insert(name, id);
    }

    pub fn lookup_struct(&self, name: &str) -> Option<StructId> {
        self.structs.get(name).copied()
    }

    pub fn lookup_struct_rec<'p>(
        &self,
        name: &types::ScopedName,
        program: &'p Program,
    ) -> Option<StructId> {
        match name.leaf() {
            Some(leaf) => self.lookup_struct(&leaf),
            None => {
                let (root, rest) = name.split_root();
                self.lookup_child_scope_object(root, program)
                    .and_then(|scope| scope.lookup_struct_rec(&rest, program))
            }
        }
    }
}
