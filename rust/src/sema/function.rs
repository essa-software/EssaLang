use log::trace;

use crate::sema::{
    id::{DeclScopeId, FunctionId, ScopeId, StructId, VarId},
    Module, Primitive, Program, Statement, Type,
};

#[derive(Debug)]
pub struct Var {
    pub(super) id: Option<VarId>,
    pub type_: Option<Type>,
    pub name: String,
    pub mut_: bool,
}

impl Var {
    pub fn new(type_: Option<Type>, name: String, mut_: bool) -> Self {
        Var {
            id: None,
            type_,
            name,
            mut_,
        }
    }
}

#[derive(Debug)]
pub struct Scope {
    pub(super) id: Option<ScopeId>,
    pub vars: Vec<VarId>,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            id: None,
            vars: vec![],
        }
    }

    pub fn lookup_var(&self, name: &str, program: &Program) -> Option<VarId> {
        // FIXME: Optimize
        self.vars
            .iter()
            .find(|var_id| program.get_var(**var_id).name == name)
            .copied()
    }
}

#[derive(Debug)]
pub struct Function {
    pub(super) id: Option<FunctionId>,
    // If Some, the Function is a non-static method of the given struct
    // i.e can be called (only) with `object.method()`
    pub struct_: Option<StructId>,
    pub decl_scope: DeclScopeId,
    pub name: String,
    pub params_scope: ScopeId,
    pub body: Option<Statement>, // none = extern function
    pub return_type: Option<Type>,
}

impl Function {
    pub fn new(name: String, decl_scope: DeclScopeId, module: &mut Module) -> Self {
        // create params scope
        let params_scope = module.add_scope(Scope::new());
        trace!(
            "Creating params scope: {:?} for function {}",
            params_scope,
            name
        );

        Function {
            id: None,
            struct_: None,
            decl_scope,
            name,
            params_scope,
            body: None,
            return_type: Some(Type::Primitive(Primitive::Void)),
        }
    }

    pub fn id(&self) -> FunctionId {
        self.id.unwrap()
    }

    pub fn add_param(&mut self, var: Var, module: &mut Module) {
        let id = module.add_var(var);
        let scope = module.get_scope_mut(self.params_scope.0.id_in_module());
        scope.vars.push(id);
    }

    pub fn with_struct(mut self, struct_: StructId) -> Function {
        self.struct_ = Some(struct_);
        self
    }
    pub fn with_name(mut self, name: impl Into<String>) -> Function {
        self.name = name.into();
        self
    }
    pub fn with_return_type(mut self, rt: Type) -> Function {
        self.return_type = Some(rt);
        self
    }
    pub fn with_body(mut self, body: Statement) -> Function {
        self.body = Some(body);
        self
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }

    pub fn should_use_print_vararg_hack(&self) -> bool {
        self.name == "print"
    }
}
