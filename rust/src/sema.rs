use std::{borrow::Borrow, collections::HashMap};

use scopeguard::{guard, ScopeGuard};

use crate::parser;

//// IDs

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id {
    module: ModuleId,
    id: usize,
}

impl Id {
    pub fn mangle(&self) -> String {
        format!("{}_{}", self.module.0, self.id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub Id);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructId(pub Id);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub Id);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarId(pub Id);

pub const PRELUDE_MODULE: ModuleId = ModuleId(0);
pub const MAIN_MODULE: ModuleId = ModuleId(1);

#[derive(Clone)]
pub enum Primitive {
    Void,
    U32,
    Bool,
    String,
    StaticString,
    Range,
    EmptyArray,
}

//// Type name
// pub enum TypeName {
//     Primitive(PrimitiveType),
//     Struct(StructId),
// }

//// Type

#[derive(Clone)]
pub enum Type {
    Primitive(Primitive),
    Array {
        inner: Box<Type>,
        size: usize,
    },
    // function pointer
    Function {
        function: FunctionId,
    },
    Slice {
        inner: Box<Type>,
        mut_elements: bool,
    },
    Struct {
        id: StructId,
    },
}

impl Type {
    pub fn name(&self, program: &Program) -> String {
        match self {
            Type::Primitive(Primitive::Void) => "void".into(),
            Type::Primitive(Primitive::U32) => "u32".into(),
            Type::Primitive(Primitive::Bool) => "bool".into(),
            Type::Primitive(Primitive::String) => "string".into(),
            Type::Primitive(Primitive::StaticString) => "static_string".into(),
            Type::Primitive(Primitive::Range) => "range".into(),
            Type::Primitive(Primitive::EmptyArray) => "<empty array>".into(),
            Type::Array { inner, size } => {
                format!("[{}]{}", size, inner.name(program))
            }
            Type::Function { function } => {
                format!("<func {}>", program.functions[function].borrow().name())
            }
            Type::Slice {
                inner,
                mut_elements,
            } => format!(
                "[]{}{}",
                if *mut_elements { "mut " } else { "" },
                inner.name(program)
            ),
            Type::Struct { id } => program.structs[id].name.clone(),
        }
    }
}

//// Function

pub struct Var {
    id: Option<VarId>,
    pub type_: Type,
    pub name: String,
}

impl Var {
    pub fn new(type_: Type, name: String) -> Self {
        Var {
            id: None,
            type_,
            name,
        }
    }
}

pub struct Scope {
    id: Option<ScopeId>,
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

pub struct Function {
    id: Option<FunctionId>,
    pub struct_: Option<StructId>,
    pub name: String,
    pub params_scope: ScopeId,
    pub body: Option<Statement>, // none = extern function
    pub return_type: Type,
}

impl Function {
    pub fn new(name: String, program: &mut Program) -> Self {
        // create params scope
        let params_scope = program.add_scope(MAIN_MODULE, Scope::new());

        Function {
            id: None,
            struct_: None,
            name,
            params_scope,
            body: None,
            return_type: Type::Primitive(Primitive::Void),
        }
    }

    pub fn id(&self) -> FunctionId {
        self.id.unwrap()
    }

    pub fn add_param(&mut self, var: Var, program: &mut Program) {
        let id = program.add_var(self.params_scope.0.module, var);
        let scope = program.get_scope_mut(self.params_scope);
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
        self.return_type = rt;
        self
    }
    pub fn with_body(mut self, body: Statement) -> Function {
        self.body = Some(body);
        self
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }
}

//// Statements

pub enum Statement {
    Expression(Expression),
    Block(Vec<Statement>),
    VarDecl {
        var: VarId,
        init: Option<Expression>,
    },
}

//// Expressions

pub enum Expression {
    Call {
        function_id: Option<FunctionId>,
        arguments: HashMap<VarId, Expression>,
    },
    StringLiteral {
        value: String,
    },
    VarRef {
        type_: Type,
        var_id: Option<VarId>,
    },
}

impl Expression {
    pub fn type_(&self, program: &Program) -> Option<Type> {
        match self {
            Expression::Call { function_id, .. } => {
                Some(program.get_function((*function_id)?).return_type.clone())
            }
            Expression::StringLiteral { .. } => Some(Type::Primitive(Primitive::StaticString)),
            Expression::VarRef { type_, .. } => Some(type_.clone()),
        }
    }
}

//// Struct

pub struct Struct {
    id: Option<StructId>,
    name: String,
}

//// Program

pub struct Program {
    functions: HashMap<FunctionId, Function>,
    structs: HashMap<StructId, Struct>,
    vars: HashMap<VarId, Var>,
    scopes: HashMap<ScopeId, Scope>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            functions: HashMap::new(),
            structs: HashMap::new(),
            vars: HashMap::new(),
            scopes: HashMap::new(),
        }
    }

    ////

    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.functions.values()
    }

    pub fn get_function(&self, id: FunctionId) -> &Function {
        self.functions.get(&id).expect("Function not found")
    }

    pub fn get_function_mut(&mut self, id: FunctionId) -> &mut Function {
        self.functions.get_mut(&id).expect("Function not found")
    }

    pub fn lookup_function(&self, name: &str) -> Option<&Function> {
        self.functions.values().find(|f| f.name == name)
    }

    pub fn add_function(&mut self, module: ModuleId, mut function: Function) -> FunctionId {
        let id = FunctionId(Id {
            module,
            id: self.functions.len(),
        });
        function.id = Some(id);
        self.functions.insert(id, function);
        id
    }

    ////

    pub fn structs(&self) -> impl Iterator<Item = &Struct> {
        self.structs.values()
    }

    pub fn get_struct(&self, id: StructId) -> &Struct {
        self.structs.get(&id).expect("Struct not found")
    }

    pub fn add_struct(&mut self, module: ModuleId, mut struct_: Struct) -> StructId {
        let id = StructId(Id {
            module,
            id: self.structs.len(),
        });
        struct_.id = Some(id);
        self.structs.insert(id, struct_);
        id
    }

    ////

    pub fn get_var(&self, id: VarId) -> &Var {
        self.vars.get(&id).expect("Var not found")
    }

    pub fn add_var(&mut self, module: ModuleId, mut var: Var) -> VarId {
        let id = VarId(Id {
            module,
            id: self.vars.len(),
        });
        var.id = Some(id);
        self.vars.insert(id, var);
        id
    }

    ////

    pub fn get_scope(&self, id: ScopeId) -> &Scope {
        self.scopes.get(&id).expect("Scope not found")
    }

    pub fn get_scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        self.scopes.get_mut(&id).expect("Scope not found")
    }

    pub fn add_scope(&mut self, module: ModuleId, mut scope: Scope) -> ScopeId {
        let id = ScopeId(Id {
            module,
            id: self.scopes.len(),
        });
        scope.id = Some(id);
        self.scopes.insert(id, scope);
        id
    }
}

////////
pub struct TypeCheckerError(pub String);

pub struct TypeChecker<'data> {
    p_program: &'data parser::Program,
    program: Program,

    // stage 1
    function_bodies_to_check: Vec<(&'data parser::Statement, FunctionId)>,

    // stage 2
    errors: Vec<TypeCheckerError>,
}

impl<'data> TypeChecker<'data> {
    pub fn new(p_program: &'data parser::Program) -> Self {
        TypeChecker {
            p_program,
            program: Program::new(),

            function_bodies_to_check: vec![],

            errors: vec![],
        }
    }

    fn errors(&self) -> impl Iterator<Item = &TypeCheckerError> {
        self.errors.iter()
    }

    fn add_prelude(&mut self) {
        // TODO: Parse this from file.

        let mut print_func = Function::new("print".into(), &mut self.program);
        print_func.add_param(
            Var::new(Type::Primitive(Primitive::StaticString), "fmtstr".into()),
            &mut self.program,
        );
        self.program.add_function(PRELUDE_MODULE, print_func);
    }

    fn typecheck_type(&self, p_type: &parser::Type) -> Type {
        match p_type {
            parser::Type::Simple(name) => match name.as_str() {
                "u32" => Type::Primitive(Primitive::U32),
                "bool" => Type::Primitive(Primitive::Bool),
                "string" => Type::Primitive(Primitive::String),
                "static_string" => Type::Primitive(Primitive::StaticString),
                "range" => Type::Primitive(Primitive::Range),
                _ => todo!("custom types / structs"),
            },
        }
    }

    fn typecheck_pass1_function_decls(&mut self) {
        for decl in &self.p_program.declarations {
            match decl {
                parser::Declaration::FunctionImpl {
                    name,
                    return_type,
                    body,
                } => {
                    let rt = self.typecheck_type(return_type);
                    let function =
                        Function::new(name.clone(), &mut self.program).with_return_type(rt);
                    let id = self.program.add_function(MAIN_MODULE, function);
                    self.function_bodies_to_check.push((body, id));
                }
            }
        }
    }

    fn typecheck_pass2_function_bodies(&mut self) {
        let btc = self.function_bodies_to_check.clone(); // Should be ok, this is just a bunch of pointers.
        for (parsed, id) in btc {
            let mut tc = TypeCheckerExecution::new(self, id);
            tc.typecheck(parsed);
        }
    }

    pub fn typecheck(mut self) -> Program {
        self.add_prelude();

        // pass 1: add function declarations
        self.typecheck_pass1_function_decls();

        // pass 2: typecheck function bodies
        self.typecheck_pass2_function_bodies();

        self.program
    }
}

/// Typechecker in execution scopes (e.g functions)
pub struct TypeCheckerExecution<'tc, 'data> {
    tc: &'tc mut TypeChecker<'data>,

    function: FunctionId,

    scope_stack: Vec<ScopeId>,
}

impl<'tc, 'data> TypeCheckerExecution<'tc, 'data> {
    pub fn new(tc: &'tc mut TypeChecker<'data>, function: FunctionId) -> Self {
        TypeCheckerExecution {
            tc,
            function,
            scope_stack: vec![],
        }
    }

    fn push_scope(&mut self) {
        let scope = self.tc.program.add_scope(MAIN_MODULE, Scope::new());
        self.scope_stack.push(scope);
    }

    fn pop_scope(&mut self) {
        self.scope_stack.pop();
    }

    fn current_scope(&self) -> ScopeId {
        *self.scope_stack.last().expect("no scope")
    }

    fn lookup_variable(&self, name: String) -> Option<&Var> {
        for scope in self.scope_stack.iter().rev() {
            let scope = self.tc.program.get_scope(*scope);
            for var_id in &scope.vars {
                let var = self.tc.program.get_var(*var_id);
                if var.name == name {
                    return Some(var);
                }
            }
        }
        None
    }

    fn typecheck_expression(&mut self, expr: &parser::Expression) -> Expression {
        match expr {
            parser::Expression::Call {
                function: name,
                args: parsed_args,
            } => {
                let typechecked_args = parsed_args
                    .iter()
                    .map(|arg| self.typecheck_expression(&arg.value))
                    .collect::<Vec<_>>();

                let function = self.tc.program.lookup_function(name);
                if let Some(func) = function {
                    let mut arguments = HashMap::new();
                    // For now we just go in order.
                    // TODO: handle kwargs.

                    let params_scope = self.tc.program.get_scope(func.params_scope);
                    for (i, expr) in typechecked_args.into_iter().enumerate() {
                        let param = *(if let Some(p) = params_scope.vars.get(i) {
                            p
                        } else {
                            self.tc
                                .errors
                                .push(TypeCheckerError("Too many arguments".into()));
                            break;
                        });
                        // TODO: Check param type
                        arguments.insert(param, expr);
                    }

                    Expression::Call {
                        function_id: Some(
                            func.borrow().id.expect(
                                "function not yet typechecked (this should be done in pass1)",
                            ),
                        ),
                        arguments,
                    }
                } else {
                    self.tc.errors.push(TypeCheckerError(format!(
                        "Function with name '{}' not found",
                        name
                    )));
                    Expression::Call {
                        function_id: None,
                        arguments: HashMap::new(),
                    }
                }
            }
            parser::Expression::StringLiteral { value } => Expression::StringLiteral {
                value: value.clone(),
            },
            parser::Expression::Name(name) => {
                if let Some(var) = self.lookup_variable(name.clone()) {
                    Expression::VarRef {
                        type_: var.type_.clone(),
                        var_id: Some(var.id.expect("var without id")),
                    }
                } else {
                    self.tc.errors.push(TypeCheckerError(format!(
                        "Variable with name '{}' not found",
                        name
                    )));
                    Expression::VarRef {
                        type_: Type::Primitive(Primitive::Void),
                        var_id: None,
                    }
                }
            }
        }
    }

    fn typecheck_statement(&mut self, stmt: &parser::Statement) -> Statement {
        match stmt {
            parser::Statement::Expression(expr) => {
                Statement::Expression(self.typecheck_expression(&expr))
            }
            parser::Statement::Block(stmts) => {
                self.push_scope();
                let b =
                    Statement::Block(stmts.iter().map(|s| self.typecheck_statement(s)).collect());
                self.pop_scope();
                b
            }
            parser::Statement::VarDecl {
                mut_,
                type_,
                name,
                init_value,
            } => {
                let type_ = self.tc.typecheck_type(type_);
                let var = Var::new(type_, name.clone());
                let func_module = self
                    .tc
                    .program
                    .get_function(self.function)
                    .params_scope
                    .0
                    .module;
                let var_id = self.tc.program.add_var(func_module, var);
                eprintln!(
                    "adding var id={:?} to scope id={:?}",
                    var_id,
                    self.current_scope()
                );
                // current scope stack
                eprintln!("current scopes stack: {:?}", self.scope_stack);

                self.tc
                    .program
                    .get_scope_mut(self.current_scope())
                    .vars
                    .push(var_id);

                let init_value = init_value
                    .as_ref()
                    .map(|expr| self.typecheck_expression(expr));

                Statement::VarDecl {
                    var: var_id,
                    init: init_value,
                }
            }
        }
    }

    fn typecheck(&mut self, stmt: &parser::Statement) {
        self.scope_stack
            .push(self.tc.program.get_function(self.function).params_scope);

        let stmt = self.typecheck_statement(stmt);
        self.tc.program.get_function_mut(self.function).body = Some(stmt);

        self.scope_stack.pop();
    }
}
