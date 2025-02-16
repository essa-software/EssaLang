use std::{borrow::Borrow, collections::HashMap, ops::Range};

use crate::{
    error::CompilationError,
    parser::{self, BinOpClass},
};

//// IDs

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(pub Id);

pub const PRELUDE_MODULE: ModuleId = ModuleId(0);
pub const MAIN_MODULE: ModuleId = ModuleId(1);
pub const PRINT_VARARGS_HACK_MODULE: ModuleId = ModuleId(2137);

#[derive(Clone, PartialEq, Eq)]
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

#[derive(Clone, PartialEq, Eq)]
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

    pub fn mangle(&self, program: &Program) -> String {
        match self {
            Type::Primitive(Primitive::Void) => "void".into(),
            Type::Primitive(Primitive::U32) => "u32".into(),
            Type::Primitive(Primitive::Bool) => "bool".into(),
            Type::Primitive(Primitive::String) => "string".into(),
            Type::Primitive(Primitive::StaticString) => "static_string".into(),
            Type::Primitive(Primitive::Range) => "range".into(),
            Type::Primitive(Primitive::EmptyArray) => "empty_array".into(),
            Type::Array { inner, size } => {
                format!("array_{}_{}", size, inner.mangle(program))
            }
            Type::Function { function } => {
                format!("func_{}", program.functions[function].borrow().name())
            }
            Type::Slice {
                inner,
                mut_elements,
            } => format!(
                "slice_{}_{}",
                if *mut_elements { "mut" } else { "const" },
                inner.mangle(program)
            ),
            Type::Struct { id } => format!("struct_{}", program.structs[id].name),
        }
    }
}

//// Function

pub struct Var {
    id: Option<VarId>,
    pub type_: Option<Type>,
    pub name: String,
}

impl Var {
    pub fn new(type_: Option<Type>, name: String) -> Self {
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

    pub fn should_use_print_vararg_hack(&self) -> bool {
        self.name == "print"
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
    Return(Option<Expression>),
    If {
        condition: Expression,
        then_block: Box<Statement>,
    },
}

//// Expressions

pub enum Expression {
    Call {
        function_id: Option<FunctionId>,
        arguments: HashMap<VarId, Expression>,
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
    VarRef {
        type_: Option<Type>,
        var_id: Option<VarId>,
    },
    BinaryOp {
        op: parser::BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
}

impl Expression {
    pub fn type_(&self, program: &Program) -> Option<Type> {
        match self {
            Expression::Call { function_id, .. } => {
                Some(program.get_function((*function_id)?).return_type.clone())
            }
            Expression::BoolLiteral { .. } => Some(Type::Primitive(Primitive::Bool)),
            Expression::IntLiteral { .. } => Some(Type::Primitive(Primitive::U32)),
            Expression::StringLiteral { .. } => Some(Type::Primitive(Primitive::StaticString)),
            Expression::VarRef { type_, .. } => type_.clone(),
            Expression::BinaryOp { op, left, right } => match op {
                _ if matches!(op.class(), BinOpClass::Comparison) => {
                    Some(Type::Primitive(Primitive::Bool))
                }
                _ if matches!(op.class(), BinOpClass::Assignment) => {
                    Some(Type::Primitive(Primitive::Void))
                }
                _ if matches!(op.class(), BinOpClass::Additive) => {
                    Some(Type::Primitive(Primitive::U32))
                }
                _ => None,
            },
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

pub struct TypeChecker<'data> {
    p_program: &'data parser::Program,
    program: Program,

    // stage 1
    function_bodies_to_check: Vec<(&'data parser::Statement, FunctionId)>,

    // stage 2
    errors: Vec<CompilationError>,
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

    fn add_prelude(&mut self) {
        // TODO: Parse this from file.

        let mut print_func = Function::new("print".into(), &mut self.program);
        print_func.add_param(
            Var::new(
                Some(Type::Primitive(Primitive::StaticString)),
                "fmtstr".into(),
            ),
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
                    let rt = return_type
                        .as_ref()
                        .map(|ty| self.typecheck_type(&ty))
                        .unwrap_or(if name == "main" {
                            Type::Primitive(Primitive::U32)
                        } else {
                            Type::Primitive(Primitive::Void)
                        });
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

    pub fn typecheck(mut self) -> (Program, Vec<CompilationError>) {
        self.add_prelude();

        // pass 1: add function declarations
        self.typecheck_pass1_function_decls();

        // pass 2: typecheck function bodies
        self.typecheck_pass2_function_bodies();

        (self.program, self.errors)
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

    fn typecheck_expr_call(
        &mut self,
        name: &String,
        parsed_args: &Vec<parser::FunctionArg>,
        range: &Range<usize>,
    ) -> Expression {
        let typechecked_args = parsed_args
            .iter()
            .map(|arg| {
                (
                    self.typecheck_expression(&arg.value),
                    arg.value.range.clone(),
                )
            })
            .collect::<Vec<_>>();

        let Some(function) = self.tc.program.lookup_function(name) else {
            self.tc.errors.push(CompilationError::new(
                format!("Function with name '{}' not found", name),
                range.clone(),
            ));
            return Expression::Call {
                function_id: None,
                arguments: HashMap::new(),
            };
        };
        let mut arguments = HashMap::new();
        // For now we just go in order.
        // TODO: handle kwargs.

        // FIXME: support varargs properly
        if function.should_use_print_vararg_hack() {
            let function_id = function.id.unwrap();

            // just push one by one with increasing
            // (nonsensical) var ids
            for (i, (expr, range)) in typechecked_args.into_iter().enumerate() {
                let Some(type_) = expr.type_(&self.tc.program) else {
                    self.tc.errors.push(CompilationError::new(
                        format!("Argument {} to print function has invalid type", i),
                        range,
                    ));
                    continue;
                };
                let var = Var::new(Some(type_), format!("arg{}", i));
                let var_id = VarId(Id {
                    module: PRINT_VARARGS_HACK_MODULE,
                    id: i,
                });
                self.tc.program.add_var(PRINT_VARARGS_HACK_MODULE, var);
                arguments.insert(var_id, expr);
            }

            return Expression::Call {
                function_id: Some(function_id),
                arguments,
            };
        }

        let params_scope = self.tc.program.get_scope(function.params_scope);
        for (i, (expr, range)) in typechecked_args.into_iter().enumerate() {
            let param = *(if let Some(p) = params_scope.vars.get(i) {
                p
            } else {
                self.tc.errors.push(CompilationError::new(
                    "Too many arguments".into(),
                    range.clone(),
                ));
                break;
            });
            // TODO: Check param type
            arguments.insert(param, expr);
        }

        Expression::Call {
            function_id: Some(
                function
                    .borrow()
                    .id
                    .expect("function not yet typechecked (this should be done in pass1)"),
            ),
            arguments,
        }
    }

    fn check_operator_types(
        &mut self,
        op: parser::BinaryOp,
        left: Type,
        right: Type,
        range: &Range<usize>,
    ) {
        if matches!(op.class(), BinOpClass::Comparison) {
            if left != right {
                self.tc.errors.push(CompilationError::new(
                    "Comparison operator with different types".into(),
                    range.clone(),
                ));
            }
        }
    }

    fn typecheck_expression(
        &mut self,
        parser::ExpressionNode {
            expression: expr,
            range,
        }: &parser::ExpressionNode,
    ) -> Expression {
        match expr {
            parser::Expression::Call { function, args } => match &function.expression {
                parser::Expression::Name(name) => self.typecheck_expr_call(&name, args, range),
                _ => {
                    self.tc.errors.push(CompilationError::new(
                        "Only function names are supported for now".into(),
                        function.range.clone(),
                    ));
                    Expression::Call {
                        function_id: None,
                        arguments: HashMap::new(),
                    }
                }
            },
            parser::Expression::BoolLiteral { value } => Expression::BoolLiteral { value: *value },
            parser::Expression::IntLiteral { value } => {
                if *value > (u32::MAX as u64) {
                    self.tc.errors.push(CompilationError::new(
                        format!("Integer literal {} is too large", value),
                        range.clone(),
                    ));
                }
                Expression::IntLiteral { value: *value }
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
                    self.tc.errors.push(CompilationError::new(
                        format!("Variable with name '{}' not found", name),
                        range.clone(),
                    ));
                    Expression::VarRef {
                        type_: None,
                        var_id: None,
                    }
                }
            }
            parser::Expression::BinaryOp {
                op,
                op_range,
                left,
                right,
            } => {
                let left = self.typecheck_expression(left);
                let right = self.typecheck_expression(right);

                if let (Some(left_type), Some(right_type)) =
                    (left.type_(&self.tc.program), right.type_(&self.tc.program))
                {
                    self.check_operator_types(*op, left_type, right_type, op_range);
                } else {
                    self.tc.errors.push(CompilationError::new(
                        "Invalid types for binary operator".into(),
                        range.clone(),
                    ));
                }

                Expression::BinaryOp {
                    op: *op,
                    left: Box::new(left),
                    right: Box::new(right),
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
                mut_: _, // TODO: handle mut
                type_,
                name,
                init_value,
            } => {
                let init_value = init_value
                    .as_ref()
                    .map(|expr| self.typecheck_expression(expr));

                let type_: Option<Type> = type_.as_ref().map(|ty| self.tc.typecheck_type(&ty)).or(
                    if let Some(init) = &init_value {
                        init.type_(&self.tc.program)
                    } else {
                        None
                    },
                );
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

                Statement::VarDecl {
                    var: var_id,
                    init: init_value,
                }
            }
            parser::Statement::Return(expression) => {
                let expr = expression
                    .as_ref()
                    .map(|expr| self.typecheck_expression(expr));

                // TODO: Check function return type

                Statement::Return(expr)
            }
            parser::Statement::If {
                condition,
                then_block,
            } => {
                let (condition, cond_range) = (
                    self.typecheck_expression(condition),
                    condition.range.clone(),
                );
                let condition_type = condition.type_(&self.tc.program);
                let then_block = self.typecheck_statement(then_block);
                if condition_type.is_some()
                    && !matches!(condition_type, Some(Type::Primitive(Primitive::Bool)))
                {
                    self.tc.errors.push(CompilationError::new(
                        "If condition must be of type bool".into(),
                        cond_range,
                    ));
                }
                Statement::If {
                    condition,
                    then_block: Box::new(then_block),
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
