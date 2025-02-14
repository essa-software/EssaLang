use std::{borrow::Borrow, collections::HashMap};

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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub Id);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructId(pub Id);

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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalVarId(pub usize);

pub struct LocalVar {
    id: Option<LocalVarId>,
    pub type_: Type,
    pub name: String,
}

impl LocalVar {
    pub fn new(type_: Type, name: String) -> Self {
        LocalVar {
            id: None,
            type_,
            name,
        }
    }
}

pub struct Function {
    id: Option<FunctionId>,
    pub struct_: Option<StructId>,
    pub name: String,
    pub params: Vec<LocalVarId>,
    pub body: Option<Statement>, // none = extern function
    pub return_type: Type,
    pub local_vars: HashMap<LocalVarId, LocalVar>,
}

impl Function {
    pub fn new(name: String) -> Self {
        Function {
            id: None,
            struct_: None,
            name,
            params: Vec::new(),
            body: None,
            return_type: Type::Primitive(Primitive::Void),
            local_vars: HashMap::new(),
        }
    }

    pub fn get_local_var(&self, id: LocalVarId) -> &LocalVar {
        self.local_vars.get(&id).expect("Local var not found")
    }

    pub fn add_local_var(&mut self, mut var: LocalVar) -> LocalVarId {
        let id = LocalVarId(self.local_vars.len());
        var.id = Some(id);
        self.local_vars.insert(id, var);
        id
    }

    pub fn add_param(&mut self, var: LocalVar) -> LocalVarId {
        let id = self.add_local_var(var);
        self.params.push(id);
        id
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
}

//// Expressions

pub enum Expression {
    Call {
        function_id: Option<FunctionId>,
        arguments: HashMap<LocalVarId, Expression>,
    },
    StringLiteral {
        value: String,
    },
}

impl Expression {
    pub fn type_(&self, program: &Program) -> Option<Type> {
        match self {
            Expression::Call { function_id, .. } => {
                Some(program.get_function((*function_id)?).return_type.clone())
            }
            Expression::StringLiteral { .. } => Some(Type::Primitive(Primitive::StaticString)),
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
}

impl Program {
    pub fn new() -> Self {
        Program {
            functions: HashMap::new(),
            structs: HashMap::new(),
        }
    }

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
}

////////
pub struct TypeCheckerError(pub String);

pub struct TypeChecker<'data> {
    p_program: &'data parser::Program,
    program: Program,

    function_bodies_to_check: Vec<(&'data parser::Statement, FunctionId)>,

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

        let mut print_func = Function::new("print".into());
        print_func.add_param(LocalVar::new(
            Type::Primitive(Primitive::StaticString),
            "fmtstr".into(),
        ));
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
                _ => todo!("custom types / structs"), // TODO: Probably struct.
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
                    let function = Function::new(name.clone()).with_return_type(rt);
                    let id = self.program.add_function(MAIN_MODULE, function);
                    self.function_bodies_to_check.push((body, id));
                }
            }
        }
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

                let function = self.program.lookup_function(name);
                if let Some(func) = function {
                    let mut arguments = HashMap::new();
                    // For now we just go in order.
                    // TODO: handle kwargs.

                    for (i, expr) in typechecked_args.into_iter().enumerate() {
                        let param = *(if let Some(p) = func.params.get(i) {
                            p
                        } else {
                            self.errors
                                .push(TypeCheckerError("Too many arguments".into()));
                            break;
                        });
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
                    self.errors.push(TypeCheckerError(format!(
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
        }
    }

    fn typecheck_statement(&mut self, stmt: &parser::Statement) -> Statement {
        match stmt {
            parser::Statement::Expression(expr) => {
                Statement::Expression(self.typecheck_expression(&expr))
            }
            parser::Statement::Block(stmts) => {
                Statement::Block(stmts.iter().map(|s| self.typecheck_statement(s)).collect())
            }
        }
    }

    fn typecheck_pass2_function_bodies(&mut self) {
        let btc = self.function_bodies_to_check.clone(); // Should be ok, this is just a bunch of pointers.
        for (parsed, id) in btc {
            self.program.get_function_mut(id).body = Some(self.typecheck_statement(parsed));
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
