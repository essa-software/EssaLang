use std::{
    backtrace::Backtrace, collections::HashMap, env::current_exe, fmt::Display, ops::Range,
    path::PathBuf,
};

use crate::{
    compiler,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Primitive {
    Void,
    U32,
    Bool,
    Char,
    String,
    StaticString,
    Range,
    EmptyArray,
}

//// Type

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Primitive(Primitive),
    Array {
        inner: Box<Type>,
        size: usize,
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
            Type::Primitive(Primitive::Char) => "char".into(),
            Type::Primitive(Primitive::String) => "string".into(),
            Type::Primitive(Primitive::StaticString) => "static_string".into(),
            Type::Primitive(Primitive::Range) => "range".into(),
            Type::Primitive(Primitive::EmptyArray) => "<empty array>".into(),
            Type::Array { inner, size } => {
                format!("[{}]{}", size, inner.name(program))
            }
            Type::Slice {
                inner,
                mut_elements,
            } => format!(
                "[]{}{}",
                if *mut_elements { "mut " } else { "" },
                inner.name(program)
            ),
            Type::Struct { id } => program.get_struct(*id).name.clone(),
        }
    }

    pub fn mangle(&self, program: &Program) -> String {
        match self {
            Type::Primitive(Primitive::Void) => "void".into(),
            Type::Primitive(Primitive::U32) => "u32".into(),
            Type::Primitive(Primitive::Bool) => "bool".into(),
            Type::Primitive(Primitive::Char) => "char".into(),
            Type::Primitive(Primitive::String) => "string".into(),
            Type::Primitive(Primitive::StaticString) => "static_string".into(),
            Type::Primitive(Primitive::Range) => "range".into(),
            Type::Primitive(Primitive::EmptyArray) => "empty_array".into(),
            Type::Array { inner, size } => {
                format!("array_{}_{}", size, inner.mangle(program))
            }
            Type::Slice {
                inner,
                mut_elements,
            } => format!(
                "slice_{}_{}",
                if *mut_elements { "mut" } else { "const" },
                inner.mangle(program)
            ),
            Type::Struct { id } => format!("struct_{}", program.get_struct(*id).name),
        }
    }

    pub fn iter_value_type(&self, _program: &Program) -> Option<Type> {
        match self {
            Type::Primitive(Primitive::Range) => Some(Type::Primitive(Primitive::U32)),
            Type::Primitive(Primitive::StaticString) => Some(Type::Primitive(Primitive::Char)),
            Type::Array { inner, .. } => Some(*inner.clone()),
            _ => None,
        }
    }

    pub fn index_value_type(&self, _program: &Program) -> Option<Type> {
        // Currently all iterable types are also indexable.
        self.iter_value_type(_program)
    }
}

//// Function

pub struct Var {
    id: Option<VarId>,
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
    pub return_type: Option<Type>,
}

impl Function {
    pub fn new(name: String, module: &mut Module) -> Self {
        // create params scope
        let params_scope = module.add_scope(Scope::new());

        Function {
            id: None,
            struct_: None,
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
        let scope = module.get_scope_mut(self.params_scope.0.id);
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

//// Statements

pub enum Statement {
    Expression(Expression),
    Block(Vec<Statement>),
    VarDecl {
        var: VarId,
        init: Option<Expression>,
    },
    Return(Option<Expression>),
    For {
        it_var: VarId,
        iterable: Expression,
        body: Box<Statement>,
    },
    If {
        condition: Expression,
        then_block: Box<Statement>,
        else_block: Option<Box<Statement>>,
    },
    While {
        condition: Expression,
        body: Box<Statement>,
    },
    Break,
    Continue,
}

//// Expressions

pub enum Expression {
    Call {
        function_id: Option<FunctionId>,
        arguments: HashMap<VarId, Expression>,
    },
    Index {
        indexable: Box<Expression>,
        index: Box<Expression>,
    },
    MemberAccess {
        object: Box<Expression>,
        member: String,
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
    CharLiteral {
        value: char,
    },
    ArrayLiteral {
        value_type: Option<Type>,
        values: Vec<Expression>,
    },
    VarRef {
        type_: Option<Type>,
        var_id: Option<VarId>,
        mut_: bool,
    },
    BinaryOp {
        op: parser::BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
}

pub enum ValueType {
    LValue,      // can be read and written to directly
    ConstLValue, // can be read directly but not written to
    RValue,      // needs to be evaluated
}

impl Expression {
    pub fn type_(&self, program: &Program) -> Option<Type> {
        match self {
            Expression::Call { function_id, .. } => {
                program.get_function((*function_id)?).return_type.clone()
            }
            Expression::Index {
                indexable,
                index: _,
            } => {
                let indexable_type = indexable.type_(program)?;
                indexable_type.index_value_type(program)
            }
            Expression::MemberAccess { object, member } => {
                let object_type = object.type_(program)?;
                match object_type {
                    Type::Struct { id } => {
                        let struct_ = program.get_struct(id);
                        struct_.resolve_field(member).and_then(|f| f.type_.clone())
                    }
                    _ => None,
                }
            }
            Expression::BoolLiteral { .. } => Some(Type::Primitive(Primitive::Bool)),
            Expression::IntLiteral { .. } => Some(Type::Primitive(Primitive::U32)),
            Expression::StringLiteral { .. } => Some(Type::Primitive(Primitive::StaticString)),
            Expression::VarRef { type_, .. } => type_.clone(),
            Expression::BinaryOp {
                op,
                left: _,
                right: _,
            } => match op.class() {
                BinOpClass::Comparison => Some(Type::Primitive(Primitive::Bool)),
                BinOpClass::Assignment => Some(Type::Primitive(Primitive::Void)),
                BinOpClass::Additive | BinOpClass::Multiplicative => {
                    Some(Type::Primitive(Primitive::U32))
                }
                BinOpClass::Range => Some(Type::Primitive(Primitive::Range)),
                BinOpClass::Logical => Some(Type::Primitive(Primitive::Bool)),
            },
            Expression::ArrayLiteral { value_type, values } => {
                if values.is_empty() {
                    Some(Type::Primitive(Primitive::EmptyArray))
                } else {
                    Some(Type::Array {
                        inner: Box::new(value_type.clone()?),
                        size: values.len(),
                    })
                }
            }
            Expression::CharLiteral { value: _ } => Some(Type::Primitive(Primitive::Char)),
        }
    }

    pub fn value_type(&self) -> ValueType {
        match self {
            Expression::VarRef { mut_, .. } => {
                if *mut_ {
                    ValueType::LValue
                } else {
                    ValueType::ConstLValue
                }
            }
            Expression::Index {
                indexable,
                index: _,
            } => indexable.value_type(),
            Expression::MemberAccess { object, member: _ } => object.value_type(),
            _ => ValueType::RValue,
        }
    }

    pub fn can_be_discarded(&self, program: &Program) -> bool {
        match self {
            Expression::Call {
                function_id,
                arguments: _,
            } => match function_id {
                // only functions returning void
                Some(fid) => {
                    matches!(
                        {
                            if let Some(rt) = &program.get_function(*fid).return_type {
                                rt
                            } else {
                                return true;
                            }
                        },
                        Type::Primitive(Primitive::Void)
                    )
                }
                None => true,
            },
            Expression::BinaryOp {
                op,
                left: _,
                right: _,
            } if matches!(op.class(), BinOpClass::Assignment) => true,
            _ => false,
        }
    }
}

//// Struct

#[derive(Debug)]
pub struct Field {
    pub type_: Option<Type>,
    pub name: String,
}

#[derive(Debug)]
pub struct Struct {
    pub id: Option<StructId>,
    pub name: String,
    pub fields: Vec<Field>,
    pub methods: Vec<FunctionId>,
}

impl Struct {
    pub fn resolve_field(&self, name: &str) -> Option<&Field> {
        self.fields.iter().find(|f| f.name == name)
    }
}

//// Module

pub struct Module {
    id: ModuleId,
    functions: Vec<Function>,
    structs: Vec<Struct>,
    vars: Vec<Var>,
    scopes: Vec<Scope>,

    imported_modules: Vec<ModuleId>,
}

impl Module {
    pub fn new(id: ModuleId) -> Self {
        Module {
            id,
            functions: Vec::new(),
            structs: Vec::new(),
            vars: Vec::new(),
            scopes: Vec::new(),
            imported_modules: Vec::new(),
        }
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
        let id = FunctionId(Id {
            module: self.id,
            id: self.functions.len(),
        });
        function.id = Some(id);
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
        let id = StructId(Id {
            module: self.id,
            id: self.structs.len(),
        });
        struct_.id = Some(id);
        eprintln!("Adding struct: {:?}", struct_);
        self.structs.push(struct_);
        id
    }

    ////

    pub fn get_var(&self, id: usize) -> &Var {
        self.vars.get(id).expect("Var not found")
    }

    pub fn add_var(&mut self, mut var: Var) -> VarId {
        let id = VarId(Id {
            module: self.id,
            id: self.vars.len(),
        });
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
        let id = ScopeId(Id {
            module: self.id,
            id: self.scopes.len(),
        });
        scope.id = Some(id);
        self.scopes.push(scope);
        id
    }

    ////

    pub fn import(&mut self, imported: ModuleId) {
        self.imported_modules.push(imported);
    }
}

//// Program

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
            .get(id.0.module.0)
            .expect("Module not found")
            .get_function(id.0.id)
    }

    pub fn get_struct(&self, id: StructId) -> &Struct {
        self.modules
            .get(id.0.module.0)
            .expect("Module not found")
            .get_struct(id.0.id)
    }

    pub fn get_var(&self, id: VarId) -> &Var {
        self.modules
            .get(id.0.module.0)
            .expect("Module not found")
            .get_var(id.0.id)
    }

    pub fn get_scope(&self, id: ScopeId) -> &Scope {
        self.modules
            .get(id.0.module.0)
            .expect("Module not found")
            .get_scope(id.0.id)
    }

    pub fn get_scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        let module = self
            .modules
            .get_mut(id.0.module.0)
            .expect("Module not found");
        module.get_scope_mut(id.0.id)
    }
}

////////

pub struct TypeChecker {
    program: Program,
    errors: Vec<CompilationError>,
    prelude_id: Option<ModuleId>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            program: Program::new(),
            errors: vec![],
            prelude_id: None,
        }
    }

    fn add_prelude(&mut self) -> ModuleId {
        let parsed_module = match compiler::parse_module_from_file(
            // We are in ./rust/target/debug/elc
            // Prelude is in ./../lib/prelude.esl
            current_exe()
                .unwrap()
                .parent()
                .unwrap()
                .join("../../../lib/prelude.esl")
                .as_path(),
        ) {
            Ok(parsed_module) => parsed_module,
            Err(e) => {
                panic!("Failed to parse prelude: {:?}", e);
            }
        };

        let (_, prelude_mod_id) = self.program.add_module();

        let typechecker = TypeCheckerModule::new(self, prelude_mod_id, parsed_module.path);
        typechecker.typecheck(parsed_module.module);
        prelude_mod_id
    }

    pub fn typecheck(mut self, p_module: parser::Module) -> (Program, Vec<CompilationError>) {
        let prelude_id = self.add_prelude();
        self.prelude_id = Some(prelude_id);

        // create main module (this)
        let (module, main_module_id) = self.program.add_module();
        module.import(prelude_id);

        let tc = TypeCheckerModule::new(&mut self, main_module_id, p_module.source_path.clone());
        tc.typecheck(p_module);

        (self.program, self.errors)
    }
}

/// Typecheck given parser::Module into a ModuleId.
pub struct TypeCheckerModule<'tc> {
    tc: &'tc mut TypeChecker,
    module: ModuleId,
    path: PathBuf,
    self_struct: Option<StructId>,
}

impl<'tc> TypeCheckerModule<'tc> {
    fn new(tc: &'tc mut TypeChecker, module: ModuleId, path: PathBuf) -> Self {
        TypeCheckerModule {
            tc,
            module,
            path,
            self_struct: None,
        }
    }

    fn program_mut(&mut self) -> &mut Program {
        &mut self.tc.program
    }

    fn program(&self) -> &Program {
        &self.tc.program
    }

    fn this_module(&self) -> &Module {
        self.program().get_module(self.module)
    }

    fn this_module_mut(&mut self) -> &mut Module {
        let m = self.module;
        self.program_mut().get_module_mut(m)
    }

    fn lookup_function(&self, call: &FunctionCall) -> Option<&Function> {
        // Lookup function in this module and in imported modules
        // FIXME: Optimize
        // FIXME: We may want to introduce some namespace support, then
        // we won't search in imported modules anymore.
        let tm = self.this_module();
        fn is_global_func_with_name(f: &Function, name: &str) -> bool {
            f.struct_.is_none() && f.name == name
        }
        match call {
            FunctionCall::Global(func_name) => tm
                .functions()
                .find(|f| is_global_func_with_name(f, func_name))
                .or_else(|| {
                    tm.imported_modules
                        .iter()
                        .map(|m| self.program().get_module(*m))
                        .filter_map(|m| {
                            m.functions()
                                .find(|f| is_global_func_with_name(f, func_name))
                        })
                        .next()
                }),
            FunctionCall::Method(struct_id, method_name) => {
                let struct_ = self.program().get_struct(*struct_id);
                struct_
                    .methods
                    .iter()
                    .map(|&fid| self.program().get_function(fid))
                    .find(|f| f.name == *method_name)
            }
        }
    }

    fn lookup_struct(&self, name: &str) -> Option<&Struct> {
        // Lookup struct in this module and in imported modules
        // FIXME: Optimize
        // FIXME: We may want to introduce some namespace support, then
        // we won't search in imported modules anymore.
        let tm = self.this_module();
        tm.structs().find(|f| f.name == name).or_else(|| {
            tm.imported_modules
                .iter()
                .map(|m| self.program().get_module(*m))
                .filter_map(|m| m.structs().find(|f| f.name == name))
                .next()
        })
    }

    fn typecheck_type(
        &mut self,
        parser::TypeNode {
            type_: p_type,
            range,
        }: &parser::TypeNode,
    ) -> Option<Type> {
        match p_type {
            parser::Type::Simple(name) => match name.as_str() {
                "void" => Some(Type::Primitive(Primitive::Void)),
                "u32" => Some(Type::Primitive(Primitive::U32)),
                "char" => Some(Type::Primitive(Primitive::Char)),
                "bool" => Some(Type::Primitive(Primitive::Bool)),
                "string" => Some(Type::Primitive(Primitive::String)),
                "static_string" => Some(Type::Primitive(Primitive::StaticString)),
                "range" => Some(Type::Primitive(Primitive::Range)),
                "Self" => {
                    let Some(struct_) = self.self_struct else {
                        self.tc.errors.push(CompilationError::new(
                            // FIXME: This error message might be confusing when
                            // using Self explicitly
                            "Cannot add 'this' argument outside of struct".into(),
                            range.clone(),
                            self.path.clone(),
                        ));
                        return None;
                    };
                    Some(Type::Struct { id: struct_ })
                }
                _ => {
                    let struct_ = self.lookup_struct(name);
                    if let Some(struct_) = struct_ {
                        Some(Type::Struct {
                            id: struct_.id.unwrap(),
                        })
                    } else {
                        self.tc.errors.push(CompilationError::new(
                            format!("Unknown type '{}'", name),
                            range.clone(),
                            self.path.clone(),
                        ));
                        None
                    }
                }
            },
            parser::Type::SizedArray { value, size } => {
                let inner = self.typecheck_type(value)?;
                Some(Type::Array {
                    inner: Box::new(inner),
                    size: *size as usize,
                })
            }
        }
    }

    fn typecheck_imports(&mut self, p_module: &mut parser::Module) {
        for import in p_module.imports.drain(..) {
            let parser::Import { name } = import;
            let Ok(parsed_module) = compiler::parse_module_from_file(
                &p_module
                    .source_path
                    .with_file_name(name.clone())
                    .with_extension("esl"),
            ) else {
                self.tc.errors.push(CompilationError::new(
                    format!("Failed to import module '{}'", name),
                    Range { start: 0, end: 0 },
                    p_module.source_path.clone(),
                ));
                continue;
            };

            let prelude_id = self.tc.prelude_id.unwrap();
            let (imported_mod, imported_mod_id) = self.program_mut().add_module();
            imported_mod.import(prelude_id);

            let typechecker = TypeCheckerModule::new(self.tc, imported_mod_id, parsed_module.path);
            typechecker.typecheck(parsed_module.module);
            self.this_module_mut().import(imported_mod_id);
        }
    }

    fn typecheck_structs(
        &mut self,
        p_module: &mut parser::Module,
    ) -> Vec<(parser::StatementNode, FunctionId)> {
        let mut function_bodies_to_check = vec![];
        for decl in p_module.structs.drain(..) {
            let parser::Struct {
                name,
                fields,
                methods,
            } = decl;

            let struct_ = Struct {
                id: None,
                name: name.clone(),
                fields: vec![],  // to be filled later
                methods: vec![], // to be filled later
            };

            let struct_id = self.this_module_mut().add_struct(struct_);
            self.self_struct = Some(struct_id);

            let methods: Vec<_> = methods
                .into_iter()
                .map(|method| {
                    let (body, id) = self.typecheck_function_decl(method);
                    if let Some(body) = body {
                        function_bodies_to_check.push((body, id));
                    }
                    id
                })
                .collect();

            let fields = fields
                .iter()
                .map(|field| Field {
                    type_: self.typecheck_type(&field.type_),
                    name: field.name.clone(),
                })
                .collect();

            let struct_ = self.this_module_mut().get_struct_mut(struct_id.0.id);
            struct_.methods = methods;
            struct_.fields = fields;

            self.self_struct = None;
        }
        function_bodies_to_check
    }

    fn typecheck_function_decl(
        &mut self,
        decl: parser::FunctionDecl,
    ) -> (Option<parser::StatementNode>, FunctionId) {
        let parser::FunctionDecl {
            name,
            params,
            body,
            return_type,
        } = decl;

        let should_infer_return_type = return_type.is_none();
        let rt = return_type
            .as_ref()
            .and_then(|ty| self.typecheck_type(&ty))
            .or_else(|| {
                if should_infer_return_type {
                    if name == "main" {
                        Some(Type::Primitive(Primitive::U32))
                    } else {
                        Some(Type::Primitive(Primitive::Void))
                    }
                } else {
                    None
                }
            });
        let mut function = Function::new(name.clone(), self.this_module_mut());
        function.struct_ = self.self_struct;
        function.return_type = rt;
        for param in params {
            let type_ = self.typecheck_type(&param.type_);
            eprintln!("adding param: {:?} type={:?}", param.name, type_);
            function.add_param(
                Var::new(type_, param.name.clone(), false),
                self.this_module_mut(),
            );
        }
        let id = self.this_module_mut().add_function(function);
        body.map(|body| (Some(body), id)).unwrap_or((None, id))
    }

    fn typecheck_function_decls(
        &mut self,
        p_module: &mut parser::Module,
    ) -> Vec<(parser::StatementNode, FunctionId)> {
        let mut function_bodies_to_check = vec![];

        for decl in p_module.functions.drain(..) {
            let (Some(body), id) = self.typecheck_function_decl(decl) else {
                continue;
            };
            function_bodies_to_check.push((body, id));
        }
        function_bodies_to_check
    }

    fn typecheck_function_bodies(
        &mut self,
        function_bodies_to_check: Vec<(parser::StatementNode, FunctionId)>,
    ) {
        for (parsed, id) in function_bodies_to_check {
            let tc = TypeCheckerExecution::new(self, id);
            let errors = tc.typecheck(&parsed);
            self.tc.errors.extend(errors);
        }
    }

    pub fn typecheck(mut self, mut p_module: parser::Module) {
        self.typecheck_imports(&mut p_module);
        let mut bodies = self.typecheck_structs(&mut p_module);
        bodies.extend(self.typecheck_function_decls(&mut p_module));
        self.typecheck_function_bodies(bodies);
    }
}

/// Typechecker in execution scopes (e.g functions)
pub struct TypeCheckerExecution<'tc, 'tcm> {
    tcm: &'tcm mut TypeCheckerModule<'tc>,
    errors: Vec<CompilationError>,

    function: FunctionId,

    scope_stack: Vec<ScopeId>,
    loop_depth: usize,
}

#[derive(Debug)]
enum FunctionCall {
    Global(String),
    Method(StructId, String),
}

impl FunctionCall {
    fn to_string(&self, program: &Program) -> String {
        match self {
            FunctionCall::Global(name) => name.clone(),
            FunctionCall::Method(struct_id, name) => {
                let struct_ = program.get_struct(*struct_id);
                format!("{}::{}", struct_.name, name)
            }
        }
    }
}

impl<'tc, 'tcm> TypeCheckerExecution<'tc, 'tcm> {
    pub fn new(tcm: &'tcm mut TypeCheckerModule<'tc>, function: FunctionId) -> Self {
        TypeCheckerExecution {
            tcm,
            errors: vec![],
            function,
            scope_stack: vec![],
            loop_depth: 0,
        }
    }

    fn program(&self) -> &Program {
        self.tcm.program()
    }

    fn this_module(&self) -> &Module {
        self.tcm.this_module()
    }

    fn this_module_mut(&mut self) -> &mut Module {
        self.tcm.this_module_mut()
    }

    fn push_scope(&mut self) {
        let scope = self.tcm.this_module_mut().add_scope(Scope::new());
        self.scope_stack.push(scope);
    }

    fn pop_scope(&mut self) {
        self.scope_stack.pop();
    }

    fn current_scope(&self) -> ScopeId {
        *self.scope_stack.last().expect("no scope")
    }

    fn add_var_to_current_scope(&mut self, var: Var) -> VarId {
        let id: VarId = self.this_module_mut().add_var(var);
        let scope_id = self.current_scope().0.id;
        let scope = self.this_module_mut().get_scope_mut(scope_id);
        scope.vars.push(id);
        id
    }

    fn is_in_loop(&self) -> bool {
        self.loop_depth > 0
    }

    fn lookup_variable(&self, name: &str) -> Option<&Var> {
        for scope in self.scope_stack.iter().rev() {
            let scope = self.program().get_scope(*scope);
            for var_id in &scope.vars {
                let var = self.program().get_var(*var_id);
                if var.name == name {
                    return Some(var);
                }
            }
        }
        None
    }

    fn typecheck_expr_call<'a>(
        &mut self,
        call: FunctionCall,
        parsed_args: impl Iterator<Item = &'a parser::FunctionArg>,
        range: &Range<usize>,
    ) -> Expression {
        let typechecked_args = parsed_args
            .map(|arg| {
                (
                    self.typecheck_expression(&arg.value),
                    arg.value.range.clone(),
                )
            })
            .collect::<Vec<_>>();

        let Some(function) = self.tcm.lookup_function(&call) else {
            self.errors.push(CompilationError::new(
                format!("Function '{}' not found", call.to_string(self.program())),
                range.clone(),
                self.tcm.path.clone(),
            ));
            return Expression::Call {
                function_id: None,
                arguments: HashMap::new(),
            };
        };
        eprintln!(
            "lookup_function({:?}) = {:?} {:?} {:?}",
            call, function.id, function.name, function.struct_
        );
        let mut arguments = HashMap::new();
        // For now we just go in order.
        // TODO: handle kwargs.

        // FIXME: support varargs properly
        if function.should_use_print_vararg_hack() {
            let function_id = function.id.unwrap();

            // just push one by one with increasing
            // (nonsensical) var ids
            for (i, (expr, range)) in typechecked_args.into_iter().enumerate() {
                let Some(type_) = expr.type_(&self.program()) else {
                    self.errors.push(CompilationError::new(
                        format!("Argument {} to print function has invalid type", i),
                        range,
                        self.tcm.path.clone(),
                    ));
                    continue;
                };
                let var = Var::new(Some(type_), format!("arg{}", i), false);
                let id = self.this_module_mut().add_var(var);
                arguments.insert(id, expr);
            }

            return Expression::Call {
                function_id: Some(function_id),
                arguments,
            };
        }

        let function_id = function.id.unwrap();
        let params_scope_id = function.params_scope;

        for (i, (expr, range)) in typechecked_args.into_iter().enumerate() {
            let params_scope = self.program().get_scope(params_scope_id);
            let param = *(if let Some(p) = params_scope.vars.get(i) {
                p
            } else {
                self.errors.push(CompilationError::new(
                    "Too many arguments".into(),
                    range.clone(),
                    self.tcm.path.clone(),
                ));
                break;
            });
            let expr_type = expr.type_(&self.program());
            arguments.insert(param, expr);

            let var = self.program().get_var(param);
            if let (Some(param), Some(expr)) = (var.type_.clone(), expr_type) {
                if !self.is_type_convertible(&param, &expr) {
                    self.errors.push(CompilationError::new(
                        format!(
                            "Cannot convert '{}' to '{}' for argument {}",
                            expr.name(&self.program()),
                            param.name(&self.program()),
                            i
                        ),
                        range.clone(),
                        self.tcm.path.clone(),
                    ));
                }
            }
        }

        // Check if all required arguments were provided
        // FIXME: get rid of clone (shouldn't be a big deal for now)
        let params_scope_vars = self.program().get_scope(params_scope_id).vars.clone();
        for var_id in &params_scope_vars {
            if !arguments.contains_key(var_id) {
                let var = self.program().get_var(*var_id);
                self.errors.push(CompilationError::new(
                    format!(
                        "Missing argument '{}' in call to '{}'",
                        var.name,
                        call.to_string(self.program())
                    ),
                    range.clone(),
                    self.tcm.path.clone(),
                ));
            }
        }

        Expression::Call {
            function_id: Some(function_id),
            arguments,
        }
    }

    fn is_type_convertible(&mut self, to: &Type, from: &Type) -> bool {
        // Special case: EmptyArray can be converted to:
        // - any Array
        // - any struct (default init)
        if matches!(from, Type::Primitive(Primitive::EmptyArray))
            && matches!(to, Type::Array { .. } | Type::Struct { .. })
        {
            return true;
        }

        // Strict type everything else
        return to == from;
    }

    fn check_operator_types(
        &mut self,
        op: parser::BinaryOp,
        left: Type,
        right: Type,
        range: &Range<usize>,
    ) {
        match op.class() {
            BinOpClass::Multiplicative | BinOpClass::Additive => {
                if !matches!(left, Type::Primitive(Primitive::U32))
                    || !matches!(right, Type::Primitive(Primitive::U32))
                {
                    self.errors.push(CompilationError::new(
                        "Invalid types for arithmetic operator".into(),
                        range.clone(),
                        self.tcm.path.clone(),
                    ));
                }
            }
            BinOpClass::Comparison => {
                if left != right {
                    self.errors.push(CompilationError::new(
                        format!(
                            "Cannot compare different types ('{}' and '{}')",
                            left.name(self.program()),
                            right.name(self.program())
                        ),
                        range.clone(),
                        self.tcm.path.clone(),
                    ));
                }
            }
            BinOpClass::Assignment => {
                if !self.is_type_convertible(&left, &right) {
                    let right_name = right.name(&self.program());
                    let left_name = left.name(&self.program());
                    self.errors.push(CompilationError::new(
                        format!("Cannot assign '{}' to '{}'", right_name, left_name,),
                        range.clone(),
                        self.tcm.path.clone(),
                    ));
                }
            }
            BinOpClass::Logical => {
                if left != Type::Primitive(Primitive::Bool)
                    || right != Type::Primitive(Primitive::Bool)
                {
                    self.errors.push(CompilationError::new(
                        "Logical operator with non-bool types".into(),
                        range.clone(),
                        self.tcm.path.clone(),
                    ));
                }
            }
            BinOpClass::Range => {
                if left != Type::Primitive(Primitive::U32)
                    || right != Type::Primitive(Primitive::U32)
                {
                    self.errors.push(CompilationError::new(
                        "Range operator with non-u32 types".into(),
                        range.clone(),
                        self.tcm.path.clone(),
                    ));
                }
            }
        }
    }

    fn typecheck_name(&mut self, name: &str, range: &Range<usize>) -> Expression {
        if let Some(var) = self.lookup_variable(name) {
            Expression::VarRef {
                type_: var.type_.clone(),
                var_id: Some(var.id.expect("var without id")),
                mut_: var.mut_,
            }
        } else {
            self.errors.push(CompilationError::new(
                format!("Variable with name '{}' not found", name),
                range.clone(),
                self.tcm.path.clone(),
            ));
            Expression::VarRef {
                type_: None,
                var_id: None,
                mut_: true, // avoid more errors if someone tries to write to this
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
                parser::Expression::Name(name) => {
                    self.typecheck_expr_call(FunctionCall::Global(name.clone()), args.iter(), range)
                }
                parser::Expression::MemberAccess { object, member } => {
                    // Desugar object.method(a,b,c) to method(object, a, b, c)
                    let object_tc = self.typecheck_expression(object);
                    let struct_id = match object_tc.type_(self.program()) {
                        Some(Type::Struct { id }) => id,
                        Some(_) => {
                            self.errors.push(CompilationError::new(
                                format!(
                                    "Method call on non-struct type '{}'",
                                    object_tc
                                        .type_(self.program())
                                        .unwrap()
                                        .name(self.program())
                                ),
                                range.clone(),
                                self.tcm.path.clone(),
                            ));
                            return Expression::Call {
                                function_id: None,
                                arguments: HashMap::new(),
                            };
                        }
                        None => todo!(),
                    };

                    // create iterator which first yields `object`
                    // and then rest of args
                    let object_arg = parser::FunctionArg {
                        param: None,
                        value: parser::ExpressionNode {
                            // FIXME: Avoid cloning
                            expression: object.expression.clone(),
                            range: range.clone(),
                        },
                    };

                    let iter = std::iter::once(&object_arg).chain(args.iter());

                    self.typecheck_expr_call(
                        FunctionCall::Method(struct_id, member.clone()),
                        iter,
                        range,
                    )
                }
                _ => {
                    self.errors.push(CompilationError::new(
                        "Only function names are supported for now".into(),
                        function.range.clone(),
                        self.tcm.path.clone(),
                    ));
                    Expression::Call {
                        function_id: None,
                        arguments: HashMap::new(),
                    }
                }
            },
            parser::Expression::Index { indexable, index } => {
                let indexable_tc = self.typecheck_expression(indexable);
                let index_tc = self.typecheck_expression(index);

                // index must be u32 for now
                if let Some(index_type) = index_tc.type_(&self.program()) {
                    if index_type != Type::Primitive(Primitive::U32) {
                        self.errors.push(CompilationError::new(
                            "Index must be u32".into(),
                            index.range.clone(),
                            self.tcm.path.clone(),
                        ));
                    }
                }

                // indexable must be indexable
                if let Some(indexable_type) = indexable_tc.type_(&self.program()) {
                    if indexable_type.index_value_type(&self.program()).is_none() {
                        self.errors.push(CompilationError::new(
                            format!(
                                "'{}' is not indexable",
                                indexable_type.name(&self.program())
                            ),
                            indexable.range.clone(),
                            self.tcm.path.clone(),
                        ));
                    }
                }

                Expression::Index {
                    indexable: Box::new(indexable_tc),
                    index: Box::new(index_tc),
                }
            }
            parser::Expression::MemberAccess { object, member } => {
                let object_tc = self.typecheck_expression(object);

                // object must be a struct
                if let Some(object_type) = object_tc.type_(&self.program()) {
                    match object_type {
                        Type::Struct { id } => {
                            let struct_ = self.program().get_struct(id);
                            if let None = struct_.resolve_field(member) {
                                self.errors.push(CompilationError::new(
                                    "Member access on non-existing field".into(),
                                    range.clone(),
                                    self.tcm.path.clone(),
                                ));
                            }
                        }
                        _ => {
                            self.errors.push(CompilationError::new(
                                format!(
                                    "Field access on non-struct type '{}'",
                                    object_tc
                                        .type_(self.program())
                                        .unwrap()
                                        .name(self.program())
                                ),
                                object.range.clone(),
                                self.tcm.path.clone(),
                            ));
                        }
                    }
                }

                Expression::MemberAccess {
                    object: Box::new(object_tc),
                    member: member.clone(),
                }
            }
            parser::Expression::BoolLiteral { value } => Expression::BoolLiteral { value: *value },
            parser::Expression::IntLiteral { value } => {
                if *value > (u32::MAX as u64) {
                    self.errors.push(CompilationError::new(
                        format!("Integer literal {} is too large", value),
                        range.clone(),
                        self.tcm.path.clone(),
                    ));
                }
                Expression::IntLiteral { value: *value }
            }
            parser::Expression::StringLiteral { value } => Expression::StringLiteral {
                value: value.clone(),
            },
            parser::Expression::Name(name) => self.typecheck_name(name, range),
            parser::Expression::BinaryOp {
                op,
                op_range,
                left,
                right,
            } => {
                let left = self.typecheck_expression(left);
                let right = self.typecheck_expression(right);

                if matches!(op.class(), BinOpClass::Assignment) {
                    match left.value_type() {
                        ValueType::LValue => {}
                        ValueType::ConstLValue => {
                            self.errors.push(CompilationError::new(
                                "Cannot assign to non-mutable value".into(),
                                range.clone(),
                                self.tcm.path.clone(),
                            ));
                        }
                        ValueType::RValue => {
                            self.errors.push(CompilationError::new(
                                "Cannot assign to rvalue".into(),
                                range.clone(),
                                self.tcm.path.clone(),
                            ));
                        }
                    }
                }

                if let (Some(left_type), Some(right_type)) =
                    (left.type_(&self.program()), right.type_(&self.program()))
                {
                    self.check_operator_types(*op, left_type, right_type, op_range);
                } else {
                    self.errors.push(CompilationError::new(
                        "Invalid types for binary operator".into(),
                        range.clone(),
                        self.tcm.path.clone(),
                    ));
                }

                Expression::BinaryOp {
                    op: *op,
                    left: Box::new(left),
                    right: Box::new(right),
                }
            }
            parser::Expression::ArrayLiteral { values } => {
                if values.is_empty() {
                    Expression::ArrayLiteral {
                        value_type: None,
                        values: Vec::new(),
                    }
                } else {
                    let tc_values: Vec<Expression> = values
                        .iter()
                        .map(|v| self.typecheck_expression(v))
                        .collect();

                    // Use first element as type hint
                    let value_type = tc_values.first().map(|v| v.type_(&self.program())).unwrap();

                    // Check if all values have type convertible to the first one
                    for (i, v) in tc_values.iter().enumerate() {
                        if let Some(value_type) = &value_type {
                            if let Some(v_type) = v.type_(&self.program()) {
                                if !self.is_type_convertible(&value_type, &v_type) {
                                    self.errors.push(CompilationError::new(
                                        format!("Array literal value {} has invalid type", i),
                                        values[i].range.clone(),
                                        self.tcm.path.clone(),
                                    ));
                                }
                            }
                        }
                    }

                    Expression::ArrayLiteral {
                        value_type,
                        values: tc_values,
                    }
                }
            }
            parser::Expression::CharLiteral { value } => Expression::CharLiteral { value: *value },
            parser::Expression::This => self.typecheck_name("this", range),
        }
    }

    fn typecheck_statement(
        &mut self,
        parser::StatementNode { statement, range }: &parser::StatementNode,
    ) -> Statement {
        match statement {
            parser::Statement::Expression(expr) => {
                let expr = self.typecheck_expression(&expr);
                if !expr.can_be_discarded(&self.program()) {
                    self.errors.push(CompilationError::new(
                        "Unused expression result".into(),
                        range.clone(),
                        self.tcm.path.clone(),
                    ));
                }
                Statement::Expression(expr)
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
                let init_value = init_value
                    .as_ref()
                    .map(|expr| self.typecheck_expression(expr));

                let type_: Option<Type> = type_
                    .as_ref()
                    .and_then(|ty| self.tcm.typecheck_type(&ty))
                    .or(if let Some(init) = &init_value {
                        init.type_(&self.program())
                    } else {
                        None
                    });

                let init_value_type = init_value.as_ref().and_then(|e| e.type_(&self.program()));
                if let (Some(var_type), Some(init_type)) = (&type_, init_value_type) {
                    if !self.is_type_convertible(&var_type, &init_type) {
                        self.errors.push(CompilationError::new(
                            format!(
                                "Cannot convert '{}' to '{}' for variable initialization",
                                init_type.name(&self.program()),
                                var_type.name(&self.program())
                            ),
                            range.clone(),
                            self.tcm.path.clone(),
                        ));
                    }
                }

                let var = Var::new(type_, name.clone(), *mut_);
                let var_id = self.add_var_to_current_scope(var);

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
            parser::Statement::For {
                it_var,
                iterable,
                body,
            } => {
                let tc_iterable = self.typecheck_expression(iterable);
                let iter_type = tc_iterable.type_(&self.program());
                let iter_value_type: Option<Type> = tc_iterable
                    .type_(&self.program())
                    .and_then(|t| t.iter_value_type(&self.program()));

                if !iter_type.is_none() && iter_value_type.is_none() {
                    self.errors.push(CompilationError::new(
                        "For loop iterable must be an iterable type".into(),
                        iterable.range.clone(),
                        self.tcm.path.clone(),
                    ));
                }

                self.push_scope();
                self.loop_depth += 1;

                // iterable var
                let var = Var::new(iter_value_type, it_var.clone(), false);
                let var_id = self.add_var_to_current_scope(var);

                let body = self.typecheck_statement(body);

                self.loop_depth -= 1;
                self.pop_scope();

                Statement::For {
                    it_var: var_id,
                    iterable: tc_iterable,
                    body: Box::new(body),
                }
            }
            parser::Statement::If {
                condition,
                then_block,
                else_block,
            } => {
                let (condition, cond_range) = (
                    self.typecheck_expression(condition),
                    condition.range.clone(),
                );
                let condition_type = condition.type_(&self.program());
                if condition_type.is_some()
                    && !matches!(condition_type, Some(Type::Primitive(Primitive::Bool)))
                {
                    self.errors.push(CompilationError::new(
                        "If condition must be of type bool".into(),
                        cond_range,
                        self.tcm.path.clone(),
                    ));
                }
                let then_block = self.typecheck_statement(then_block);
                let else_block = else_block
                    .as_ref()
                    .map(|block| Box::new(self.typecheck_statement(block)));
                Statement::If {
                    condition,
                    then_block: Box::new(then_block),
                    else_block,
                }
            }
            parser::Statement::Break => {
                if !self.is_in_loop() {
                    self.errors.push(CompilationError::new(
                        "'break' outside of a loop".into(),
                        range.clone(),
                        self.tcm.path.clone(),
                    ))
                }
                Statement::Break
            }
            parser::Statement::Continue => {
                if !self.is_in_loop() {
                    self.errors.push(CompilationError::new(
                        "'continue' outside of a loop".into(),
                        range.clone(),
                        self.tcm.path.clone(),
                    ))
                }
                Statement::Continue
            }
            parser::Statement::While { condition, body } => {
                let (condition, cond_range) = (
                    self.typecheck_expression(condition),
                    condition.range.clone(),
                );
                let condition_type = condition.type_(&self.program());
                if condition_type.is_some()
                    && !matches!(condition_type, Some(Type::Primitive(Primitive::Bool)))
                {
                    self.errors.push(CompilationError::new(
                        "While condition must be of type bool".into(),
                        cond_range,
                        self.tcm.path.clone(),
                    ));
                }
                self.push_scope();
                self.loop_depth += 1;
                let body = self.typecheck_statement(body);
                self.loop_depth -= 1;
                self.pop_scope();
                Statement::While {
                    condition,
                    body: Box::new(body),
                }
            }
        }
    }

    fn typecheck(mut self, stmt: &parser::StatementNode) -> Vec<CompilationError> {
        self.scope_stack
            .push(self.program().get_function(self.function).params_scope);

        let stmt = self.typecheck_statement(stmt);
        let function_id: usize = self.function.0.id;
        self.this_module_mut().get_function_mut(function_id).body = Some(stmt);

        self.scope_stack.pop();

        self.errors
    }
}
