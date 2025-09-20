mod decl_scope;
pub mod expression;
pub mod function;
pub mod id;
pub mod module;
pub mod program;
pub mod statement;
pub mod struct_;
pub mod type_;

use std::{
    collections::{HashMap, HashSet},
    env::current_exe,
    ops::Range,
    path::PathBuf,
};

use log::{debug, info};
use multimap::MultiMap;

use crate::{
    compiler,
    error::CompilationError,
    parser::{self, BinOpClass, BinaryOp},
    types,
};

pub use expression::*;
pub use function::*;
pub use id::*;
pub use module::*;
pub use program::*;
pub use statement::*;
pub use struct_::*;
pub use type_::*;

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
        self.errors.extend(parsed_module.errors);

        let (_, prelude_mod_id) = self.program.add_module();

        let typechecker = TypeCheckerModule::new(self, prelude_mod_id, parsed_module.path);
        typechecker.typecheck(parsed_module.module);
        prelude_mod_id
    }

    pub fn typecheck(mut self, p_module: parser::Module) -> (Program, Vec<CompilationError>) {
        let prelude_id = self.add_prelude();
        self.prelude_id = Some(prelude_id);
        let prelude_decl_scope_id = self
            .program
            .get_module(prelude_id)
            .top_level_decl_scope()
            .id();

        // create main module (this)
        let (module, main_module_id) = self.program.add_module();
        module.flat_import(prelude_decl_scope_id);

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

    fn lookup_foreign_function(&self, func_name: &types::ScopedName) -> Option<&Function> {
        self.this_module()
            .foreign_decl_scopes
            .iter()
            .find_map(|sid| {
                let scope = self.program().get_decl_scope(*sid);
                scope
                    .lookup_function_rec(func_name, self.program())
                    .map(|id| self.program().get_function(id))
            })
    }

    fn lookup_function(&self, call: &FunctionCall) -> Option<&Function> {
        match call {
            FunctionCall::Global(func_name) => {
                let tm = self.this_module();
                tm.top_level_decl_scope()
                    .lookup_function_rec(func_name, self.program())
                    .map(|id| self.program().get_function(id))
                    .or_else(|| self.lookup_foreign_function(func_name))
            }
            FunctionCall::Method(struct_id, method_name) => {
                let struct_ = self.program().get_struct(*struct_id);
                debug!("Looking up method {} in struct {:?}", method_name, struct_);
                // FIXME: Optimize
                struct_
                    .methods
                    .iter()
                    .map(|&fid| self.program().get_function(fid))
                    .find(|f| f.name == *method_name)
            }
        }
    }

    fn lookup_foreign_struct(&self, name: &types::ScopedName) -> Option<&Struct> {
        debug!(
            "Module lookup foreign struct {} in {} foreign scopes",
            name,
            self.this_module().foreign_decl_scopes.len()
        );
        self.this_module()
            .foreign_decl_scopes
            .iter()
            .find_map(|sid| {
                let scope = self.program().get_decl_scope(*sid);
                scope
                    .lookup_struct_rec(name, self.program())
                    .map(|id| self.program().get_struct(id))
            })
    }

    fn lookup_struct(&self, name: &types::ScopedName) -> Option<&Struct> {
        debug!("Module lookup struct {}", name);
        let tm = self.this_module();
        tm.top_level_decl_scope()
            .lookup_struct_rec(name, self.program())
            .map(|id| self.program().get_struct(id))
            .or_else(|| self.lookup_foreign_struct(name))
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
                "usize" => Some(Type::Primitive(Primitive::USize)),
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
                    Some(Type::RawReference {
                        inner: Box::new(Type::Struct { id: struct_ }),
                    })
                }
                _ => {
                    // TODO: Support structs in namespaces (in parser)
                    let struct_ = self.lookup_struct(&types::ScopedName::new(vec![name.clone()]));

                    if let Some(struct_) = struct_ {
                        Some(Type::Struct {
                            id: struct_.id.unwrap(),
                        })
                    } else {
                        debug!("ERROR: Unknown type '{}'", name);
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

            self.tc.errors.extend(parsed_module.errors);

            let prelude_id = self.tc.prelude_id.unwrap();
            let prelude_decl_scope_id = self
                .program()
                .get_module(prelude_id)
                .top_level_decl_scope()
                .id();
            let (imported_mod, imported_mod_id) = self.program_mut().add_module();
            imported_mod.flat_import(prelude_decl_scope_id);
            let imported_tlds_id = imported_mod.top_level_decl_scope().id();

            let typechecker = TypeCheckerModule::new(self.tc, imported_mod_id, parsed_module.path);
            typechecker.typecheck(parsed_module.module);
            self.this_module_mut()
                .top_level_decl_scope_mut()
                .add_child_scope(name.clone(), imported_tlds_id);
        }
    }

    fn lookup_special_method(&mut self, struct_id: StructId, name: &str) -> Option<FunctionId> {
        let func = self.lookup_function(&FunctionCall::Method(struct_id, name.into()))?;
        let params_scope = self.program().get_scope(func.params_scope);
        // there should be only the 'this' param with type &Self
        if params_scope.vars.len() != 1 {
            self.tc.errors.push(CompilationError::new(
                format!(
                    "Special method '{}' should have exactly one 'this' parameter",
                    name
                ),
                Range { start: 0, end: 0 },
                self.path.clone(),
            ));
            return None;
        }
        let self_param = self.program().get_var(params_scope.vars[0]);
        let expected_self_param_type = Type::RawReference {
            inner: Box::new(Type::Struct { id: struct_id }),
        };
        if self_param.type_ != Some(expected_self_param_type) {
            self.tc.errors.push(CompilationError::new(
                format!(
                    "Special method '{}' 'this' parameter should have type of the struct",
                    name
                ),
                Range { start: 0, end: 0 },
                self.path.clone(),
            ));
            return None;
        }
        Some(func.id())
    }

    fn typecheck_structs(
        &mut self,
        p_module: &mut parser::Module,
    ) -> Vec<(StructId, parser::FunctionDecl)> {
        info!("typecheck_structs() @ {}", self.path.display());
        let mut methods_to_check = vec![];
        for decl in p_module.structs.drain(..) {
            let parser::Struct {
                extern_,
                name,
                fields,
                methods,
            } = decl;

            let tlds_id = self.this_module().top_level_decl_scope().id();
            debug!("add struct decl scope {}", name);
            let struct_decl_scope = self
                .this_module_mut()
                .add_child_decl_scope(name.clone(), tlds_id);

            let struct_ = Struct {
                id: None,
                parent_decl_scope: tlds_id,
                struct_decl_scope,
                name: name.clone(),
                is_extern: extern_,
                fields: vec![],  // to be filled later
                methods: vec![], // to be filled later
                drop_method: None,
            };

            let struct_id = self.this_module_mut().add_struct(struct_);
            self.self_struct = Some(struct_id);

            // methods
            methods_to_check.extend(methods.into_iter().map(|a| (struct_id, a)));

            // fields
            let fields = fields
                .iter()
                .map(|field| Field {
                    type_: self.typecheck_type(&field.type_),
                    name: field.name.clone(),
                })
                .collect();

            let struct_ = self
                .this_module_mut()
                .get_struct_mut(struct_id.0.id_in_module());
            struct_.fields = fields;

            self.self_struct = None;
        }
        methods_to_check
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

        let scope = self
            .self_struct
            .map(|s| self.program().get_struct(s).struct_decl_scope)
            .unwrap_or(self.this_module().top_level_decl_scope().id());

        let mut function = Function::new(name.clone(), scope, self.this_module_mut());
        function.struct_ = self.self_struct;
        function.return_type = rt;

        for param in params {
            let type_ = self.typecheck_type(&param.type_);
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
        methods: Vec<(StructId, parser::FunctionDecl)>,
    ) -> Vec<(parser::StatementNode, FunctionId)> {
        info!("typecheck_function_decls() @ {}", self.path.display());
        let mut function_bodies_to_check = vec![];

        for decl in p_module.functions.drain(..) {
            let (Some(body), id) = self.typecheck_function_decl(decl) else {
                continue;
            };
            function_bodies_to_check.push((body, id));
        }

        let mut methods_per_struct = MultiMap::new();
        for (struct_id, decl) in methods {
            self.self_struct = Some(struct_id);
            let (body, id) = self.typecheck_function_decl(decl);
            self.self_struct = None;
            if let Some(body) = body {
                function_bodies_to_check.push((body, id));
            }
            debug!(
                "adding method {:?} ({}) for struct id {:?}",
                id.0,
                self.program().get_function(id).name,
                struct_id.0
            );
            methods_per_struct.insert(struct_id, id);
        }

        // register methods in structs
        for (struct_id, method_ids) in methods_per_struct.into_iter() {
            let struct_ = self
                .this_module_mut()
                .get_struct_mut(struct_id.0.id_in_module());
            struct_.methods.extend(method_ids);
        }

        function_bodies_to_check
    }

    fn typecheck_structs_stage2(&mut self) {
        let struct_ids = self
            .this_module()
            .structs()
            .map(|s| s.id.unwrap())
            .collect::<Vec<_>>();
        for struct_id in struct_ids {
            // special methods
            let drop_method = self.lookup_special_method(struct_id, "__drop__");
            let struct_ = self
                .this_module_mut()
                .get_struct_mut(struct_id.0.id_in_module());
            struct_.drop_method = drop_method;
        }
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
        debug!("!!! Typecheck Module: {:?}", self.path);
        self.typecheck_imports(&mut p_module);
        let methods = self.typecheck_structs(&mut p_module);
        let bodies = self.typecheck_function_decls(&mut p_module, methods);
        self.typecheck_structs_stage2();
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
    Global(types::ScopedName),
    Method(StructId, String),
}

impl FunctionCall {
    fn to_string(&self, program: &Program) -> String {
        match self {
            FunctionCall::Global(name) => name.clone().to_string(),
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

    fn this_module_mut(&mut self) -> &mut Module {
        self.tcm.this_module_mut()
    }

    fn push_scope(&mut self) -> ScopeId {
        let scope = self.tcm.this_module_mut().add_scope(Scope::new());
        self.scope_stack.push(scope);
        scope
    }

    fn pop_scope(&mut self) {
        self.scope_stack.pop();
    }

    fn with_scope<Ret>(&mut self, callback: impl FnOnce(&mut Self) -> Ret) -> Ret {
        self.push_scope();
        let ret = callback(self);
        self.pop_scope();
        ret
    }

    fn current_scope(&self) -> ScopeId {
        *self.scope_stack.last().expect("no scope")
    }

    fn add_var_to_current_scope(&mut self, var: Var) -> VarId {
        let id: VarId = self.this_module_mut().add_var(var);
        let scope_id = self.current_scope().0.id_in_module();
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

    fn typecheck_call<'a>(
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

        debug!("Typechecking call to {}", call.to_string(self.program()));
        let Some(function) = self.tcm.lookup_function(&call) else {
            debug!(
                "ERROR: Function '{}' not found",
                call.to_string(self.program())
            );
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

        debug!("Looked up function: {:?}", function.name);

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
                let expr = self
                    .generate_implicit_conversion_expr(
                        expr.clone(),
                        &expr.default_repr_type(self.program()).unwrap(),
                    )
                    .unwrap();
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
        debug!(
            "Function params scope: {:?} (name={})",
            params_scope_id, function.name
        );

        for (i, (expr, range)) in typechecked_args.into_iter().enumerate() {
            let params_scope = self.program().get_scope(params_scope_id);
            let param = *(if let Some(p) = params_scope.vars.get(i) {
                p
            } else {
                self.errors.push(CompilationError::new(
                    format!(
                        "Too many arguments in call to '{}'",
                        call.to_string(self.program())
                    ),
                    range.clone(),
                    self.tcm.path.clone(),
                ));
                break;
            });

            let var = self.program().get_var(param);
            let param_type = var.type_.as_ref();
            if param_type.is_none() {
                arguments.insert(param, expr);
                continue;
            }
            let converted_expr =
                self.generate_implicit_conversion_expr(expr.clone(), param_type.unwrap());
            if let (Some(expr), Some(param_type)) = (converted_expr, param_type) {
                let expr_type = expr.type_(&self.program()).unwrap();
                let expr_value_type = expr.value_type(&self.program());
                arguments.insert(param, expr);

                // Enforce noncopyable types
                if expr_value_type != ValueType::RValue && !expr_type.is_copyable(self.program()) {
                    self.errors.push(CompilationError::new(
                        format!(
                            "Passing non-copyable type '{}' by value is not allowed (argument `{}`)",
                            param_type.name(&self.program()),
                            self.program().get_var(param).name
                        ),
                        range.clone(),
                        self.tcm.path.clone(),
                    ));
                }
            } else {
                let expr_type = expr.type_(&self.program());
                if let (Some(param_type), Some(expr_type)) = (param_type, expr_type) {
                    self.errors.push(CompilationError::new(
                        format!(
                            "Cannot convert '{}' to '{}' for argument {}",
                            expr_type.name(&self.program()),
                            param_type.name(&self.program()),
                            i
                        ),
                        range.clone(),
                        self.tcm.path.clone(),
                    ));
                    arguments.insert(param, expr);
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

    /// Returns index of an overload from `candidates` that best matches
    /// to `arg_types`. If no match found, returns None.
    /// The returned overload takes into account implicit conversions
    /// and default repr types, but exact matches are preferred.
    ///
    /// Returns: Overload index, converted exprs.
    fn select_overload(
        &self,
        candidates: &[Vec<Type>],
        args: &[Expression],
    ) -> Option<(usize, Vec<Expression>)> {
        debug!("Selecting overload for args: {:?}", args);

        // 1. first to try exact match
        let arg_types = args
            .iter()
            .map(|arg| arg.type_(&self.program()))
            .collect::<Option<Vec<_>>>()?;

        if let Some(s) = candidates
            .iter()
            .position(|overload| *overload == arg_types)
        {
            return Some((s, args.to_vec()));
        }

        // 2. try with conversion
        for (s, overload) in candidates.iter().enumerate() {
            if overload.len() != arg_types.len() {
                continue;
            }
            // Check if each arg is implicitly convertible to candidate's corresponding type
            let args = overload
                .iter()
                .zip(args.iter())
                .map(|(to, from)| self.generate_implicit_conversion_expr(from.clone(), to))
                .collect();
            if let Some(args) = args {
                debug!("overload found with conversion: {:?}", overload);
                return Some((s, args));
            }
        }

        // nothing found :(
        None
    }

    /// Used in cases where implicit conversion is allowed. E.g
    /// for Range constructor
    fn generate_implicit_conversion_expr(
        &self,
        from: Expression,
        to_type: &Type,
    ) -> Option<Expression> {
        let from_type = from.type_(&self.program())?;

        if from_type == *to_type {
            return Some(from); // nothing to do
        }

        match (from_type, to_type) {
            // empty array to any array
            (Type::Primitive(Primitive::EmptyArray), Type::Array { .. }) => {
                Some(Expression::ImplicitCast {
                    to: to_type.clone(),
                    expr: Box::new(from),
                })
            }
            (Type::Primitive(Primitive::LiteralInt), r) if r.is_integer() => {
                Some(Expression::ImplicitCast {
                    to: to_type.clone(),
                    expr: Box::new(from),
                })
            }
            _ => None,
        }
    }

    fn operator_overloads(op: parser::BinaryOp) -> Vec<Vec<Type>> {
        match op.class() {
            BinOpClass::Range => vec![
                // _esl_oprange_usize_usize
                vec![
                    Type::Primitive(Primitive::USize),
                    Type::Primitive(Primitive::USize),
                ],
            ],
            BinOpClass::Multiplicative | BinOpClass::Additive => vec![
                // _esl_op[add,sub,mul,div,mod]_u32_u32
                vec![
                    Type::Primitive(Primitive::U32),
                    Type::Primitive(Primitive::U32),
                ],
                // _esl_op[add,sub,mul,div,mod]_usize_usize
                vec![
                    Type::Primitive(Primitive::USize),
                    Type::Primitive(Primitive::USize),
                ],
            ],
            BinOpClass::Comparison => match op {
                parser::BinaryOp::CmpEquals => vec![
                    // _esl_opcmpeq_usize_usize
                    vec![
                        Type::Primitive(Primitive::USize),
                        Type::Primitive(Primitive::USize),
                    ],
                    // _esl_opcmpeq_u32_u32
                    vec![
                        Type::Primitive(Primitive::U32),
                        Type::Primitive(Primitive::U32),
                    ],
                    // _esl_opcmpeq_bool_bool
                    vec![
                        Type::Primitive(Primitive::Bool),
                        Type::Primitive(Primitive::Bool),
                    ],
                    // _esl_opcmpeq_char_char
                    vec![
                        Type::Primitive(Primitive::Char),
                        Type::Primitive(Primitive::Char),
                    ],
                ],
                _ => vec![
                    // _esl_opcmp[neq,lt,lte,gt,gte]_usize_usize
                    vec![
                        Type::Primitive(Primitive::USize),
                        Type::Primitive(Primitive::USize),
                    ],
                    // _esl_opcmp[neq,lt,lte,gt,gte]_u32_u32
                    vec![
                        Type::Primitive(Primitive::U32),
                        Type::Primitive(Primitive::U32),
                    ],
                    // _esl_opcmp[neq,lt,lte,gt,gte]_char_char
                    vec![
                        Type::Primitive(Primitive::Char),
                        Type::Primitive(Primitive::Char),
                    ],
                ],
            },
            BinOpClass::Logical => vec![
                // _esl_op[and,or]_bool_bool
                vec![
                    Type::Primitive(Primitive::Bool),
                    Type::Primitive(Primitive::Bool),
                ],
            ],
            BinOpClass::Assignment => {
                match op {
                    BinaryOp::Assignment => vec![], // handled separately
                    _ => vec![
                        // _esl_opass[add,sub,mul,div,mod]_usize_usize
                        vec![
                            Type::Primitive(Primitive::USize),
                            Type::Primitive(Primitive::USize),
                        ],
                        // _esl_opass[add,sub,mul,div,mod]_u32_u32
                        vec![
                            Type::Primitive(Primitive::U32),
                            Type::Primitive(Primitive::U32),
                        ],
                    ],
                }
            }
        }
    }

    fn generate_operand_conversion_exprs(
        &mut self,
        op: parser::BinaryOp,
        left: Expression,
        right: Expression,
        range: &Range<usize>,
    ) -> Option<(Expression, Expression)> {
        match op.class() {
            BinOpClass::Assignment => {
                // right must be implicitly convertible to left
                let right_converted = self.generate_implicit_conversion_expr(
                    right.clone(),
                    &left.type_(&self.program())?,
                );
                if let Some(right) = right_converted {
                    Some((left, right))
                } else {
                    self.errors.push(CompilationError::new(
                        format!(
                            "Cannot convert '{}' to '{}' in assignment",
                            right
                                .type_(&self.program())
                                .map(|t| t.name(self.program()))
                                .unwrap_or("unknown".into()),
                            left.type_(&self.program())
                                .map(|t| t.name(self.program()))
                                .unwrap_or("unknown".into()),
                        ),
                        range.clone(),
                        self.tcm.path.clone(),
                    ));
                    Some((left, right))
                }
            }
            _ => {
                let overloads = Self::operator_overloads(op);
                if let Some((_, converted_exprs)) =
                    self.select_overload(&overloads, &[left.clone(), right.clone()])
                {
                    Some((converted_exprs[0].clone(), converted_exprs[1].clone()))
                } else {
                    self.errors.push(CompilationError::new(
                        format!(
                            "No overload for operator '{}' with operand types '{}' and '{}'",
                            op.symbol(),
                            left.type_(&self.program())
                                .map(|t| t.name(self.program()))
                                .unwrap_or("unknown".into()),
                            right
                                .type_(&self.program())
                                .map(|t| t.name(self.program()))
                                .unwrap_or("unknown".into()),
                        ),
                        range.clone(),
                        self.tcm.path.clone(),
                    ));
                    None
                }
            }
        }
    }

    fn typecheck_variable_name(
        &mut self,
        name: &types::ScopedName,
        range: &Range<usize>,
    ) -> Expression {
        if let Some(name) = name.leaf() {
            if let Some(var) = self.lookup_variable(name) {
                Expression::VarRef(Some(var.id.expect("var without id")))
            } else {
                self.errors.push(CompilationError::new(
                    format!("Variable with name '{}' not found", name),
                    range.clone(),
                    self.tcm.path.clone(),
                ));
                Expression::VarRef(None)
            }
        } else {
            self.errors.push(CompilationError::new(
                format!("Scoped variables are not yet supported"),
                range.clone(),
                self.tcm.path.clone(),
            ));
            Expression::VarRef(None)
        }
    }

    fn typecheck_expr_call(
        &mut self,
        range: &Range<usize>,
        function: &parser::ExpressionNode,
        args: &Vec<parser::FunctionArg>,
    ) -> Expression {
        match &function.expression {
            parser::Expression::Name(name) => {
                self.typecheck_call(FunctionCall::Global(name.clone()), args.iter(), range)
            }
            parser::Expression::MemberAccess { object, member } => {
                // Desugar object.method(a,b,c) to method(object, a, b, c)
                let object_tc = self.typecheck_expression(object);
                let struct_id = match object_tc.type_(self.program()) {
                    Some(Type::Struct { id }) => id,
                    Some(Type::RawReference { inner: _ }) => {
                        // Desugar `(object.method)(a,b,c) to ((*object).method)(a,b,c)`
                        debug!("Dereference method {:#?}.{}", object, member);
                        return self.typecheck_expression(&parser::ExpressionNode {
                            expression: parser::Expression::Call {
                                function: Box::new(parser::ExpressionNode {
                                    expression: parser::Expression::MemberAccess {
                                        object: Box::new(parser::ExpressionNode {
                                            expression: parser::Expression::Dereference(
                                                object.clone(),
                                            ),
                                            range: object.range.clone(),
                                        }),
                                        member: member.clone(),
                                    },
                                    range: range.clone(),
                                }),
                                args: args.clone(),
                            },
                            range: range.clone(),
                        });
                    }
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
                    None => {
                        return Expression::Call {
                            function_id: None,
                            arguments: HashMap::new(),
                        };
                    }
                };

                // create iterator which first yields `object`
                // and then rest of args
                let object_arg = parser::FunctionArg {
                    param: None,
                    // &this
                    value: parser::ExpressionNode {
                        expression: parser::Expression::Reference(object.clone()),
                        range: object.range.clone(),
                    },
                };

                let iter = std::iter::once(&object_arg).chain(args.iter());

                self.typecheck_call(FunctionCall::Method(struct_id, member.clone()), iter, range)
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
        }
    }

    fn typecheck_expr_index(
        &mut self,
        indexable: &parser::ExpressionNode,
        index: &parser::ExpressionNode,
    ) -> Expression {
        let indexable_tc = self.typecheck_expression(indexable);
        let mut index_tc = self.typecheck_expression(index);
        let converted_index_tc = self.generate_implicit_conversion_expr(
            index_tc.clone(),
            &Type::Primitive(Primitive::USize),
        );

        // index must be implicit convertible to usize
        if let Some(converted_index_tc) = converted_index_tc {
            index_tc = converted_index_tc;
        } else {
            let index_type = index_tc.type_(&self.program());
            if let Some(index_type) = index_type {
                self.errors.push(CompilationError::new(
                    format!(
                        "Index must be `usize`, got '{}'",
                        index_type.name(self.program())
                    ),
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

    fn typecheck_expr_member_access(
        &mut self,
        range: &Range<usize>,
        object: &parser::ExpressionNode,
        member: &str,
    ) -> Expression {
        let object_tc = self.typecheck_expression(&object);

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
                Type::RawReference { inner: _ } => {
                    // Desugar `object.member` to `(*object).member` recursively
                    debug!("Dereference {:#?}.{}", object, member);
                    return self.typecheck_expression(&parser::ExpressionNode {
                        expression: parser::Expression::MemberAccess {
                            object: Box::new(parser::ExpressionNode {
                                expression: parser::Expression::Dereference(Box::new(
                                    object.clone(),
                                )),
                                range: object.range.clone(),
                            }),
                            member: member.into(),
                        },
                        range: range.clone(),
                    });
                }
                Type::Primitive(Primitive::Range) => {
                    // Hacky special-case: range.begin, range.end
                    if member != RANGE_BEGIN && member != RANGE_END {
                        self.errors.push(CompilationError::new(
                            format!(
                                "Range has only '{}' and '{}' fields",
                                RANGE_BEGIN, RANGE_END
                            ),
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
            member: member.into(),
        }
    }

    fn typecheck_expr_int_literal(&mut self, range: &Range<usize>, value: u64) -> Expression {
        if value > (u32::MAX as u64) {
            self.errors.push(CompilationError::new(
                format!("Integer literal {} is too large", value),
                range.clone(),
                self.tcm.path.clone(),
            ));
        }
        Expression::IntLiteral { value }
    }

    fn typecheck_expr_binary_op(
        &mut self,
        range: &Range<usize>,
        op: &BinaryOp,
        op_range: &Range<usize>,
        left: &parser::ExpressionNode,
        right: &parser::ExpressionNode,
    ) -> Expression {
        let left = self.typecheck_expression(left);
        let right = self.typecheck_expression(right);

        if matches!(op.class(), BinOpClass::Assignment) {
            match left.value_type(self.program()) {
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

            // Noncopyable
            if let Some(right_type) = right.type_(&self.program()) {
                if right.value_type(self.program()) != ValueType::RValue
                    && !right_type.is_copyable(&self.program())
                {
                    self.errors.push(CompilationError::new(
                        format!(
                            "Cannot assign to non-copyable type '{}'",
                            right_type.name(self.program())
                        ),
                        range.clone(),
                        self.tcm.path.clone(),
                    ));
                }
            }
        }

        if let (Some(_), Some(_)) = (left.type_(&self.program()), right.type_(&self.program())) {
            let Some((left_converted, right_converted)) =
                self.generate_operand_conversion_exprs(*op, left.clone(), right.clone(), op_range)
            else {
                return Expression::BinaryOp {
                    op: *op,
                    left: Box::new(left),
                    right: Box::new(right),
                };
            };
            Expression::BinaryOp {
                op: *op,
                left: Box::new(left_converted),
                right: Box::new(right_converted),
            }
        } else {
            // one of the sides has invalid type
            Expression::BinaryOp {
                op: *op,
                left: Box::new(left),
                right: Box::new(right),
            }
        }
    }

    fn typecheck_expr_array_literal(&mut self, values: &Vec<parser::ExpressionNode>) -> Expression {
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
            let value_type = tc_values
                .first()
                .map(|v| v.default_repr_type(&self.program()))
                .unwrap();

            // Check if all values have type convertible to the first one
            let mut converted_tc_values = Vec::new();
            if let Some(value_type) = &value_type {
                for (i, v) in tc_values.iter().enumerate() {
                    let converted_v =
                        self.generate_implicit_conversion_expr(v.clone(), &value_type.clone());
                    if let Some(v) = converted_v {
                        converted_tc_values.push(v);
                    } else {
                        self.errors.push(CompilationError::new(
                            format!("Array literal value {} has invalid type", i),
                            values[i].range.clone(),
                            self.tcm.path.clone(),
                        ));
                    }
                }
            }

            Expression::ArrayLiteral {
                value_type,
                values: tc_values,
            }
        }
    }

    fn typecheck_expr_struct_literal(
        &mut self,
        range: &Range<usize>,
        struct_name: &types::ScopedName,
        fields: &HashMap<String, parser::ExpressionNode>,
    ) -> Expression {
        // Typecheck field expressions
        let mut tc_fields = HashMap::new();
        for field in fields {
            let value_tc = self.typecheck_expression(&field.1);
            tc_fields.insert(field.0.clone(), (value_tc, field.1.range.clone()));
        }

        // Check if the struct exists
        let struct_ = self.tcm.lookup_struct(struct_name);
        if struct_.is_none() {
            self.errors.push(CompilationError::new(
                format!("Struct '{}' not found", struct_name),
                range.clone(),
                self.tcm.path.clone(),
            ));
            return Expression::StructLiteral {
                struct_id: None,
                fields: HashMap::new(),
            };
        }
        let struct_ = struct_.unwrap();
        let struct_id = struct_.id.unwrap();

        if struct_.is_extern {
            self.errors.push(CompilationError::new(
                format!(
                    "Cannot construct extern struct '{}' using struct literal",
                    struct_.name
                ),
                range.clone(),
                self.tcm.path.clone(),
            ));
            return Expression::StructLiteral {
                struct_id: None,
                fields: HashMap::new(),
            };
        }

        let mut uninitialized_fields = struct_
            .fields
            .iter()
            .map(|f| f.name.clone())
            .collect::<HashSet<_>>();

        // Check if the fields make sense in context of the struct
        let mut converted_tc_fields = HashMap::new();
        for (field_name, (expr, expr_range)) in &tc_fields {
            uninitialized_fields.remove(field_name);

            let field_def = struct_.resolve_field(&field_name);
            if field_def.is_none() {
                self.errors.push(CompilationError::new(
                    format!(
                        "Struct '{}' has no field named '{}'",
                        struct_.name, field_name
                    ),
                    expr_range.clone(),
                    self.tcm.path.clone(),
                ));
                continue;
            }
            let field_def = field_def.unwrap();

            if let (Some(value_type), Some(field_type)) =
                (expr.type_(&self.program()), &field_def.type_)
            {
                let expr_converted =
                    self.generate_implicit_conversion_expr(expr.clone(), field_type);
                if let Some(expr_converted) = expr_converted {
                    converted_tc_fields
                        .insert(field_name.clone(), (expr_converted, expr_range.clone()));
                } else {
                    self.errors.push(CompilationError::new(
                        format!(
                            "Cannot convert '{}' to '{}' for field '{}'",
                            value_type.name(self.program()),
                            field_type.name(self.program()),
                            field_name
                        ),
                        expr_range.clone(),
                        self.tcm.path.clone(),
                    ));
                }
            }
        }

        // Check if all fields were initialized
        if !uninitialized_fields.is_empty() {
            let mut fields_sorted: Vec<String> = uninitialized_fields.iter().cloned().collect();
            fields_sorted.sort();
            self.errors.push(CompilationError::new(
                        format!(
                            "Not all fields of struct '{}' were initialized (missing initializers for: '{}')",
                            struct_.name,
                            fields_sorted.into_iter().collect::<Vec<_>>().join("', '"),
                        ),
                        range.clone(),
                        self.tcm.path.clone(),
                    ));
        }

        Expression::StructLiteral {
            struct_id: Some(struct_id),
            fields: converted_tc_fields
                .into_iter()
                .map(|(k, (v, _))| (k, v))
                .collect(),
        }
    }

    fn typecheck_dereference(&mut self, pointer: &parser::ExpressionNode) -> Expression {
        let value = self.typecheck_expression(pointer);
        let ptr_type_name = value
            .type_(self.program())
            .map(|a| a.name(self.program()))
            .unwrap_or("<unknown>".into());
        let expr_tc = Expression::Dereference {
            pointer: Box::new(value),
        };
        if let None = expr_tc.type_(&self.program()) {
            self.errors.push(CompilationError::new(
                format!("Cannot dereference non-reference type `{}`", ptr_type_name),
                pointer.range.clone(),
                self.tcm.path.clone(),
            ));
            panic!();
        }
        expr_tc
    }

    fn typecheck_reference(&mut self, inner: &parser::ExpressionNode) -> Expression {
        let value = self.typecheck_expression(inner);
        Expression::Reference {
            value: Box::new(value),
        }
    }

    fn typecheck_expression(
        &mut self,
        parser::ExpressionNode { expression, range }: &parser::ExpressionNode,
    ) -> Expression {
        match &expression {
            parser::Expression::Call { function, args } => {
                self.typecheck_expr_call(&range, &function, &args)
            }
            parser::Expression::Index { indexable, index } => {
                self.typecheck_expr_index(indexable, index)
            }
            parser::Expression::MemberAccess { object, member } => {
                self.typecheck_expr_member_access(range, object, member)
            }
            parser::Expression::VoidLiteral => Expression::VoidLiteral,
            parser::Expression::BoolLiteral { value } => Expression::BoolLiteral { value: *value },
            parser::Expression::IntLiteral { value } => {
                self.typecheck_expr_int_literal(range, *value)
            }
            parser::Expression::StringLiteral { value } => Expression::StringLiteral {
                value: value.clone(),
            },
            parser::Expression::Name(name) => self.typecheck_variable_name(name, range),
            parser::Expression::BinaryOp {
                op,
                op_range,
                left,
                right,
            } => self.typecheck_expr_binary_op(range, op, op_range, left, right),
            parser::Expression::ArrayLiteral { values } => {
                self.typecheck_expr_array_literal(values)
            }
            parser::Expression::StructLiteral {
                struct_name,
                fields,
            } => self.typecheck_expr_struct_literal(range, struct_name, fields),
            parser::Expression::CharLiteral { value } => Expression::CharLiteral { value: *value },
            parser::Expression::This => {
                self.typecheck_variable_name(&types::ScopedName::new(vec!["this".into()]), range)
            }
            parser::Expression::Dereference(inner) => self.typecheck_dereference(inner),
            parser::Expression::Reference(inner) => self.typecheck_reference(inner),
        }
    }

    fn generate_iterator_new_call(&self, iterable_var: VarId) -> Expression {
        let iterable_type = self.program().get_var(iterable_var).type_.clone().unwrap();
        match iterable_type {
            Type::Array { inner: _, size: _ } => Expression::IntLiteral { value: 0 },
            Type::Primitive(Primitive::Range) => {
                // range.begin
                Expression::MemberAccess {
                    object: Box::new(Expression::VarRef(Some(iterable_var))),
                    member: RANGE_BEGIN.into(),
                }
            }
            _ => todo!(),
        }
    }

    fn generate_iterator_has_next_call(
        &self,
        iterable_var: VarId,
        iterator_var: VarId,
    ) -> Expression {
        let iterable_type = self.program().get_var(iterable_var).type_.clone().unwrap();
        match iterable_type {
            Type::Array { inner: _, size } => {
                // iterator < size
                Expression::BinaryOp {
                    op: BinaryOp::CmpLess,
                    left: Box::new(Expression::VarRef(Some(iterator_var))),
                    right: Box::new(Expression::ImplicitCast {
                        to: Type::Primitive(Primitive::USize),
                        expr: Box::new(Expression::IntLiteral { value: size as u64 }),
                    }),
                }
            }
            Type::Primitive(Primitive::Range) => {
                // iterator < range.end
                Expression::BinaryOp {
                    op: BinaryOp::CmpLess,
                    left: Box::new(Expression::VarRef(Some(iterator_var))),
                    right: Box::new(Expression::MemberAccess {
                        object: Box::new(Expression::VarRef(Some(iterable_var))),
                        member: RANGE_END.into(),
                    }),
                }
            }
            _ => todo!(),
        }
    }

    // Generate statements that will dereference iterator into `it_var`
    // and increment `iterator_var`
    // (basically `it_var = iterator_next(&iterator_var)` but we can't
    //  express this in the language yet because of lack of references)
    fn generate_iterator_next_statements(
        &self,
        iterable_var: VarId,
        it_var: VarId,
        iterator_var: VarId,
    ) -> Vec<Statement> {
        let mut statements = vec![];

        let iterable_type = self.program().get_var(iterable_var).type_.clone().unwrap();

        match iterable_type {
            Type::Primitive(Primitive::Range) => {
                // it_var = iterator_var
                statements.push(Statement::Expression(Expression::BinaryOp {
                    op: BinaryOp::Assignment,
                    left: Box::new(Expression::VarRef(Some(it_var))),
                    right: Box::new(Expression::VarRef(Some(iterator_var))),
                }));

                // iterator_var += 1
                statements.push(Statement::Expression(Expression::BinaryOp {
                    op: BinaryOp::AssAdd,
                    left: Box::new(Expression::VarRef(Some(iterator_var))),
                    right: Box::new(Expression::ImplicitCast {
                        to: Type::Primitive(Primitive::USize),
                        expr: Box::new(Expression::IntLiteral { value: 1 }),
                    }),
                }));
            }
            Type::Array { inner: _, size: _ } => {
                // it_var = iterable_var[iterator_var]
                statements.push(Statement::Expression(Expression::BinaryOp {
                    op: BinaryOp::Assignment,
                    left: Box::new(Expression::VarRef(Some(it_var))),
                    right: Box::new(Expression::Index {
                        indexable: Box::new(Expression::VarRef(Some(iterable_var))),
                        index: Box::new(Expression::VarRef(Some(iterator_var))),
                    }),
                }));

                // iterator_var += 1
                statements.push(Statement::Expression(Expression::BinaryOp {
                    op: BinaryOp::AssAdd,
                    left: Box::new(Expression::VarRef(Some(iterator_var))),
                    right: Box::new(Expression::ImplicitCast {
                        to: Type::Primitive(Primitive::USize),
                        expr: Box::new(Expression::IntLiteral { value: 1 }),
                    }),
                }));
            }
            _ => todo!(),
        }

        statements
    }

    fn typecheck_stmt_expression(
        &mut self,
        range: &Range<usize>,
        expr: &parser::ExpressionNode,
    ) -> Statement {
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

    fn typecheck_stmt_block(&mut self, stmts: &Vec<parser::StatementNode>) -> Statement {
        self.push_scope();
        let b = Statement::Block(stmts.iter().map(|s| self.typecheck_statement(s)).collect());
        self.pop_scope();
        b
    }

    fn typecheck_stmt_var_decl(
        &mut self,
        range: &Range<usize>,
        mut_: bool,
        type_: &Option<parser::TypeNode>,
        name: &str,
        init_value: &Option<parser::ExpressionNode>,
    ) -> Statement {
        let mut init_value = init_value
            .as_ref()
            .map(|expr| self.typecheck_expression(expr));

        let type_: Option<Type> = type_
            .as_ref()
            .and_then(|ty| self.tcm.typecheck_type(&ty))
            .or(if let Some(init) = &init_value {
                init.default_repr_type(&self.program())
            } else {
                None
            });

        debug!("var decl {} deduced type = {:?}", name, type_);

        let init_type = init_value.as_ref().and_then(|e| e.type_(&self.program()));
        if let (Some(var_type), Some(init_type)) = (&type_, init_type) {
            init_value =
                self.generate_implicit_conversion_expr(init_value.clone().unwrap(), &var_type);

            // If rhs is not temporary, we would make a copy - check
            // if type is copyable.
            if let Some(init_value) = &init_value {
                let init_value_type: ValueType = init_value.value_type(self.program());
                if init_value_type != ValueType::RValue && !init_type.is_copyable(self.program()) {
                    self.errors.push(CompilationError::new(
                        format!(
                            "Type '{}' is not copyable, cannot initialize variable from non-temporary value",
                            init_type.name(&self.program())
                        ),
                        range.clone(),
                        self.tcm.path.clone(),
                    ));
                }
            } else {
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

        let var = Var::new(type_, name.into(), mut_);
        let var_id = self.add_var_to_current_scope(var);

        Statement::VarDecl {
            var: var_id,
            init: init_value,
        }
    }

    fn typecheck_stmt_return(&mut self, expression: &Option<parser::ExpressionNode>) -> Statement {
        let expr = expression
            .as_ref()
            .map(|expr| self.typecheck_expression(expr))
            .unwrap_or(Expression::VoidLiteral);

        let Some(expected_return_type) = self
            .program()
            .get_function(self.function)
            .return_type
            .clone()
        else {
            return Statement::Return(expr);
        };
        let expr_converted =
            self.generate_implicit_conversion_expr(expr.clone(), &expected_return_type);

        if let Some(expr_converted) = expr_converted {
            Statement::Return(expr_converted)
        } else {
            self.errors.push(CompilationError::new(
                format!(
                    "Cannot convert '{}' to '{}' in return statement",
                    expr.type_(&self.program())
                        .map(|t| t.name(self.program()))
                        .unwrap_or("unknown".into()),
                    expected_return_type.name(self.program()),
                ),
                expression
                    .as_ref()
                    .map(|e| e.range.clone())
                    .unwrap_or_else(|| 0..0),
                self.tcm.path.clone(),
            ));
            Statement::Return(expr)
        }
    }

    fn typecheck_stmt_for(
        &mut self,
        it_var: &str,
        iterable: &parser::ExpressionNode,
        body: &parser::StatementNode,
    ) -> Statement {
        // Desugar
        // ```
        // for (let it_var of iterable) { body }
        // ```
        // into
        // ```
        // {
        //     let _iterable = iterable;
        //     let _iterator = <iterator>(_iterable);
        //     while (<has_next>(_iterator)) {
        //         let _it_var = <next>(_iterator);
        //         body;
        //     }
        // }
        // ```

        const ITERABLE: &str = "_$iterable";
        const ITERATOR: &str = "_$iterator";

        // {
        self.with_scope(|self_| {
            let mut statements = vec![];

            // let _iterable = iterable;
            let iterable_tc = self_.typecheck_expression(iterable);
            let iter_value_type: Option<Type> = iterable_tc
                .type_(&self_.program())
                .and_then(|t| t.iter_value_type(&self_.program()));

            let iterable_var = {
                match iterable_tc {
                    // Workaround for missing references... just refer
                    // to variables directly.
                    Expression::VarRef(Some(var_id)) => var_id,
                    _ => {
                        let Some(iterable_type) = iterable_tc.type_(&self_.program()) else {
                            return Statement::Block(vec![]);
                        };

                        if iter_value_type.is_none() {
                            self_.errors.push(CompilationError::new(
                                "For loop iterable must be an iterable type".into(),
                                iterable.range.clone(),
                                self_.tcm.path.clone(),
                            ));
                            return Statement::Block(vec![]);
                        }

                        let iterable_var = self_.add_var_to_current_scope(Var::new(
                            Some(iterable_type.clone()),
                            ITERABLE.into(),
                            false,
                        ));

                        statements.push(Statement::VarDecl {
                            var: iterable_var,
                            init: Some(iterable_tc),
                        });
                        iterable_var
                    }
                }
            };

            // let _iterator = <iterator_new>(_iterable);
            let iterator_init_tc = self_.generate_iterator_new_call(iterable_var);

            let iterator_var = self_.add_var_to_current_scope(Var::new(
                Some(Type::Primitive(Primitive::USize)),
                ITERATOR.into(),
                true,
            ));

            statements.push(Statement::VarDecl {
                var: iterator_var,
                init: Some(iterator_init_tc),
            });

            // while (<has_next>(_iterator)) {
            let while_ = self_.with_scope(|self_| {
                let mut iteration_statements = vec![];

                // if (!<has_next>(_iterator)) break;
                let iterator_has_next_tc =
                    self_.generate_iterator_has_next_call(iterable_var, iterator_var);

                iteration_statements.push(Statement::If {
                    // Note: `has_next == false` because we don't support
                    // negation of bools yet
                    condition: Expression::BinaryOp {
                        op: BinaryOp::CmpEquals,
                        left: Box::new(iterator_has_next_tc),
                        right: Box::new(Expression::BoolLiteral { value: false }),
                    },
                    then_block: Box::new(Statement::Break),
                    else_block: None,
                });

                // let _it_var = <next>(_iterator);
                let it_var = self_.add_var_to_current_scope(Var::new(
                    iter_value_type.clone(),
                    it_var.into(),
                    false,
                ));
                iteration_statements.push(Statement::VarDecl {
                    var: it_var,
                    init: None,
                });

                let iterator_next_stmts =
                    self_.generate_iterator_next_statements(iterable_var, it_var, iterator_var);

                iteration_statements.extend(iterator_next_stmts);

                // body;
                self_.loop_depth += 1;
                // (The only part controlled by user code)
                iteration_statements.push(self_.typecheck_statement(body));
                self_.loop_depth -= 1;

                Statement::Loop(Box::new(Statement::Block(iteration_statements)))
            });
            // }

            statements.push(while_);

            Statement::Block(statements)
        })
    }

    fn typecheck_stmt_if(
        &mut self,
        condition: &parser::ExpressionNode,
        then_block: &parser::StatementNode,
        else_block: &Option<Box<parser::StatementNode>>,
    ) -> Statement {
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

    fn typecheck_stmt_break(&mut self, range: &Range<usize>) -> Statement {
        if !self.is_in_loop() {
            self.errors.push(CompilationError::new(
                "'break' outside of a loop".into(),
                range.clone(),
                self.tcm.path.clone(),
            ))
        }
        Statement::Break
    }

    fn typecheck_stmt_continue(&mut self, range: &Range<usize>) -> Statement {
        if !self.is_in_loop() {
            self.errors.push(CompilationError::new(
                "'continue' outside of a loop".into(),
                range.clone(),
                self.tcm.path.clone(),
            ))
        }
        Statement::Continue
    }

    fn typecheck_stmt_while(
        &mut self,
        condition: &parser::ExpressionNode,
        body: &parser::StatementNode,
    ) -> Statement {
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

        // Desugar while(cond) {body} to while(true) { if (!cond) break; body }
        let iteration_stmt = self.with_scope(|self_| {
            self_.loop_depth += 1;
            let mut statements = vec![];

            statements.push(Statement::If {
                // Note: `condition == false` because we don't support
                // negation of bools yet
                condition: Expression::BinaryOp {
                    op: BinaryOp::CmpEquals,
                    left: Box::new(condition),
                    right: Box::new(Expression::BoolLiteral { value: false }),
                },
                then_block: Box::new(Statement::Break),
                else_block: None,
            });

            let body = self_.typecheck_statement(body);
            statements.push(body);

            self_.loop_depth -= 1;
            Statement::Block(statements)
        });
        Statement::Loop(Box::new(iteration_stmt))
    }

    fn typecheck_statement(
        &mut self,
        parser::StatementNode { statement, range }: &parser::StatementNode,
    ) -> Statement {
        match statement {
            parser::Statement::Expression(expr) => self.typecheck_stmt_expression(range, expr),
            parser::Statement::Block(stmts) => self.typecheck_stmt_block(stmts),
            parser::Statement::VarDecl {
                mut_,
                type_,
                name,
                init_value,
            } => self.typecheck_stmt_var_decl(range, *mut_, type_, name, init_value),
            parser::Statement::Return(expression) => self.typecheck_stmt_return(expression),
            parser::Statement::For {
                it_var,
                iterable,
                body,
            } => self.typecheck_stmt_for(it_var, iterable, body),
            parser::Statement::If {
                condition,
                then_block,
                else_block,
            } => self.typecheck_stmt_if(condition, then_block, else_block),
            parser::Statement::Break => self.typecheck_stmt_break(range),
            parser::Statement::Continue => self.typecheck_stmt_continue(range),
            parser::Statement::While { condition, body } => {
                self.typecheck_stmt_while(condition, body)
            }
        }
    }

    fn check_missing_returns(&mut self, range: Range<usize>, stmt: &Statement) {
        if stmt.return_type(self.program()).is_none() {
            let func = self.program().get_function(self.function);
            // It's ok for void.
            if func.return_type == Some(Type::Primitive(Primitive::Void)) {
                return;
            }
            // It's ok for main (implicit int return type)
            if func.name == "main" {
                return;
            }
            self.errors.push(CompilationError::new(
                "Not all control paths return a value".into(),
                range,
                self.tcm.path.clone(),
            ));
        }
    }

    fn typecheck(mut self, stmt: &parser::StatementNode) -> Vec<CompilationError> {
        self.scope_stack
            .push(self.program().get_function(self.function).params_scope);

        // FIXME: This range is bleh (covers the entire function!)
        let stmt_range = stmt.range.clone();
        let stmt = self.typecheck_statement(stmt);
        let function_id: usize = self.function.0.id_in_module();

        self.check_missing_returns(stmt_range, &stmt);

        self.this_module_mut().get_function_mut(function_id).body = Some(stmt);

        self.scope_stack.pop();

        self.errors
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn select_overload() {
        let mut tc = TypeChecker::new();
        let mut tcm = TypeCheckerModule::new(&mut tc, ModuleId(0), "test".into());
        let tce = TypeCheckerExecution::new(&mut tcm, FunctionId(Id::new(ModuleId(0), 0)));

        let overloads = [
            vec![
                Type::Primitive(Primitive::U32),
                Type::Primitive(Primitive::U32),
            ],
            vec![
                Type::Primitive(Primitive::USize),
                Type::Primitive(Primitive::USize),
            ],
        ];

        let u32_expr = Expression::ImplicitCast {
            to: Type::Primitive(Primitive::U32),
            expr: Box::new(Expression::IntLiteral { value: 42 }),
        };
        let usize_expr = Expression::ImplicitCast {
            to: Type::Primitive(Primitive::USize),
            expr: Box::new(Expression::IntLiteral { value: 42 }),
        };
        let literal_int_expr = Expression::IntLiteral { value: 42 };

        assert_eq!(
            tce.select_overload(&overloads, &[u32_expr.clone(), u32_expr.clone()]),
            Some((0, vec![u32_expr.clone(), u32_expr.clone()]))
        );
        assert_eq!(
            tce.select_overload(&overloads, &[usize_expr.clone(), usize_expr.clone()]),
            Some((1, vec![usize_expr.clone(), usize_expr.clone()]))
        );
        assert_eq!(
            tce.select_overload(
                &overloads,
                &[literal_int_expr.clone(), literal_int_expr.clone()],
            ),
            Some((0, vec![u32_expr.clone(), u32_expr.clone()])) // prefer first overload
        );
    }
}
