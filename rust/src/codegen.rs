use std::{cell::RefCell, collections::HashMap, io::Write, rc::Rc};

use log::debug;

use crate::{
    parser,
    sema::{self, Type},
};

pub struct CodeGen<'data> {
    out: Rc<RefCell<dyn Write>>,
    program: &'data sema::Program,
    tmp_var_counter: usize,
    local_var_counter: usize,
    variable_names: HashMap<sema::id::VarId, TmpVar>,
    current_function: Option<sema::id::FunctionId>,
    drop_scope_stack: Vec<DropScope>,
}

type IoResult<T> = std::result::Result<T, std::io::Error>;

#[allow(dead_code)] // FIXME: FirstArg not used yet
enum FunctionReturnMethod {
    None,     // Nothing is returned (void/unknown/noreturn)
    Return,   // Returned as C return value
    FirstArg, // Returned as C first arg
}

impl sema::Type {
    fn function_return_method(&self) -> FunctionReturnMethod {
        match self {
            sema::Type::Primitive(sema::Primitive::Void) => FunctionReturnMethod::None,
            sema::Type::Primitive(_) => FunctionReturnMethod::Return,
            sema::Type::Array { inner: _, size: _ } => todo!(),
            sema::Type::Slice {
                inner: _,
                mut_elements: _,
            } => todo!(),
            sema::Type::Struct { id: _ } => FunctionReturnMethod::Return,
            sema::Type::RawReference { inner: _ } => FunctionReturnMethod::Return,
        }
    }
}

fn escape_c(str: &str) -> String {
    // TODO
    str.into()
}

/// A "temporary variable" that stores some ESL value.
#[derive(Clone)]
struct TmpVar {
    name: String,
    /// If true, the tmpvar is represented in C as *pointer*
    /// to the actual object. This is used to represent results
    /// of lvalue expressions, like member accesses, for example:
    ///
    /// ```
    /// struct S { field: u32; }
    ///
    /// obj.field = 5;
    /// ```
    ///
    /// is codegen'd to:
    ///
    /// ```
    /// // eval `obj.field`
    /// u32* member = &obj.field;
    /// // eval `5`
    /// u32 tmp_5 = 5;
    /// // eval assignment
    /// *member = tmp_5;
    /// ```
    is_ptr: bool,
}

impl TmpVar {
    fn new(name: String, is_ptr: bool) -> Self {
        Self { name, is_ptr }
    }

    // Get expression that accesses the object stored in this tmpvar.
    fn access(&self) -> String {
        match self.is_ptr {
            true => format!("/*Deref is_ptr=true*/ *{}", self.name),
            false => self.name.clone(),
        }
    }

    // Get expression that accesses pointer to the the object stored in
    // this tmpvar.
    fn access_ptr(&self) -> String {
        match self.is_ptr {
            true => self.name.clone(),
            false => format!("/*Ref is_ptr=false*/ &{}", self.name),
        }
    }
}

#[derive(PartialEq, Eq)]
enum DropScopeType {
    Normal,
    Loop,
}

struct DropScope {
    tmp_vars: Vec<(TmpVar, sema::Type)>,
    type_: DropScopeType,
}

impl DropScope {
    fn add_tmp_var(&mut self, var: (TmpVar, sema::Type)) {
        self.tmp_vars.push(var);
    }
}

impl<'data> CodeGen<'data> {
    fn out(&self) -> std::cell::RefMut<'_, dyn Write> {
        self.out.borrow_mut()
    }

    fn mangled_function_name(&self, func: &sema::Function) -> String {
        let struct_prefix = func
            .struct_
            .map(|id| format!("$s${}$", self.program.get_struct(id).name.clone()))
            .unwrap_or("".into());
        if func.name == "main" {
            "$$esl_main".into()
        } else {
            struct_prefix + func.name.clone().as_str()
        }
    }

    fn emit_type(&mut self, ty: &sema::Type) -> IoResult<()> {
        match ty {
            sema::Type::Primitive(sema::Primitive::Void) => {
                self.out().write_all("void".as_bytes())?
            }
            sema::Type::Primitive(sema::Primitive::U32) => {
                self.out().write_all("esl_u32".as_bytes())?
            }
            sema::Type::Primitive(sema::Primitive::Char) => {
                self.out().write_all("esl_char".as_bytes())?
            }
            sema::Type::Primitive(sema::Primitive::Bool) => {
                self.out().write_all("esl_bool".as_bytes())?
            }
            sema::Type::Primitive(sema::Primitive::String) => {
                self.out().write_all("esl_string".as_bytes())?
            }
            sema::Type::Primitive(sema::Primitive::StaticString) => {
                self.out().write_all("esl_static_string".as_bytes())?
            }
            sema::Type::Primitive(sema::Primitive::Range) => {
                self.out().write_all("esl_range".as_bytes())?
            }
            sema::Type::Primitive(sema::Primitive::EmptyArray) => {
                self.out().write_all("void*".as_bytes())?
            }
            sema::Type::Array { inner, size } => {
                self.emit_type(inner)?;
                write!(self.out(), "[{}]", size)?;
            }
            sema::Type::Slice {
                inner: _,
                mut_elements: _,
            } => todo!(),
            sema::Type::Struct { id } => write!(self.out(), "struct{}", id.0.mangle())?,
            sema::Type::RawReference { inner } => {
                self.emit_type(inner)?;
                write!(self.out(), "/*ref*/ *")?;
            }
        };
        Ok(())
    }

    // like `int var` or `int var[5]`
    fn emit_var_decl_without_drop(
        &mut self,
        type_: &sema::Type,
        name: &str,
        value_type: sema::ValueType,
    ) -> IoResult<TmpVar> {
        match type_ {
            sema::Type::Primitive(..) | sema::Type::Struct { .. } => {
                self.emit_type(type_)?;
                match value_type {
                    sema::ValueType::LValue => {
                        write!(self.out(), " /*l-value*/ *{}", name)?;
                    }
                    sema::ValueType::ConstLValue => {
                        write!(self.out(), " /*const l-value*/ *{}", name)?;
                    }
                    sema::ValueType::RValue => {
                        write!(self.out(), " {}", name)?;
                    }
                }
                Ok(TmpVar::new(
                    name.into(),
                    !matches!(value_type, sema::ValueType::RValue),
                ))
            }
            sema::Type::Array { inner, size } => {
                self.emit_type(inner)?;
                write!(self.out(), " {}[{}]", name, size)?;
                Ok(TmpVar::new(
                    name.into(),
                    !matches!(value_type, sema::ValueType::RValue),
                ))
            }
            sema::Type::Slice {
                inner: _,
                mut_elements: _,
            } => todo!(),
            sema::Type::RawReference { inner } => {
                self.emit_type(inner)?;
                write!(self.out(), "/*ref*/ *{}", name)?;
                Ok(TmpVar::new(
                    name.into(),
                    !matches!(value_type, sema::ValueType::RValue),
                ))
            }
        }
    }

    fn emit_var_decl(
        &mut self,
        type_: &sema::Type,
        name: &str,
        value_type: sema::ValueType,
    ) -> IoResult<TmpVar> {
        let tmpvar = self.emit_var_decl_without_drop(type_, name, value_type)?;
        if let Some(scope) = self.drop_scope_stack.last_mut() {
            scope.add_tmp_var((tmpvar.clone(), type_.clone()));
        }
        Ok(tmpvar)
    }

    fn push_drop_scope(&mut self, type_: DropScopeType) {
        self.drop_scope_stack.push(DropScope {
            tmp_vars: Vec::new(),
            type_,
        });
    }

    fn emit_drop_scope(&self, scope: &DropScope) -> IoResult<()> {
        debug!("Emitting drop scope ({} vars)", scope.tmp_vars.len());
        for var in scope.tmp_vars.iter().rev() {
            self.emit_drop(var.clone())?;
        }
        Ok(())
    }

    fn pop_drop_scope(&mut self) -> IoResult<()> {
        let scope = self.drop_scope_stack.pop().expect("unmatched drop scope");
        self.emit_drop_scope(&scope)?;
        Ok(())
    }

    /// Removes a tmpvar from all drop scopes because it has been moved.
    fn mark_tmp_var_as_moved(&mut self, var: &TmpVar) {
        for scope in self.drop_scope_stack.iter_mut().rev() {
            if let Some(pos) = scope.tmp_vars.iter().position(|(v, _)| v.name == var.name) {
                scope.tmp_vars.remove(pos);
                break;
            }
        }
    }

    /// Drops tmpvars until a scope of the given type is found.
    /// The last scope of the given type is also dropped.
    /// No scopes are *popped*. This is used for `break` and `continue`.
    fn emit_drop_scope_until(&mut self, type_: DropScopeType) -> IoResult<()> {
        for scope in self.drop_scope_stack.iter().rev() {
            self.emit_drop_scope(scope)?;
            if scope.type_ == type_ {
                break;
            }
        }
        Ok(())
    }

    /// Drops all tmpvar scopes. No scopes are *popped*. This is used
    /// for `return`.
    fn emit_drop_all_scopes(&mut self) -> IoResult<()> {
        for scope in self.drop_scope_stack.iter().rev() {
            self.emit_drop_scope(scope)?;
        }
        Ok(())
    }

    // Emit C declaration of a temporary variable.
    // `value_type` in this context means that the C variable stores:
    // - RValue - an actual ESL object
    // - LValue - pointer to mutable ESL object
    // - ConstLValue - pointer to const ESL object
    fn emit_tmp_var(
        &mut self,
        ty: &sema::Type,
        debug: &str,
        value_type: sema::ValueType,
    ) -> IoResult<TmpVar> {
        self.tmp_var_counter += 1;

        let name = format!("$$tmp{}_{}", self.tmp_var_counter, debug.to_string());

        writeln!(self.out(), "    ")?;
        let tmpvar = self.emit_var_decl(ty, &name, value_type)?;
        writeln!(self.out(), ";")?;

        Ok(tmpvar)
    }

    fn emit_move(&mut self, from: &TmpVar, to: &str) -> IoResult<()> {
        writeln!(self.out(), "    /* move */ {} = {};", to, from.access())?;
        self.mark_tmp_var_as_moved(from);
        Ok(())
    }

    // Emit code for copying tmpvar into a C expression.
    fn emit_copy(&mut self, from: &TmpVar, to: &str, type_: &sema::Type) -> IoResult<()> {
        assert!(
            type_.is_copyable(self.program),
            "type '{}' is noncopyable",
            type_.name(self.program)
        );
        // Currently there is no custom copy support so we can just do a simple assignment.
        writeln!(self.out(), "    /*copy*/ {} = {};", to, from.access())?;
        Ok(())
    }

    fn emit_format_arg_eval(&mut self, arg: &sema::Expression) -> IoResult<String> {
        let tmp_var = self
            .emit_expression_eval(arg)?
            .expect("void expression used as print arg");
        let arg_var_name = format!("$$arg{}", self.tmp_var_counter);
        self.tmp_var_counter += 1;
        write!(self.out(), "    esl_format_arg {arg_var_name};")?;
        write!(
            self.out(),
            " {arg_var_name}.print = {};",
            match arg.type_(self.program) {
                Some(sema::Type::Primitive(sema::Primitive::Bool)) => "_esl_print_bool",
                Some(sema::Type::Primitive(sema::Primitive::Range)) => "_esl_print_range",
                Some(sema::Type::Primitive(sema::Primitive::StaticString)) =>
                    "_esl_print_static_string",
                Some(sema::Type::Primitive(sema::Primitive::String)) => "_esl_print_string",
                Some(sema::Type::Primitive(sema::Primitive::U32)) => "_esl_print_u32",
                _ => todo!(),
            }
        )?;
        writeln!(
            self.out(),
            " {arg_var_name}.data = {};",
            tmp_var.access_ptr()
        )?;
        Ok(arg_var_name)
    }

    fn emit_print_call(
        &mut self,
        args: &HashMap<sema::id::VarId, sema::Expression>,
    ) -> IoResult<()> {
        writeln!(self.out(), "    /*print()*/")?;

        if args.is_empty() {
            return Ok(());
        }

        // setup args (varid order)
        let mut args_in_varid_order = args.iter().collect::<Vec<_>>();
        args_in_varid_order.sort_by_key(|(varid, _)| *varid);

        // create fmtstr eval (first arg)
        let fmtstr_var_name = self
            .emit_expression_eval(args_in_varid_order.remove(0).1)?
            .expect("void expression passed as format string");

        // create esl_format_args
        let args_var_name = format!("$$args{}", self.tmp_var_counter);
        self.tmp_var_counter += 1;

        writeln!(
            self.out(),
            "    esl_format_arg {args_var_name}[{}];",
            args_in_varid_order.len()
        )?;

        // push all args in varid order
        for (i, (_, expr)) in args_in_varid_order.iter().enumerate() {
            let arg_var_name = self.emit_format_arg_eval(expr)?;
            writeln!(self.out(), "    {args_var_name}[{i}] = {arg_var_name};")?;
        }

        // call print
        writeln!(
            self.out(),
            "    _esl_print({}, {}, {args_var_name});",
            fmtstr_var_name.access(),
            args_in_varid_order.len(),
        )?;

        Ok(())
    }

    fn emit_expr_binary_op(
        &mut self,
        op: &parser::BinaryOp,
        left: &sema::Expression,
        right: &sema::Expression,
        out_type: &sema::Type,
    ) -> IoResult<Option<TmpVar>> {
        let left_tmp = self
            .emit_expression_eval(left)?
            .expect("void expression in bin op lhs");

        let right_tmp;
        if matches!(op.class(), parser::BinOpClass::Logical) {
            // Short-circuiting - evaluate right expr only if left_tmp is true (for and) or false (for or)
            let left_val_to_eval = match op {
                parser::BinaryOp::LogicalAnd => true,
                parser::BinaryOp::LogicalOr => false,
                _ => unreachable!(),
            };
            right_tmp = self.emit_tmp_var(
                &sema::Type::Primitive(sema::Primitive::Bool),
                "logic",
                sema::ValueType::RValue,
            )?;
            writeln!(
                self.out(),
                "    if({} == {}) {{",
                left_tmp.access(),
                left_val_to_eval
            )?;
            let eval_tmp = self.emit_expression_eval(&right)?.unwrap();
            self.emit_move(&eval_tmp, &right_tmp.access())?;
            writeln!(self.out(), "    }} else {{")?;
            writeln!(
                self.out(),
                "        {} = {};",
                right_tmp.access(),
                !left_val_to_eval
            )?;
            writeln!(self.out(), "    }}")?;
        } else {
            right_tmp = self
                .emit_expression_eval(right)?
                .expect("void expression in bin op rhs");
        }

        let left_type = left.type_(self.program).unwrap().mangle(self.program);
        let right_type = right.type_(self.program).unwrap().mangle(self.program);

        writeln!(self.out(), "// operator: {}", op.mangle())?;

        let is_assignment = matches!(op.class(), parser::BinOpClass::Assignment);
        if is_assignment {
            // op(lhs, rhs)
            let overload = format!("_esl_op{}_{}_{}", op.mangle(), left_type, right_type);
            writeln!(
                self.out(),
                "    {}({}, {});",
                overload,
                left_tmp.access_ptr(),
                right_tmp.access()
            )?;
            Ok(None)
        } else {
            // out = op(lhs, rhs)
            let tmp_var = self.emit_tmp_var(out_type, "binop", sema::ValueType::RValue)?;

            // select C overload
            let overload = format!("_esl_op{}_{}_{}", op.mangle(), left_type, right_type);

            writeln!(
                self.out(),
                "    {} = {}({}, {});",
                tmp_var.access(),
                overload,
                left_tmp.access(),
                right_tmp.access()
            )?;
            Ok(Some(tmp_var))
        }
    }

    // Emit appropriate drop procedure for a given tmp var.
    fn emit_drop(&self, (var, type_): (TmpVar, sema::Type)) -> IoResult<()> {
        writeln!(self.out(), "    // drop {}", var.name)?;
        match type_ {
            Type::Primitive(sema::Primitive::String) => {
                writeln!(self.out(), "    string_drop({});", var.access())?;
            }
            Type::Struct { id } => {
                // If it has `__drop__()`
                let struct_ = self.program.get_struct(id);
                if let Some(drop) = struct_.drop_method {
                    // Emit simplified to avoid recursion in drop scopes
                    let fnname = self.mangled_function_name(self.program.get_function(drop));
                    writeln!(self.out(), "    {}(&{});", fnname, var.access())?;
                }
            }
            _ => { /*noop */ }
        }
        Ok(())
    }

    fn emit_expr_call(
        &mut self,
        function_id: sema::id::FunctionId,
        arguments: &HashMap<sema::VarId, sema::Expression>,
    ) -> IoResult<Option<TmpVar>> {
        // var tmp_arg1 = eval_copy(arg1_expr)
        // var tmp_arg2 = eval_copy(arg2_expr)
        // ...
        // var rv = func(move(tmp_arg1), move(tmp_arg2), ...)
        // yield rv
        let func = self.program.get_function(function_id);

        if func.should_use_print_vararg_hack() {
            self.emit_print_call(arguments)?;
            return Ok(None);
        }

        // Emit argument evaluation
        let mut arg_tmps = Vec::new();
        let scope = self.program.get_scope(func.params_scope);
        for arg in &scope.vars {
            let argval = arguments
                .get(&arg)
                .expect("missing argument for function call");
            let var = self.emit_expression_eval_copy(argval)?;
            // The var is moved into the function call
            self.mark_tmp_var_as_moved(&var);
            arg_tmps.push(var);
        }

        // Emit function call
        let return_method = func.return_type.as_ref().unwrap().function_return_method();
        let c_function_name = self.mangled_function_name(&func);
        match return_method {
            FunctionReturnMethod::None => {
                write!(self.out(), "{}(", c_function_name)?;
                write!(
                    self.out(),
                    "{}",
                    arg_tmps
                        .iter()
                        .map(|a| a.access())
                        .collect::<Vec<_>>()
                        .join(", ")
                )?;
                writeln!(self.out(), ");")?;
                return Ok(None);
            }
            FunctionReturnMethod::Return => {
                let tmp_var = self.emit_tmp_var(
                    func.return_type.as_ref().unwrap(),
                    "rv",
                    sema::ValueType::RValue,
                )?;
                write!(self.out(), "{} = ", tmp_var.access())?;
                write!(self.out(), "{}(", c_function_name)?;
                write!(
                    self.out(),
                    "{}",
                    arg_tmps
                        .iter()
                        .map(|a| a.access())
                        .collect::<Vec<_>>()
                        .join(", ")
                )?;
                writeln!(self.out(), ");")?;
                return Ok(Some(tmp_var));
            }
            FunctionReturnMethod::FirstArg => {
                todo!();
            }
        }
    }

    // Returns tmp var with indexing result
    fn emit_expr_index(
        &mut self,
        indexable: &sema::Expression,
        index: &sema::Expression,
    ) -> IoResult<TmpVar> {
        let index_tmp_var = self.emit_expression_eval(index)?.unwrap();
        let indexable_tmp_var = self.emit_expression_eval(indexable)?.unwrap();

        let indexable_type = indexable.type_(self.program).unwrap();

        match indexable_type {
            sema::Type::Primitive(sema::Primitive::StaticString) => {
                let out_tmp_var = self.emit_tmp_var(
                    &sema::Type::Primitive(sema::Primitive::Char),
                    "index",
                    sema::ValueType::RValue,
                )?;
                // bounds check
                writeln!(
                    self.out(),
                    "    if ({} >= str_size({})) {{",
                    index_tmp_var.access(),
                    indexable_tmp_var.access()
                )?;
                writeln!(self.out(), "        _esl_panic(\"index out of bounds\");")?;
                writeln!(self.out(), "    }}")?;
                writeln!(
                    self.out(),
                    // FIXME: This is *very* wrong as it doesn't care about utf8 and stuff
                    "    {} = (esl_char) {{ .cp = {}[{}] }};",
                    out_tmp_var.access(),
                    indexable_tmp_var.access(),
                    index_tmp_var.access()
                )?;
                Ok(out_tmp_var)
            }
            sema::Type::Array { inner, size } => {
                let out_tmp_var = self.emit_tmp_var(&inner, "index", sema::ValueType::LValue)?;
                // bounds check
                writeln!(
                    self.out(),
                    "    if ({} >= {}) {{",
                    index_tmp_var.access(),
                    size
                )?;
                writeln!(self.out(), "        _esl_panic(\"index out of bounds\");")?;
                writeln!(self.out(), "    }}")?;
                writeln!(
                    self.out(),
                    "    {} = &{}[{}];",
                    out_tmp_var.access_ptr(),
                    indexable_tmp_var.access(),
                    index_tmp_var.access()
                )?;
                Ok(out_tmp_var)
            }
            _ => panic!("invalid indexable type"),
        }
    }

    fn emit_expr_member_access(
        &mut self,
        expr: &sema::Expression,
        object: &sema::Expression,
        member: &str,
    ) -> IoResult<TmpVar> {
        let object_tmp_var = self.emit_expression_eval(object)?.unwrap();
        let Some(member_type) = expr.type_(self.program) else {
            panic!("invalid member access expression: {:?}", expr);
        };
        let member_tmp_var =
            self.emit_tmp_var(&member_type, "member", expr.value_type(&self.program))?;
        writeln!(
            self.out(),
            "    {} = &({}).{};",
            member_tmp_var.access_ptr(),
            object_tmp_var.access(),
            member
        )?;
        return Ok(member_tmp_var);
    }

    fn emit_expr_bool_literal(&mut self, value: bool) -> IoResult<TmpVar> {
        let tmp_var = self.emit_tmp_var(
            &sema::Type::Primitive(sema::Primitive::Bool),
            "bool",
            sema::ValueType::RValue,
        )?;
        writeln!(self.out(), "{} = {};", tmp_var.access(), value)?;
        Ok(tmp_var)
    }

    fn emit_expr_int_literal(&mut self, value: u64) -> IoResult<TmpVar> {
        let tmp_var = self.emit_tmp_var(
            &sema::Type::Primitive(sema::Primitive::U32),
            "int",
            sema::ValueType::RValue,
        )?;
        writeln!(self.out(), "{} = {};", tmp_var.access(), value)?;
        return Ok(tmp_var);
    }

    fn emit_expr_string_literal(&mut self, value: &str) -> IoResult<TmpVar> {
        let tmp_var = self.emit_tmp_var(
            &sema::Type::Primitive(sema::Primitive::StaticString),
            "str",
            sema::ValueType::RValue,
        )?;
        writeln!(
            self.out(),
            "{} = \"{}\";",
            tmp_var.access(),
            escape_c(value)
        )?;
        return Ok(tmp_var);
    }

    fn emit_expr_var_ref(&mut self, var_id: sema::id::VarId) -> IoResult<TmpVar> {
        // in most cases we can just refer directly.
        return Ok(self.variable_names.get(&var_id).cloned().unwrap_or(TmpVar {
            name: format!("INVALID_VAR_ID_{}", var_id.0.mangle()),
            is_ptr: false,
        }));
    }

    fn emit_expr_array_literal(
        &mut self,
        expr: &sema::Expression,
        values: &[sema::Expression],
    ) -> IoResult<TmpVar> {
        let type_ = expr.type_(self.program).unwrap();
        let tmp_var = self.emit_tmp_var(&type_, "array", sema::ValueType::RValue)?;
        for (i, value) in values.iter().enumerate() {
            let value_tmp = self
                .emit_expression_eval(value)?
                .expect("void expression in array literal");
            writeln!(
                self.out(),
                "    ({})[{}] = {};",
                tmp_var.access(),
                i,
                value_tmp.access()
            )?;
        }
        Ok(tmp_var)
    }

    fn emit_expr_struct_literal(
        &mut self,
        struct_id: sema::id::StructId,
        fields: &HashMap<String, sema::Expression>,
    ) -> IoResult<TmpVar> {
        // Struct { field1: expr1, field2: expr2 } -->
        // structX tmp = {};
        // tmp.field1 = expr1;
        // tmp.field2 = expr2;
        let type_ = Type::Struct { id: struct_id };
        let tmp_var = self.emit_tmp_var(&type_, "struct", sema::ValueType::RValue)?;

        for (field_name, field_expr) in fields {
            let field_tmp_var = self.emit_expression_eval(field_expr)?;
            writeln!(
                self.out(),
                "    ({}).{} = {};",
                tmp_var.access(),
                field_name,
                field_tmp_var.unwrap().access()
            )?;
        }

        Ok(tmp_var)
    }

    fn emit_expr_char_literal(&mut self, value: char) -> IoResult<TmpVar> {
        let tmp_var = self.emit_tmp_var(
            &sema::Type::Primitive(sema::Primitive::Char),
            "char",
            sema::ValueType::RValue,
        )?;
        // FIXME: escape
        writeln!(
            self.out(),
            "{} = (esl_char) {{ .cp = {} }};",
            tmp_var.access(),
            value as u32
        )?;
        return Ok(tmp_var);
    }

    fn emit_expr_dereference(&mut self, pointer: &sema::Expression) -> IoResult<TmpVar> {
        let expr_tmp_var = self.emit_expression_eval(pointer)?;

        // We can refer directly to the eval'led tmpvar but
        // change type to lvalue.

        let name = expr_tmp_var.unwrap().name;
        writeln!(self.out(), "    // dereference expr tmpvar={}", name)?;
        Ok(TmpVar::new(name, true))
    }

    fn emit_expr_reference(
        &mut self,
        expr: &sema::Expression,
        value: &sema::Expression,
    ) -> IoResult<TmpVar> {
        writeln!(self.out(), "    // reference eval")?;
        let expr_tmp_var = self.emit_expression_eval(value)?;
        writeln!(self.out(), "    // reference tmpvar")?;
        let tmp_var = self.emit_tmp_var(
            &expr.type_(self.program).unwrap(),
            "ref",
            sema::ValueType::RValue,
        )?;

        writeln!(self.out(), "    // reference assignment")?;
        writeln!(
            self.out(),
            "    {} = &{};",
            tmp_var.access(),
            expr_tmp_var.unwrap().access(),
        )?;

        Ok(tmp_var)
    }

    // Emit ESL expression evaluation as C statements.
    // Returns local var name with result if applicable.
    // Emits *pointer* if expr is a (const) lvalue.
    fn emit_expression_eval(&mut self, expr: &sema::Expression) -> IoResult<Option<TmpVar>> {
        match expr {
            sema::Expression::Call {
                function_id,
                arguments,
            } => self.emit_expr_call(function_id.unwrap(), arguments),
            sema::Expression::Index { indexable, index } => {
                Ok(Some(self.emit_expr_index(indexable, index)?))
            }
            sema::Expression::MemberAccess { object, member } => {
                Ok(Some(self.emit_expr_member_access(expr, object, member)?))
            }
            sema::Expression::BoolLiteral { value } => {
                Ok(Some(self.emit_expr_bool_literal(*value)?))
            }
            sema::Expression::IntLiteral { value } => Ok(Some(self.emit_expr_int_literal(*value)?)),
            sema::Expression::StringLiteral { value } => {
                Ok(Some(self.emit_expr_string_literal(value)?))
            }
            sema::Expression::VarRef(var_id) => Ok(Some(self.emit_expr_var_ref(var_id.unwrap())?)),
            sema::Expression::BinaryOp { op, left, right } => {
                let out_type = expr.type_(self.program).unwrap();
                Ok(self.emit_expr_binary_op(op, left, right, &out_type)?)
            }
            sema::Expression::ArrayLiteral {
                value_type: _,
                values,
            } => Ok(Some(self.emit_expr_array_literal(expr, values)?)),
            sema::Expression::StructLiteral { struct_id, fields } => Ok(Some(
                self.emit_expr_struct_literal(struct_id.unwrap(), fields)?,
            )),
            sema::Expression::CharLiteral { value } => {
                Ok(Some(self.emit_expr_char_literal(*value)?))
            }
            sema::Expression::Dereference { pointer } => {
                Ok(Some(self.emit_expr_dereference(pointer)?))
            }
            sema::Expression::Reference { value } => {
                Ok(Some(self.emit_expr_reference(expr, value)?))
            }
        }
    }

    // Emit ESL expression into a new object (makes a copy if necessary)
    fn emit_expression_eval_copy(&mut self, expr: &sema::Expression) -> IoResult<TmpVar> {
        // var tmp = eval(expr);
        let tmp = self.emit_expression_eval(expr)?.unwrap();
        // if (tmp is not rvalue) {
        if expr.value_type(self.program) != sema::ValueType::RValue {
            // var tmp_copy = copy(tmp);
            let tmp_copy = self.emit_tmp_var(
                &expr.type_(self.program).unwrap(),
                "copy",
                sema::ValueType::RValue,
            )?;
            self.emit_copy(&tmp, &tmp_copy.name, &expr.type_(self.program).unwrap())?;
            // yield tmp_copy
            Ok(tmp_copy)
        } else {
            // yield tmp
            Ok(tmp)
        }
    }

    // Emit ESL expression written into `var`. Useful for initialization
    // (to avoid copy).
    //
    // Panics if expr is void.
    fn emit_expression_into(&mut self, expr: &sema::Expression, var: &TmpVar) -> IoResult<()> {
        match &expr {
            sema::Expression::ArrayLiteral { values, .. } => {
                for (i, value) in values.iter().enumerate() {
                    let value_tmp = self.emit_expression_eval_copy(value)?;
                    writeln!(
                        self.out(),
                        "    ({})[{}] = {};",
                        var.access(),
                        i,
                        value_tmp.access()
                    )?;
                }
                Ok(())
            }
            _ => {
                let tmp_var = self.emit_expression_eval_copy(expr)?;
                self.emit_move(&tmp_var, &var.name)
            }
        }
    }

    // Emit ESL statement as C statements.
    fn emit_statement(&mut self, block: &sema::Statement) -> IoResult<()> {
        match block {
            sema::Statement::Expression(expression) => {
                _ = {
                    self.push_drop_scope(DropScopeType::Normal);
                    self.emit_expression_eval(expression)?;
                    self.pop_drop_scope()?;
                }
            }
            sema::Statement::Block(statements) => {
                writeln!(self.out(), "{{")?;
                self.push_drop_scope(DropScopeType::Normal);
                for statement in statements {
                    self.emit_statement(statement)?;
                }
                self.pop_drop_scope()?;
                writeln!(self.out(), "}}")?;
            }
            sema::Statement::VarDecl { var: var_id, init } => {
                let var = self.program.get_var(*var_id);
                let var_name = format!(
                    "{}{}_{}",
                    var.name,
                    self.local_var_counter,
                    var_id.0.mangle()
                );
                self.local_var_counter += 1;
                let tmp_var = TmpVar::new(var_name.clone(), false);
                self.variable_names.insert(*var_id, tmp_var.clone());
                writeln!(
                    self.out(),
                    "    // let {}: {}",
                    var.name,
                    var.type_.as_ref().unwrap().name(self.program)
                )?;
                write!(self.out(), "    ")?;
                self.emit_var_decl(
                    var.type_.as_ref().unwrap(),
                    &var_name,
                    sema::ValueType::RValue,
                )?;
                writeln!(self.out(), ";")?;

                // init
                if let Some(init) = init {
                    self.emit_expression_into(init, &tmp_var)?;
                }
            }
            sema::Statement::Return(expression) => {
                // TODO: handle return by first arg
                if let Some(expr) = expression {
                    let tmp_var = self.emit_expression_eval(expr)?;
                    let tmp_var = tmp_var.unwrap();
                    self.mark_tmp_var_as_moved(&tmp_var); // Moving into return value
                    self.emit_drop_all_scopes()?;
                    write!(self.out(), "return {};", &tmp_var.access())?;
                } else {
                    self.emit_drop_all_scopes()?;
                    writeln!(self.out(), "return;")?;
                }
            }
            sema::Statement::If {
                condition,
                then_block,
                else_block,
            } => {
                let condition_tmp_var = self
                    .emit_expression_eval(condition)?
                    .expect("void expression in if condition");
                writeln!(self.out(), "    if ({}) {{", condition_tmp_var.access())?;
                self.emit_statement(&then_block)?;
                writeln!(self.out(), "    }}")?;
                if let Some(else_block) = else_block {
                    writeln!(self.out(), "    else {{")?;
                    self.emit_statement(&else_block)?;
                    writeln!(self.out(), "    }}")?;
                }
            }
            sema::Statement::Break => {
                self.emit_drop_scope_until(DropScopeType::Loop)?;
                writeln!(self.out(), "break;")?;
            }
            sema::Statement::Continue => {
                self.emit_drop_scope_until(DropScopeType::Loop)?;
                writeln!(self.out(), "continue;")?;
            }
            sema::Statement::Loop(body) => {
                writeln!(self.out(), "    while(true) {{")?;
                self.push_drop_scope(DropScopeType::Loop);
                self.emit_statement(body)?;
                self.pop_drop_scope()?;
                writeln!(self.out(), "    }}")?;
            }
        }
        Ok(())
    }

    fn emit_function_decl(&mut self, function: &sema::Function) -> IoResult<()> {
        writeln!(
            self.out(),
            "/* function {} (ID={}, params scope ID={})*/",
            self.mangled_function_name(function),
            function.id().0.mangle(),
            function.params_scope.0.mangle()
        )?;
        self.emit_type(function.return_type.as_ref().unwrap())?;

        write!(self.out(), " {}(", self.mangled_function_name(&function))?;
        let scope = self.program.get_scope(function.params_scope);
        for (i, param) in scope.vars.iter().enumerate() {
            if i > 0 {
                write!(self.out(), ", ")?;
            }

            // TODO: handle different ways to pass arguments
            let var = self.program.get_var(*param);
            self.emit_type(&var.type_.as_ref().unwrap())?;

            write!(self.out(), " {}", var.name)?;
        }
        writeln!(self.out(), ");\n")?;
        Ok(())
    }

    fn emit_function_impl(&mut self, function: &sema::Function) -> IoResult<()> {
        assert!(function.body.is_some());
        self.emit_type(&function.return_type.as_ref().expect("invalid type"))?;

        self.push_drop_scope(DropScopeType::Normal); // Params scope

        write!(self.out(), " {}(", self.mangled_function_name(&function))?;
        let scope = self.program.get_scope(function.params_scope);
        for (i, param) in scope.vars.iter().enumerate() {
            if i > 0 {
                write!(self.out(), ", ")?;
            }

            // TODO: handle different ways to pass arguments
            let var = self.program.get_var(*param);
            self.emit_type(&var.type_.as_ref().unwrap())?;

            let var_name = format!("{}{}_param", var.name, self.local_var_counter);
            write!(self.out(), " {}", var_name)?;
            self.local_var_counter += 1;
            let tmpvar = TmpVar::new(var_name, false);
            self.variable_names.insert(*param, tmpvar.clone());
            self.drop_scope_stack
                .last_mut()
                .unwrap()
                .add_tmp_var((tmpvar, var.type_.as_ref().unwrap().clone()));
        }
        writeln!(self.out(), ") {{")?;
        self.emit_statement(&function.body.as_ref().unwrap())?;

        self.pop_drop_scope()?;

        // return, for main function
        if function.name == "main" {
            writeln!(self.out(), "    return 0;")?;
        }
        writeln!(self.out(), "}}")?;

        Ok(())
    }

    fn emit_header(&mut self) -> IoResult<()> {
        writeln!(self.out(), "#include <esl_header.h>\n")?;
        Ok(())
    }

    fn emit_footer(&mut self) -> IoResult<()> {
        writeln!(self.out(), "int main() {{")?;
        writeln!(self.out(), "    return $$esl_main();")?;
        writeln!(self.out(), "}}")?;
        Ok(())
    }

    pub fn emit_program(&mut self) -> IoResult<()> {
        self.emit_header()?;

        for module in self.program.modules() {
            for struct_ in module.structs() {
                let struct_name = format!("struct{}", struct_.id.unwrap().0.mangle());
                writeln!(self.out(), "typedef struct _{} {{", struct_name)?;
                for field in struct_.fields.iter() {
                    self.emit_type(&field.type_.as_ref().unwrap())?;
                    writeln!(self.out(), " {};", field.name)?;
                }
                writeln!(self.out(), "}} {};", struct_name)?;
            }
        }
        for module in self.program.modules() {
            for func in module.functions() {
                self.emit_function_decl(func)?;
            }
        }
        for module in self.program.modules() {
            for func in module.functions() {
                if func.body.is_some() {
                    self.current_function = Some(func.id());
                    let e = self.emit_function_impl(func);
                    self.current_function = None;
                    e?;
                } else {
                    writeln!(self.out(), "/* function {} is external */", func.name)?;
                }
            }
        }

        self.emit_footer()?;
        Ok(())
    }

    pub fn new(out: Rc<RefCell<dyn Write>>, program: &'data sema::Program) -> Self {
        Self {
            out,
            program,
            variable_names: HashMap::new(),
            local_var_counter: 0,
            tmp_var_counter: 0,
            current_function: None,
            drop_scope_stack: Vec::new(),
        }
    }
}
