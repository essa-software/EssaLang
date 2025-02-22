use std::{collections::HashMap, io::Write};

use crate::{parser, sema};

pub struct CodeGen<'data> {
    out: &'data mut dyn Write,
    program: &'data sema::Program,
    tmp_var_counter: usize,
    local_var_counter: usize,
    variable_names: HashMap<sema::VarId, TmpVar>,
}

type IoResult<T> = std::result::Result<T, std::io::Error>;

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
            sema::Type::Array { inner, size } => todo!(),
            sema::Type::Function { function } => todo!(),
            sema::Type::Slice {
                inner,
                mut_elements,
            } => todo!(),
            sema::Type::Struct { id } => FunctionReturnMethod::FirstArg,
        }
    }
}

fn escape_c(str: &String) -> String {
    // TODO
    str.clone()
}

#[derive(Clone)]
struct TmpVar {
    name: String,
    is_ptr: bool,
}

impl TmpVar {
    fn new(name: String, is_ptr: bool) -> Self {
        Self { name, is_ptr }
    }

    fn access(&self) -> String {
        match self.is_ptr {
            true => format!("*{}", self.name),
            false => self.name.clone(),
        }
    }

    fn access_ptr(&self) -> String {
        match self.is_ptr {
            true => self.name.clone(),
            false => format!("&{}", self.name),
        }
    }
}

impl<'data> CodeGen<'data> {
    fn mangled_function_name(&self, func: &sema::Function) -> String {
        if func.name == "main" {
            "$$esl_main".into()
        } else {
            func.name.clone()
        }
    }

    fn emit_type(&mut self, ty: &sema::Type) -> IoResult<()> {
        match ty {
            sema::Type::Primitive(sema::Primitive::Void) => {
                self.out.write_all("void".as_bytes())?
            }
            sema::Type::Primitive(sema::Primitive::U32) => {
                self.out.write_all("esl_u32".as_bytes())?
            }
            sema::Type::Primitive(sema::Primitive::Bool) => {
                self.out.write_all("esl_bool".as_bytes())?
            }
            sema::Type::Primitive(sema::Primitive::String) => {
                self.out.write_all("esl_string".as_bytes())?
            }
            sema::Type::Primitive(sema::Primitive::StaticString) => {
                self.out.write_all("esl_static_string".as_bytes())?
            }
            sema::Type::Primitive(sema::Primitive::Range) => {
                self.out.write_all("esl_range".as_bytes())?
            }
            sema::Type::Primitive(sema::Primitive::EmptyArray) => {
                self.out.write_all("void*".as_bytes())?
            }
            sema::Type::Array { inner, size } => {
                self.emit_type(inner)?;
                write!(self.out, "[{}]", size)?;
            }
            sema::Type::Function { function } => todo!(),
            sema::Type::Slice {
                inner,
                mut_elements,
            } => todo!(),
            sema::Type::Struct { id } => write!(self.out, "struct{}", id.0.mangle())?,
        };
        Ok(())
    }

    // like `int var` or `int var[5]`
    fn emit_var_decl(
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
                        write!(self.out, " *{}", name)?;
                    }
                    sema::ValueType::ConstLValue => {
                        write!(self.out, " const *{}", name)?;
                    }
                    sema::ValueType::RValue => {
                        write!(self.out, " {}", name)?;
                    }
                }
                Ok(TmpVar::new(
                    name.into(),
                    !matches!(value_type, sema::ValueType::RValue),
                ))
            }
            sema::Type::Array { inner, size } => {
                self.emit_type(inner)?;
                write!(self.out, " {}[{}]", name, size)?;
                Ok(TmpVar::new(
                    name.into(),
                    !matches!(value_type, sema::ValueType::RValue),
                ))
            }
            sema::Type::Function { function } => todo!(),
            sema::Type::Slice {
                inner,
                mut_elements,
            } => todo!(),
        }
    }

    // Returns a name.
    fn emit_tmp_var_c(&mut self, c_type: &str, debug: &str) -> IoResult<String> {
        self.tmp_var_counter += 1;
        let name = format!("$$tmp{}_{}", self.tmp_var_counter, debug.to_string());
        writeln!(self.out, "    {} {};", c_type, name)?;
        Ok(name)
    }

    fn emit_tmp_var(
        &mut self,
        ty: &sema::Type,
        debug: &str,
        value_type: sema::ValueType,
    ) -> IoResult<TmpVar> {
        self.tmp_var_counter += 1;

        let name = format!("$$tmp{}_{}", self.tmp_var_counter, debug.to_string());

        writeln!(self.out, "    ")?;
        let tmpvar = self.emit_var_decl(ty, &name, value_type)?;
        writeln!(self.out, ";")?;

        Ok(tmpvar)
    }

    fn emit_format_arg_eval(&mut self, arg: &sema::Expression) -> IoResult<String> {
        let tmp_var = self
            .emit_expression_eval(arg)?
            .expect("void expression used as print arg");
        let arg_var_name = format!("$$arg{}", self.tmp_var_counter);
        self.tmp_var_counter += 1;
        write!(self.out, "    esl_format_arg {arg_var_name};")?;
        write!(
            self.out,
            " {arg_var_name}.print = {};",
            match arg.type_(self.program) {
                Some(sema::Type::Primitive(sema::Primitive::Bool)) => "_esl_print_bool",
                Some(sema::Type::Primitive(sema::Primitive::StaticString)) => "_esl_print_u32",
                Some(sema::Type::Primitive(sema::Primitive::U32)) => "_esl_print_u32",
                Some(sema::Type::Primitive(sema::Primitive::Range)) => "_esl_print_range",
                _ => todo!(),
            }
        )?;
        writeln!(self.out, " {arg_var_name}.data = {};", tmp_var.access_ptr())?;
        Ok(arg_var_name)
    }

    fn emit_print_call(&mut self, args: &HashMap<sema::VarId, sema::Expression>) -> IoResult<()> {
        writeln!(self.out, "    /*print()*/")?;

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
            self.out,
            "    esl_format_arg {args_var_name}[{}];",
            args_in_varid_order.len()
        )?;

        // push all args in varid order
        for (i, (_, expr)) in args_in_varid_order.iter().enumerate() {
            let arg_var_name = self.emit_format_arg_eval(expr)?;
            writeln!(self.out, "    {args_var_name}[{i}] = {arg_var_name};")?;
        }

        // call print
        writeln!(
            self.out,
            "    _esl_print({}, {}, {args_var_name});",
            fmtstr_var_name.access(),
            args_in_varid_order.len(),
        )?;

        Ok(())
    }

    fn emit_binary_op(
        &mut self,
        op: &parser::BinaryOp,
        left: &sema::Expression,
        right: &sema::Expression,
        out_type: &sema::Type,
    ) -> IoResult<Option<TmpVar>> {
        let left_tmp = self
            .emit_expression_eval(left)?
            .expect("void expression in bin op lhs");
        let right_tmp = self
            .emit_expression_eval(right)?
            .expect("void expression in bin op rhs");

        let left_type = left.type_(self.program).unwrap().mangle(self.program);
        let right_type = right.type_(self.program).unwrap().mangle(self.program);

        let is_assignment = matches!(op.class(), parser::BinOpClass::Assignment);
        if is_assignment {
            // op(lhs, rhs)
            let overload = format!("_esl_op{}_{}_{}", op.mangle(), left_type, right_type);
            writeln!(
                self.out,
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
                self.out,
                "    {} = {}({}, {});",
                tmp_var.access(),
                overload,
                left_tmp.access(),
                right_tmp.access()
            )?;
            Ok(Some(tmp_var))
        }
    }

    // Emit ESL expression evaluation as C statements.
    // Returns local var name with result if applicable.
    // Emits *pointer* if expr is a (const) lvalue.
    fn emit_expression_eval(&mut self, expr: &sema::Expression) -> IoResult<Option<TmpVar>> {
        match expr {
            sema::Expression::Call {
                function_id,
                arguments,
            } => {
                let func = self
                    .program
                    .get_function(function_id.expect("invalid function id"));

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
                        .expect("argument list refers to non-existent param id");
                    let var = self.emit_expression_eval(argval)?;
                    arg_tmps.push(var.expect("void expr passed as function argument"));
                }

                // Emit function call
                let return_method = func.return_type.as_ref().unwrap().function_return_method();
                match return_method {
                    FunctionReturnMethod::None => {
                        write!(self.out, "{}(", func.name)?;
                        write!(
                            self.out,
                            "{}",
                            arg_tmps
                                .iter()
                                .map(|a| a.access())
                                .collect::<Vec<_>>()
                                .join(", ")
                        )?;
                        writeln!(self.out, ");")?;
                        return Ok(None);
                    }
                    FunctionReturnMethod::Return => {
                        let tmp_var = self.emit_tmp_var(
                            func.return_type.as_ref().unwrap(),
                            "rv",
                            sema::ValueType::RValue,
                        )?;
                        write!(self.out, "{} = ", tmp_var.access())?;
                        write!(self.out, "{}(", func.name)?;
                        write!(
                            self.out,
                            "{}",
                            arg_tmps
                                .iter()
                                .map(|a| a.access())
                                .collect::<Vec<_>>()
                                .join(", ")
                        )?;
                        writeln!(self.out, ");")?;
                        return Ok(Some(tmp_var));
                    }
                    FunctionReturnMethod::FirstArg => {
                        todo!();
                    }
                }
            }
            sema::Expression::Index { indexable, index } => {
                let index_tmp_var = self.emit_expression_eval(index)?.unwrap();
                let indexable_tmp_var = self.emit_expression_eval(indexable)?.unwrap();
                let out_tmp_var = self.emit_tmp_var(
                    &indexable
                        .type_(self.program)
                        .unwrap()
                        .iter_value_type(self.program)
                        .unwrap(),
                    "index",
                    expr.value_type(),
                )?;
                // Note: out_tmp_var is lvalue so it's always a ptr.
                assert!(out_tmp_var.is_ptr);
                let indexable_type = indexable.type_(self.program).unwrap();

                // bounds check
                writeln!(
                    self.out,
                    "    if({} >= {}) {{",
                    index_tmp_var.access(),
                    match &indexable_type {
                        sema::Type::Array { inner: _, size } => size.to_string(),
                        _ => todo!(),
                    },
                )?;
                writeln!(self.out, "        _esl_panic(\"index out of bounds\");")?;
                writeln!(self.out, "    }}")?;

                // access itself
                writeln!(
                    self.out,
                    "    {} = &({})[{}];",
                    out_tmp_var.access_ptr(),
                    indexable_tmp_var.access(),
                    index_tmp_var.access()
                )?;
                Ok(Some(out_tmp_var))
            }
            sema::Expression::MemberAccess { object, member } => {
                let object_tmp_var = self.emit_expression_eval(object)?.unwrap();
                let member_type = expr.type_(self.program).unwrap();
                let member_tmp_var =
                    self.emit_tmp_var(&member_type, "member", expr.value_type())?;
                writeln!(
                    self.out,
                    "    {} = &({}).{};",
                    member_tmp_var.access_ptr(),
                    object_tmp_var.access(),
                    member
                )?;
                return Ok(Some(member_tmp_var));
            }
            sema::Expression::BoolLiteral { value } => {
                let tmp_var = self.emit_tmp_var(
                    &sema::Type::Primitive(sema::Primitive::Bool),
                    "bool",
                    sema::ValueType::RValue,
                )?;
                writeln!(self.out, "{} = {};", tmp_var.access(), value)?;
                return Ok(Some(tmp_var));
            }
            sema::Expression::IntLiteral { value } => {
                let tmp_var = self.emit_tmp_var(
                    &sema::Type::Primitive(sema::Primitive::U32),
                    "int",
                    sema::ValueType::RValue,
                )?;
                writeln!(self.out, "{} = {};", tmp_var.access(), value)?;
                return Ok(Some(tmp_var));
            }
            sema::Expression::StringLiteral { value } => {
                let tmp_var = self.emit_tmp_var(
                    &sema::Type::Primitive(sema::Primitive::StaticString),
                    "str",
                    sema::ValueType::RValue,
                )?;
                writeln!(self.out, "{} = \"{}\";", tmp_var.access(), escape_c(value))?;
                return Ok(Some(tmp_var));
            }
            sema::Expression::VarRef { var_id, .. } => {
                // in most cases we can just refer directly.
                return Ok(Some(
                    self.variable_names
                        .get(&var_id.expect("invalid var ref"))
                        .cloned()
                        .unwrap(),
                ));
            }
            sema::Expression::BinaryOp { op, left, right } => {
                let out_type = expr.type_(self.program).unwrap();
                Ok(self.emit_binary_op(op, left, right, &out_type)?)
            }
            sema::Expression::ArrayLiteral {
                value_type: _,
                values,
            } => {
                let type_ = expr.type_(self.program).unwrap();
                let tmp_var = self.emit_tmp_var(&type_, "array", sema::ValueType::RValue)?;
                for (i, value) in values.iter().enumerate() {
                    let value_tmp = self
                        .emit_expression_eval(value)?
                        .expect("void expression in array literal");
                    writeln!(
                        self.out,
                        "    ({})[{}] = {};",
                        tmp_var.access(),
                        i,
                        value_tmp.access()
                    )?;
                }
                Ok(Some(tmp_var))
            }
        }
    }

    // Emit ESL expression written into `var`. Useful for initialization
    // (to avoid copy).
    //
    // Panics if expr is void.
    fn emit_expression_into(&mut self, expr: &sema::Expression, var: &str) -> IoResult<()> {
        match &expr {
            sema::Expression::ArrayLiteral { values, .. } => {
                for (i, value) in values.iter().enumerate() {
                    let value_tmp = self.emit_expression_eval(value)?.unwrap();
                    writeln!(self.out, "    {}[{}] = {};", var, i, value_tmp.access())?;
                }
                Ok(())
            }
            _ => {
                let tmp_var = self.emit_expression_eval(expr)?;
                writeln!(self.out, "    {} = {};", var, tmp_var.unwrap().access())
            }
        }
    }

    // Emit iterator construction for an expression, return
    // (iterable, iterator)
    fn emit_iterator_new(&mut self, iterable: &sema::Expression) -> IoResult<(TmpVar, TmpVar)> {
        // Eval iterable
        let iterable_tmp = self
            .emit_expression_eval(iterable)?
            .expect("void for iterable");

        let iterable_type = iterable.type_(self.program).unwrap();

        match &iterable_type {
            sema::Type::Array { inner: _, size: _ } => {
                // We'll just iterate "traditionally" i.e `for(size_t i = 0; i < n; i++)`
                // size_t iter = 0;
                let iterator = self.emit_tmp_var_c("esl_usize", "array_iter")?;
                writeln!(self.out, "    {} = 0;", iterator)?;
                Ok((iterable_tmp, TmpVar::new(iterator, false)))
            }
            _ => {
                let iterable_type_name = iterable_type.mangle(self.program);
                let iterator_type_name = format!("esl_iterator__{}", iterable_type_name);

                // Construct iterator for iterable
                let iterator_new_func = format!("_{}_new", iterator_type_name);
                let iterator_tmp = self.emit_tmp_var_c(&iterator_type_name, "iterator")?;
                writeln!(
                    self.out,
                    "    {} = {}({});",
                    iterator_tmp,
                    iterator_new_func,
                    iterable_tmp.access_ptr()
                )?;
                Ok((iterable_tmp, TmpVar::new(iterator_tmp, false)))
            }
        }
    }

    // Generate has_next() check for iterator (as C expression (!))
    fn generate_iterator_has_next_expr(
        &self,
        iterable_type: &sema::Type,
        iterator: &TmpVar,
    ) -> String {
        match iterable_type {
            sema::Type::Array { inner: _, size } => {
                // iter < size
                let iter = iterator;
                format!("{} < {}", iter.access(), size)
            }
            _ => {
                let iterable_type_name = iterable_type.mangle(self.program);
                let iterator_type_name = format!("esl_iterator__{}", iterable_type_name);
                let iterator_has_next_func = format!("_{}_has_next", iterator_type_name);
                format!("!{}({})", iterator_has_next_func, iterator.access_ptr())
            }
        }
    }

    // Emit next() call (iterator dereference + advance) (as C statement).
    // Returns tmp var name with the next value.
    fn emit_iterator_next(
        &mut self,
        iterable_type: &sema::Type,
        iterable: &TmpVar,
        iterator: &TmpVar,
    ) -> IoResult<TmpVar> {
        match iterable_type {
            sema::Type::Array { inner, size: _ } => {
                let tmp_var = self.emit_tmp_var(inner, "array_elem", sema::ValueType::RValue)?;
                writeln!(
                    self.out,
                    "    {} = {}[{}++];",
                    tmp_var.access(),
                    iterable.access(),
                    iterator.access()
                )?;
                Ok(tmp_var)
            }
            _ => {
                let iterable_type_name = iterable_type.mangle(self.program);
                let iterator_type_name = format!("esl_iterator__{}", iterable_type_name);
                let iterator_next_func = format!("_{}_next", iterator_type_name);
                let value_type = iterable_type.iter_value_type(self.program).unwrap();
                let tmp_var =
                    self.emit_tmp_var(&value_type, "array_elem", sema::ValueType::RValue)?;
                writeln!(
                    self.out,
                    "    {} = {}({});",
                    tmp_var.access(),
                    iterator_next_func,
                    iterator.access_ptr()
                )?;
                Ok(tmp_var)
            }
        }
    }

    // Emit ESL statement as C statements.
    fn emit_statement(&mut self, block: &sema::Statement) -> IoResult<()> {
        match block {
            sema::Statement::Expression(expression) => _ = self.emit_expression_eval(expression)?,
            sema::Statement::Block(statements) => {
                writeln!(self.out, "{{")?;
                for statement in statements {
                    self.emit_statement(statement)?;
                }
                writeln!(self.out, "}}")?;
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
                self.variable_names
                    .insert(*var_id, TmpVar::new(var_name.clone(), false));
                writeln!(
                    self.out,
                    "    // let {}: {}",
                    var.name,
                    var.type_.as_ref().unwrap().name(self.program)
                )?;
                write!(self.out, "    ")?;
                self.emit_var_decl(
                    var.type_.as_ref().unwrap(),
                    &var_name,
                    sema::ValueType::RValue,
                )?;
                writeln!(self.out, ";")?;
                // init
                if let Some(init) = init {
                    self.emit_expression_into(init, &var_name)?;
                }
            }
            sema::Statement::Return(expression) => {
                // TODO: handle return by first arg
                if let Some(expr) = expression {
                    let tmp_var = self.emit_expression_eval(expr)?;
                    write!(self.out, "return {};", tmp_var.unwrap().access())?;
                } else {
                    writeln!(self.out, "return;")?;
                }
            }
            sema::Statement::For {
                it_var,
                iterable,
                body,
            } => {
                let iterable_type = iterable.type_(self.program).unwrap();

                // construct iterator
                let (iterable_tmp, iterator_tmp) = self.emit_iterator_new(iterable)?;

                // while(iterator.has_next()) { body }
                writeln!(
                    self.out,
                    "    while({}) {{",
                    self.generate_iterator_has_next_expr(&iterable_type, &iterator_tmp),
                )?;

                // get next value
                let it_var_name =
                    self.emit_iterator_next(&iterable_type, &iterable_tmp, &iterator_tmp)?;
                self.variable_names.insert(*it_var, it_var_name);

                // emit body
                self.emit_statement(body)?;

                writeln!(self.out, "    }}")?;
            }
            sema::Statement::If {
                condition,
                then_block,
                else_block,
            } => {
                let condition_tmp_var = self
                    .emit_expression_eval(condition)?
                    .expect("void expression in if condition");
                writeln!(self.out, "    if ({}) {{", condition_tmp_var.access())?;
                self.emit_statement(&then_block)?;
                writeln!(self.out, "    }}")?;
                if let Some(else_block) = else_block {
                    writeln!(self.out, "    else {{")?;
                    self.emit_statement(&else_block)?;
                    writeln!(self.out, "    }}")?;
                }
            }
            sema::Statement::Break => {
                writeln!(self.out, "break;")?;
            }
            sema::Statement::Continue => {
                writeln!(self.out, "continue;")?;
            }
            sema::Statement::While { condition, body } => {
                writeln!(self.out, "    while(true) {{")?;
                let condition_tmp_var = self.emit_expression_eval(condition)?;
                writeln!(
                    self.out,
                    "    if (!{}) break;",
                    condition_tmp_var.unwrap().access()
                )?;
                self.emit_statement(body)?;
                writeln!(self.out, "    }}")?;
            }
        }
        Ok(())
    }

    fn emit_function_decl(&mut self, function: &sema::Function) -> IoResult<()> {
        writeln!(
            self.out,
            "/* function {} (ID={}, params scope ID={})*/",
            function.name,
            function.id().0.mangle(),
            function.params_scope.0.mangle()
        )?;
        self.emit_type(function.return_type.as_ref().unwrap())?;

        write!(self.out, " {}(", self.mangled_function_name(&function))?;
        let scope = self.program.get_scope(function.params_scope);
        for (i, param) in scope.vars.iter().enumerate() {
            if i > 0 {
                write!(self.out, ", ")?;
            }

            // TODO: handle different ways to pass arguments
            let var = self.program.get_var(*param);
            self.emit_type(&var.type_.as_ref().unwrap())?;

            write!(self.out, " {}", var.name)?;
        }
        writeln!(self.out, ");\n")?;
        Ok(())
    }

    fn emit_function_impl(&mut self, function: &sema::Function) -> IoResult<()> {
        assert!(function.body.is_some());
        self.emit_type(&function.return_type.as_ref().expect("invalid type"))?;

        write!(self.out, " {}(", self.mangled_function_name(&function))?;
        let scope = self.program.get_scope(function.params_scope);
        for (i, param) in scope.vars.iter().enumerate() {
            if i > 0 {
                write!(self.out, ", ")?;
            }

            // TODO: handle different ways to pass arguments
            let var = self.program.get_var(*param);
            self.emit_type(&var.type_.as_ref().unwrap())?;

            let var_name = format!("{}{}_param", var.name, self.local_var_counter);
            write!(self.out, " {}", var_name)?;
            self.local_var_counter += 1;
            self.variable_names
                .insert(*param, TmpVar::new(var_name, false));
        }
        writeln!(self.out, ") {{")?;
        self.emit_statement(&function.body.as_ref().unwrap())?;
        // return, for main function
        if function.name == "main" {
            writeln!(self.out, "    return 0;")?;
        }
        writeln!(self.out, "}}")?;
        Ok(())
    }

    fn emit_header(&mut self) -> IoResult<()> {
        writeln!(self.out, "#include <esl_header.h>\n")?;
        Ok(())
    }

    fn emit_footer(&mut self) -> IoResult<()> {
        writeln!(self.out, "int main() {{")?;
        writeln!(self.out, "    return $$esl_main();")?;
        writeln!(self.out, "}}")?;
        Ok(())
    }

    pub fn emit_program(&mut self) -> IoResult<()> {
        self.emit_header()?;
        for struct_ in self.program.structs() {
            let struct_name = format!("struct{}", struct_.id.unwrap().0.mangle());
            writeln!(self.out, "typedef struct _{} {{", struct_name)?;
            for field in struct_.fields.iter() {
                self.emit_type(&field.type_.as_ref().unwrap())?;
                writeln!(self.out, " {};", field.name)?;
            }
            writeln!(self.out, "}} {};", struct_name)?;
        }
        for func in self.program.functions() {
            self.emit_function_decl(func)?;
        }
        for func in self.program.functions() {
            if func.body.is_some() {
                self.emit_function_impl(func)?;
            }
        }
        self.emit_footer()?;
        Ok(())
    }

    pub fn new(out: &'data mut dyn Write, program: &'data sema::Program) -> Self {
        Self {
            out,
            program,
            variable_names: HashMap::new(),
            local_var_counter: 0,
            tmp_var_counter: 0,
        }
    }
}
