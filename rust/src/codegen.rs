use std::{collections::HashMap, io::Write};

use crate::{parser, sema};

pub struct CodeGen<'data> {
    out: &'data mut dyn Write,
    program: &'data sema::Program,
    tmp_var_counter: usize,
    local_var_counter: usize,
    variable_names: HashMap<sema::VarId, String>,
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

    // Returns a name.
    fn emit_tmp_var(&mut self, ty: &sema::Type, debug: &str) -> IoResult<String> {
        self.tmp_var_counter += 1;

        let name = format!("$$tmp{}_{}", self.tmp_var_counter, debug.to_string());

        writeln!(self.out, "    ")?;
        self.emit_type(ty)?;
        writeln!(self.out, " {};", name)?;

        Ok(name)
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
                _ => todo!(),
            }
        )?;
        writeln!(self.out, " {arg_var_name}.data = &{};", tmp_var)?;
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
            "    _esl_print({fmtstr_var_name}, {}, {args_var_name});",
            args_in_varid_order.len(),
        )?;

        Ok(())
    }

    fn emit_binary_op(
        &mut self,
        op: &parser::BinaryOp,
        left: &sema::Expression,
        right: &sema::Expression,
    ) -> IoResult<Option<String>> {
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
            writeln!(self.out, "    {}(&{}, {});", overload, left_tmp, right_tmp)?;
            Ok(None)
        } else {
            // out = op(lhs, rhs)
            let tmp_var = self.emit_tmp_var(&left.type_(self.program).unwrap(), "binop")?;

            // select C overload
            let overload = format!("_esl_op{}_{}_{}", op.mangle(), left_type, right_type);

            writeln!(
                self.out,
                "    {} = {}({}, {});",
                tmp_var, overload, left_tmp, right_tmp
            )?;
            Ok(Some(tmp_var))
        }
    }

    // Emit ESL expression evaluation as C statements.
    // Returns local var name with result if applicable
    fn emit_expression_eval(&mut self, expr: &sema::Expression) -> IoResult<Option<String>> {
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
                let return_method = func.return_type.function_return_method();
                match return_method {
                    FunctionReturnMethod::None => {
                        write!(self.out, "{}(", func.name)?;
                        write!(self.out, "{}", arg_tmps.join(", "))?;
                        writeln!(self.out, ");")?;
                        return Ok(None);
                    }
                    FunctionReturnMethod::Return => {
                        let tmp_var = self.emit_tmp_var(&func.return_type, "rv")?;
                        write!(self.out, "{} = ", tmp_var)?;
                        write!(self.out, "{}(", func.name)?;
                        write!(self.out, "{}", arg_tmps.join(", "))?;
                        writeln!(self.out, ");")?;
                        return Ok(Some(tmp_var));
                    }
                    FunctionReturnMethod::FirstArg => {
                        let tmp_var = self.emit_tmp_var(&func.return_type, "bigrv")?;
                        write!(self.out, "{}(", func.name)?;
                        if arg_tmps.is_empty() {
                            write!(self.out, "/*rv*/&{}", tmp_var)?;
                        } else {
                            write!(self.out, "/*rv*/&{}, ", tmp_var)?;
                            write!(self.out, "{}", arg_tmps.join(", "))?;
                        }
                        writeln!(self.out, ");")?;
                        return Ok(Some(tmp_var));
                    }
                }
            }
            sema::Expression::BoolLiteral { value } => {
                let tmp_var =
                    self.emit_tmp_var(&sema::Type::Primitive(sema::Primitive::Bool), "bool")?;
                writeln!(self.out, "{} = {};", tmp_var, value)?;
                return Ok(Some(tmp_var));
            }
            sema::Expression::IntLiteral { value } => {
                let tmp_var =
                    self.emit_tmp_var(&sema::Type::Primitive(sema::Primitive::U32), "int")?;
                writeln!(self.out, "{} = {};", tmp_var, value)?;
                return Ok(Some(tmp_var));
            }
            sema::Expression::StringLiteral { value } => {
                let tmp_var = self
                    .emit_tmp_var(&sema::Type::Primitive(sema::Primitive::StaticString), "str")?;
                writeln!(self.out, "{} = \"{}\";", tmp_var, escape_c(value))?;
                return Ok(Some(tmp_var));
            }
            sema::Expression::VarRef { var_id, .. } => {
                // in most cases we can just refer directly.
                return Ok(Some(
                    self.variable_names
                        .get(&var_id.expect("invalid var ref"))
                        .unwrap()
                        .clone(),
                ));
            }
            sema::Expression::BinaryOp { op, left, right } => {
                Ok(self.emit_binary_op(op, left, right)?)
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
                let var_name = format!("{}{}", var.name, self.local_var_counter);
                self.local_var_counter += 1;
                self.variable_names.insert(*var_id, var_name.clone());
                writeln!(self.out, "    /* var decl {} */", var.name)?;
                write!(self.out, "    ")?;
                self.emit_type(&var.type_.as_ref().unwrap())?;
                writeln!(self.out, " {};", var_name)?;
                // init
                if let Some(init) = init {
                    let tmp_var = self.emit_expression_eval(init)?;
                    writeln!(self.out, "    {} = {};", var_name, tmp_var.unwrap())?;
                }
            }
            sema::Statement::Return(expression) => {
                // TODO: handle return by first arg
                if let Some(expr) = expression {
                    let tmp_var = self.emit_expression_eval(expr)?;
                    write!(
                        self.out,
                        "return {};",
                        tmp_var.expect("void expression in return statement")
                    )?;
                } else {
                    writeln!(self.out, "return;")?;
                }
            }
            sema::Statement::If {
                condition,
                then_block,
            } => {
                let condition_tmp_var = self
                    .emit_expression_eval(condition)?
                    .expect("void expression in if condition");
                writeln!(self.out, "    if ({condition_tmp_var}) {{")?;
                self.emit_statement(&then_block)?;
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
        self.emit_type(&function.return_type)?;

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
        self.emit_type(&function.return_type)?;

        write!(self.out, " {}(", self.mangled_function_name(&function))?;
        // TODO: Params
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
