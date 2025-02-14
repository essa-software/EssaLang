use std::io::Write;

use crate::sema;

pub struct CodeGen<'data> {
    out: &'data mut dyn Write,
    program: &'data sema::Program,
    tmp_var_counter: usize,
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

                // Emit argument evaluation
                let mut arg_tmps = Vec::new();
                for arg in &func.params {
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
            sema::Expression::StringLiteral { value } => {
                let tmp_var = self
                    .emit_tmp_var(&sema::Type::Primitive(sema::Primitive::StaticString), "lit")?;
                writeln!(self.out, "{} = \"{}\";", tmp_var, escape_c(value))?;
                return Ok(Some(tmp_var));
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
        }
        Ok(())
    }

    fn emit_function_decl(&mut self, function: &sema::Function) -> IoResult<()> {
        self.emit_type(&function.return_type)?;

        write!(self.out, " {}(", self.mangled_function_name(&function))?;
        for (i, param) in function.params.iter().enumerate() {
            if i > 0 {
                write!(self.out, ", ")?;
            }

            // TODO: handle different ways to pass arguments
            let var = function.get_local_var(*param);
            self.emit_type(&var.type_)?;

            write!(self.out, " {}", var.name)?;
        }
        writeln!(self.out, ");")?;
        Ok(())
    }

    fn emit_function_impl(&mut self, function: &sema::Function) -> IoResult<()> {
        assert!(function.body.is_some());
        self.emit_type(&function.return_type)?;

        write!(self.out, " {}(", self.mangled_function_name(&function))?;
        // TODO: Params
        writeln!(self.out, ") {{")?;
        self.emit_statement(&function.body.as_ref().unwrap())?;
        writeln!(self.out, "}}")?;
        Ok(())
    }

    fn emit_header(&mut self) -> IoResult<()> {
        writeln!(self.out, "#include <esl_header.h>")?;
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
            tmp_var_counter: 0,
        }
    }
}
