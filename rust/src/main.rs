pub mod codegen;
pub mod parser;
pub mod sema;

use std::{env::args, io::stdout, path::Path};

use codegen::CodeGen;
use sema::TypeChecker;

fn run_file(path: &Path) {
    eprintln!("Running file: {:?}", path);

    let program = parser::Program {
        declarations: vec![parser::Declaration::FunctionImpl {
            name: "main".into(),
            return_type: parser::Type::Simple("u32".into()),
            body: parser::Statement::Block(vec![parser::Statement::Expression(
                parser::Expression::Call {
                    function: "print".into(),
                    args: vec![parser::FunctionArg {
                        param: None,
                        value: parser::Expression::StringLiteral {
                            value: "Hello, world!\\n".into(),
                        },
                    }],
                },
            )]),
        }],
    };

    let typechecker = TypeChecker::new(&program);
    let program = typechecker.typecheck();

    let mut stdout = stdout();
    let mut codegen = CodeGen::new(&mut stdout, &program);
    codegen.emit_program().expect("codegen failed");
}

fn main() {
    let input = args().nth(1).expect("Expected input file");
    let path = Path::new(&input);
    run_file(path);
}
