pub mod codegen;
pub mod compiler;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod sema;

use std::{fs, path::Path};

use argparse::{ArgumentParser, Store, StoreTrue};
use codegen::CodeGen;

pub struct CompileArgs {
    machine_readable_errors: bool,
}

// This returns Err only on fatal errors like failing to open a file.
// For "soft" errors (compilation) this returns false.
fn compile_file(path: &Path, args: &CompileArgs) -> anyhow::Result<bool> {
    eprintln!("Running file: {:?}", path);

    let mut main_module = compiler::parse_module_from_file(path)?;

    let typechecker = sema::TypeChecker::new();
    let (program, typechecker_errors) = typechecker.typecheck(main_module.module);

    main_module.errors.extend(typechecker_errors);

    if !main_module.errors.is_empty() {
        if args.machine_readable_errors {
            eprintln!("Errors:");
            for error in main_module.errors {
                // Note: This is the only thing printed on stdout in the
                // whole program. (The correct program prints nothing)
                println!("{:?}", error);
            }
        } else {
            for error in main_module.errors {
                // FIXME: don't read file every time
                let source = compiler::read_file(&error.path)?;
                error.print(&source);
            }
        }
        return Ok(false);
    }

    // codegen
    const BUILD_DIR: &str = "build";
    match fs::create_dir("build") {
        Ok(_) => {}
        Err(ref e) if e.kind() == std::io::ErrorKind::AlreadyExists => {}
        Err(e) => return Err(e.into()),
    }

    let mut tmp_file = tempfile::NamedTempFile::with_suffix_in(".c", BUILD_DIR)?;
    let mut codegen = CodeGen::new(&mut tmp_file, &program);
    codegen.emit_program().expect("codegen failed");

    // copy tmp file for debug
    fs::copy(tmp_file.path(), Path::new(BUILD_DIR).join("out.c"))?;

    // gcc pass
    let runtime_prefix = dirs::home_dir()
        .expect("Failed to get home dir")
        .join(".local");

    let runtime_libs_path = runtime_prefix.join("lib");
    let runtime_include_path = runtime_prefix.join("include");

    eprintln!("calling gcc");
    let mut cmd = std::process::Command::new("gcc");
    cmd
        // c version
        .arg("-std=c23")
        // warnings
        .arg("-Wall")
        .arg("-Wextra")
        // .arg("-Werror")
        // debug info
        .arg("-g")
        // output
        .arg("-o")
        .arg(Path::new(BUILD_DIR).join("out"))
        // input
        .arg(tmp_file.path())
        // include dirs
        .arg("-I")
        .arg(runtime_include_path)
        // lib dirs
        .arg("-L")
        .arg(runtime_libs_path)
        // libs
        .arg("-leslrt");

    eprintln!("Calling: {:?}", cmd);
    Ok(cmd.spawn()?.wait()?.code().map_or(false, |c| c == 0))
}

fn main() {
    let mut path = String::new();
    let mut args = CompileArgs {
        machine_readable_errors: false,
    };

    {
        let mut ap = ArgumentParser::new();
        ap.refer(&mut path)
            .add_argument("source", Store, "Source file")
            .required();
        ap.refer(&mut args.machine_readable_errors).add_option(
            &["--machine-readable-errors"],
            StoreTrue,
            "Print errors in a machine-readable format",
        );
        ap.parse_args_or_exit();
    }

    match compile_file(Path::new(&path), &args) {
        Ok(true) => {}
        Ok(false) => {
            eprintln!("Compilation failed");
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("Fatal error: {:?}", e);
            std::process::exit(2);
        }
    }
}
