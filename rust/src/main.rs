pub mod codegen;
pub mod compiler;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod sema;
pub mod types;

use std::{
    cell::RefCell,
    fs,
    path::{Path, PathBuf},
    rc::Rc,
};

use argparse::{ArgumentParser, Collect, Store, StoreTrue};
use codegen::CodeGen;
use log::{info, trace};

pub struct CompileArgs {
    machine_readable_errors: bool,
    c_includes: Vec<String>,
    c_include_dirs: Vec<PathBuf>,
    c_sources: Vec<PathBuf>,
    additional_cc_args: Vec<String>,
}

fn realpath(p: &PathBuf) -> PathBuf {
    match fs::canonicalize(&p) {
        Ok(p) => p,
        Err(_) => p.clone(),
    }
}

// This returns Err only on fatal errors like failing to open a file.
// For "soft" errors (compilation) this returns false.
fn compile_file(path: &Path, args: CompileArgs) -> anyhow::Result<bool> {
    info!("Running file: {:?}", path);

    let mut main_module = compiler::parse_module_from_file(path)?;

    let typechecker = sema::TypeChecker::new();
    let (program, typechecker_errors) = typechecker.typecheck(main_module.module);

    main_module.errors.extend(typechecker_errors);

    if !main_module.errors.is_empty() {
        if args.machine_readable_errors {
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

    trace!("Program: {:#?}", program);

    // codegen
    const BUILD_DIR: &str = "build";
    match fs::create_dir("build") {
        Ok(_) => {}
        Err(ref e) if e.kind() == std::io::ErrorKind::AlreadyExists => {}
        Err(e) => return Err(e.into()),
    }

    let tmp_file = tempfile::NamedTempFile::with_suffix_in(".c", BUILD_DIR)?;
    let tmp_file_path = tmp_file.path().to_path_buf();
    let mut codegen = CodeGen::new(Rc::new(RefCell::new(tmp_file)), &program);
    codegen
        .emit_program(args.c_includes)
        .expect("codegen failed");

    // copy tmp file for debug
    fs::copy(&tmp_file_path, Path::new(BUILD_DIR).join("out.c"))?;

    // gcc pass
    let runtime_prefix = dirs::home_dir()
        .expect("Failed to get home dir")
        .join(".local");

    let runtime_libs_path = runtime_prefix.join("lib");
    let runtime_include_path = runtime_prefix.join("include");

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
        .arg(tmp_file_path)
        // user inputs
        .args(
            args.c_sources
                .iter()
                .map(|p| realpath(p).to_string_lossy().to_string()),
        )
        // include dirs
        .arg("-I")
        .arg(runtime_include_path)
        // user include dirs
        .args(
            args.c_include_dirs
                .iter()
                .flat_map(|d| vec!["-I".into(), realpath(d).to_string_lossy().to_string()]),
        )
        // lib dirs
        .arg("-L")
        .arg(runtime_libs_path)
        // libs
        .arg("-leslrt")
        // user args
        .args(args.additional_cc_args);

    info!("Calling: {:?}", cmd);
    Ok(cmd.spawn()?.wait()?.code().map_or(false, |c| c == 0))
}

fn main() {
    env_logger::init();

    let mut path = String::new();
    let mut args = CompileArgs {
        machine_readable_errors: false,
        c_includes: vec![],
        c_include_dirs: vec![],
        c_sources: vec![],
        additional_cc_args: vec![],
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
        ap.refer(&mut args.c_includes).add_option(
            &["--c-include"],
            Collect,
            "Include additional C header in codegen, relative to source file.",
        );
        ap.refer(&mut args.c_include_dirs).add_option(
            &["--c-include-dir"],
            Collect,
            "Add include path for C headers (see --c-include)",
        );
        ap.refer(&mut args.c_sources).add_option(
            &["--c-source"],
            Collect,
            "Add additional C source file to compile and link",
        );
        ap.refer(&mut args.additional_cc_args).add_option(
            &["--cc-arg"],
            Collect,
            "Add additional argument to C compiler's command line",
        );
        ap.parse_args_or_exit();
    }

    match compile_file(Path::new(&path), args) {
        Ok(true) => {}
        Ok(false) => {
            info!("Compilation failed");
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("Fatal error: {:?}", e);
            std::process::exit(2);
        }
    }
}
