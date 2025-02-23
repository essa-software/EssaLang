use std::{
    fs::File,
    io::Read,
    path::{Path, PathBuf},
};

use crate::{error::CompilationError, parser};

pub struct ParseModuleResult {
    pub path: PathBuf,
    pub module: parser::Module,
    pub errors: Vec<CompilationError>,
}

pub fn read_file(path: &Path) -> anyhow::Result<Vec<u8>> {
    let mut source_file = File::open(path)?;
    let mut source = Vec::new();
    source_file.read_to_end(&mut source)?;
    Ok(source)
}

pub fn parse_module_from_file(path: &Path) -> anyhow::Result<ParseModuleResult> {
    eprintln!("Loading module from {}", path.to_string_lossy());

    let source = read_file(path)?;

    let parser = parser::Parser::new(path.into(), &source)?;
    let (p_module, errors) = parser.parse();

    Ok(ParseModuleResult {
        path: path.to_path_buf(),
        module: p_module,
        errors,
    })
}
