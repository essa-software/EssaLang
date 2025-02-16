use std::ops::Range;

#[derive(Debug)]
pub struct CompilationError {
    pub message: String,
    pub range: Range<usize>,
}

impl CompilationError {
    pub fn new(message: String, range: Range<usize>) -> Self {
        Self { message, range }
    }
}
