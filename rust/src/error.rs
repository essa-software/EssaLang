use std::{
    io::{BufRead, Cursor, Seek},
    ops::Range,
};

#[derive(Debug)]
pub struct CompilationError {
    pub message: String,
    pub range: Range<usize>,
}

impl CompilationError {
    pub fn new(message: String, range: Range<usize>) -> Self {
        Self { message, range }
    }

    pub fn print(&self, input_filename: &str, input: &[u8]) {
        let mut line_no = 1;
        let mut reader = Cursor::new(input);
        loop {
            let mut line = String::new();
            let line_start = reader.seek(std::io::SeekFrom::Current(0)).unwrap();
            if reader.read_line(&mut line).unwrap() == 0 {
                break;
            }
            let line_end = reader.seek(std::io::SeekFrom::Current(0)).unwrap();
            if self.range.start >= line_start as usize && self.range.end <= line_end as usize {
                let column = self.range.start - line_start as usize;
                let line_no_str_len = line_no.to_string().len();
                eprintln!(
                    "{}:{}:{}: error: {}",
                    input_filename,
                    line_no,
                    column + 1,
                    self.message
                );
                eprintln!("{} |{}", line_no, line.trim_end());
                eprint!("{}", " ".repeat(column));
                eprint!(
                    "{}{}",
                    " ".repeat(line_no_str_len + 2),
                    "^".repeat(self.range.end - self.range.start)
                );
                eprintln!("\n");
            }
            line_no += 1;
        }
    }
}
