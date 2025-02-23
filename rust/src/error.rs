use std::{
    io::{BufRead, Cursor, Seek},
    ops::{Range, RangeInclusive},
};

#[derive(Debug)]
pub struct CompilationError {
    pub message: String,
    pub range: Range<usize>,
}

// These are 0-based.
struct LineColumn {
    line: usize,
    column: usize,
}

fn offset_to_line_column(input: &[u8], offset: usize) -> LineColumn {
    let mut line = 0;
    let mut column = 0;
    for i in 0..offset {
        if i >= input.len() {
            break;
        }
        if input[i] == b'\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }
    LineColumn { line, column }
}

struct Line {
    content: String,
    start: usize,
    end: usize,
}

fn get_lines(input: &[u8], lines: RangeInclusive<usize>) -> Vec<Line> {
    let mut result = Vec::new();
    let mut reader = Cursor::new(input);
    for _ in 0..*lines.start() {
        let mut line = String::new();
        reader.read_line(&mut line).unwrap();
    }
    for _ in lines {
        let mut line = String::new();
        reader.read_line(&mut line).unwrap();
        let end = reader.seek(std::io::SeekFrom::Current(0)).unwrap() as usize;
        let start = end - line.len();
        result.push(Line {
            content: line,
            start,
            end,
        });
    }
    result
}

fn intersection(range1: Range<usize>, range2: Range<usize>) -> Option<Range<usize>> {
    let start = range1.start.max(range2.start);
    let end = range1.end.min(range2.end);
    if start < end {
        Some(start..end)
    } else {
        None
    }
}

impl CompilationError {
    pub fn new(message: String, range: Range<usize>) -> Self {
        Self { message, range }
    }

    pub fn print(&self, input_filename: &str, input: &[u8]) {
        let LineColumn {
            line: start_line,
            column: start_column,
        } = offset_to_line_column(input, self.range.start);

        let _print_colors = true;

        const FMT_RED: &str = "\x1b[31m";
        const FMT_BLUE: &str = "\x1b[36m";
        const FMT_BOLD: &str = "\x1b[1m";
        const FMT_RESET: &str = "\x1b[0m";

        let LineColumn {
            line: end_line,
            column: _end_column,
        } = offset_to_line_column(input, self.range.end);

        let lines_range = start_line..=end_line;
        let lines = get_lines(input, lines_range.clone());

        eprintln!(
            "{FMT_BOLD}{FMT_RED}error{FMT_RESET}: {FMT_BOLD}{}{FMT_RESET}\n  {FMT_BLUE}{}:{}:{}{FMT_RESET}",
            self.message,
            input_filename,
            start_line + 1,
            start_column + 1,
        );
        for (i, line) in lines.iter().zip(lines_range) {
            let line = line + 1;
            let line_num_str_len = line.to_string().len() + 3;
            eprint!("{} | {}", line, i.content);

            // underline range = intersection of error range & line range
            let underline_range = intersection(i.start..i.end, self.range.clone());

            if let Some(underline_range) = underline_range {
                let underline_start = underline_range.start - i.start;
                let underline_end = underline_range.end - i.start;
                let underline_len = underline_end - underline_start;
                let indent = line_num_str_len + underline_start;
                let indent = std::iter::repeat(' ').take(indent).collect::<String>();
                let underline = std::iter::repeat('^')
                    .take(underline_len)
                    .collect::<String>();
                eprintln!("{}{FMT_RED}{FMT_BOLD}{}{FMT_RESET}", indent, underline);
            } else {
                eprintln!();
            }
        }
        eprintln!();
    }
}
