use std::{
    fmt::Display,
    ops::Range,
    str::{self, Chars, Utf8Error},
};

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    BraceClose,
    BraceOpen,
    Colon,
    ColonColon,
    Comma,
    CurlyClose,
    CurlyOpen,
    Integer(u64),
    KeywordBreak,
    KeywordContinue,
    KeywordElse,
    KeywordExtern,
    KeywordFalse,
    KeywordFor,
    KeywordFunc,
    KeywordIf,
    KeywordImport,
    KeywordLet,
    KeywordMut,
    KeywordOf,
    KeywordReturn,
    KeywordStruct,
    KeywordThis,
    KeywordTrue,
    KeywordWhile,
    Name(String),
    OpAmpAmp,         // &&
    OpAsterisk,       // *
    OpAsteriskEquals, // *=
    OpDot,            // .
    OpDotDot,         // ..
    OpEquals,         // =
    OpEqualsEquals,   // ==
    OpExlmEquals,     // !=
    OpGreater,        // >
    OpGreaterEquals,  // >=
    OpLess,           // <
    OpLessEquals,     // <=
    OpMinus,          // -
    OpMinusEquals,    // -=
    OpPercent,        // %
    OpPercentEquals,  // %=
    OpPipePipe,       // ||
    OpPlus,           // +
    OpPlusEquals,     // +=
    OpSlash,          // /=
    OpSlashEquals,    // /=
    ParenClose,
    ParenOpen,
    Semicolon,
    StringLiteral(String),
    CharLiteral(String),
    Garbage,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::BraceClose => f.write_str("']'"),
            TokenType::BraceOpen => f.write_str("'['"),
            TokenType::Colon => f.write_str("':'"),
            TokenType::ColonColon => f.write_str("'::'"),
            TokenType::Comma => f.write_str("','"),
            TokenType::CurlyClose => f.write_str("'}'"),
            TokenType::CurlyOpen => f.write_str("'{'"),
            TokenType::Garbage => f.write_str("<garbage>"),
            TokenType::Integer(i) => write!(f, "number '{}'", i),
            TokenType::KeywordBreak => f.write_str("'break'"),
            TokenType::KeywordContinue => f.write_str("'continue'"),
            TokenType::KeywordElse => f.write_str("'else'"),
            TokenType::KeywordExtern => f.write_str("'extern'"),
            TokenType::KeywordFalse => f.write_str("'false'"),
            TokenType::KeywordFor => f.write_str("'for'"),
            TokenType::KeywordFunc => f.write_str("'func'"),
            TokenType::KeywordIf => f.write_str("'if'"),
            TokenType::KeywordImport => f.write_str("'import'"),
            TokenType::KeywordLet => f.write_str("'let'"),
            TokenType::KeywordMut => f.write_str("'mut'"),
            TokenType::KeywordOf => f.write_str("'of'"),
            TokenType::KeywordReturn => f.write_str("'return'"),
            TokenType::KeywordStruct => f.write_str("'struct'"),
            TokenType::KeywordThis => f.write_str("'this'"),
            TokenType::KeywordTrue => f.write_str("'true'"),
            TokenType::KeywordWhile => f.write_str("'while'"),
            TokenType::Name(n) => write!(f, "name '{}'", n),
            TokenType::OpAmpAmp => f.write_str("'&&'"),
            TokenType::OpAsterisk => f.write_str("'*'"),
            TokenType::OpAsteriskEquals => f.write_str("'*='"),
            TokenType::OpDot => f.write_str("'.'"),
            TokenType::OpDotDot => f.write_str("'..'"),
            TokenType::OpEquals => f.write_str("'='"),
            TokenType::OpEqualsEquals => f.write_str("'=='"),
            TokenType::OpExlmEquals => f.write_str("'!='"),
            TokenType::OpGreater => f.write_str("'>'"),
            TokenType::OpGreaterEquals => f.write_str("'>='"),
            TokenType::OpLess => f.write_str("'<'"),
            TokenType::OpLessEquals => f.write_str("'<='"),
            TokenType::OpMinus => f.write_str("'-'"),
            TokenType::OpMinusEquals => f.write_str("'-='"),
            TokenType::OpPercent => f.write_str("'%'"),
            TokenType::OpPercentEquals => f.write_str("'%='"),
            TokenType::OpPipePipe => f.write_str("'||'"),
            TokenType::OpPlus => f.write_str("'+'"),
            TokenType::OpPlusEquals => f.write_str("'+='"),
            TokenType::OpSlash => f.write_str("'/'"),
            TokenType::OpSlashEquals => f.write_str("'/='"),
            TokenType::ParenClose => f.write_str("')'"),
            TokenType::ParenOpen => f.write_str("'('"),
            TokenType::Semicolon => f.write_str("';'"),
            TokenType::StringLiteral(l) => write!(f, "\"{}\"", l),
            TokenType::CharLiteral(l) => write!(f, "'{}'", l),
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub type_: TokenType,
    pub range: Range<usize>,
}

impl Token {
    pub fn slice<'a>(&self, input: &'a [u8]) -> &'a [u8] {
        &input[self.range.clone()]
    }

    pub fn slice_str<'a>(&self, input: &'a [u8]) -> Result<&'a str, Utf8Error> {
        str::from_utf8(self.slice(input))
    }
}

#[derive(Clone)]
pub struct TokenIterator<'a> {
    input: &'a [u8],
    iter: Chars<'a>,
    offset_bytes: usize,
}

impl<'a> TokenIterator<'a> {
    pub fn new(input: &'a [u8]) -> Result<Self, Utf8Error> {
        Ok(Self {
            input,
            iter: str::from_utf8(input)?.chars(),
            offset_bytes: 0,
        })
    }

    fn slice(&self, start: usize, end: usize) -> &'a [u8] {
        &self.input[start..end]
    }

    fn slice_str(&self, start: usize, end: usize) -> Result<&'a str, Utf8Error> {
        str::from_utf8(self.slice(start, end))
    }

    fn read_one(&mut self) -> Option<char> {
        let c = self.iter.next()?;
        self.offset_bytes += c.len_utf8();
        Some(c)
    }

    fn peek(&self) -> Option<char> {
        self.iter.clone().next()
    }

    fn read_while(&mut self, pred: impl Fn(char) -> bool) -> &'a str {
        let start = self.offset_bytes;
        while let Some(c) = self.peek() {
            if !pred(c) {
                break;
            }
            self.read_one();
        }
        self.slice_str(start, self.offset_bytes).unwrap()
    }

    fn read_whitespace(&mut self) {
        self.read_while(|c| c.is_whitespace());
    }

    fn token(&self, type_: TokenType, start: usize) -> Token {
        Token {
            type_,
            range: start..self.offset_bytes,
        }
    }

    fn name(&self, str: String, start: usize) -> Token {
        Token {
            type_: TokenType::Name(str),
            range: start..self.offset_bytes,
        }
    }

    // None on EOF
    fn read_token(&mut self) -> Option<Token> {
        self.read_whitespace();

        let start = self.offset_bytes;
        let c = self.peek()?;
        match c {
            // comments
            '/' => {
                self.read_one();
                match self.peek() {
                    Some('/') => {
                        self.read_while(|c| c != '\n');
                        return self.read_token();
                    }
                    Some('=') => {
                        self.read_one();
                        return Some(self.token(TokenType::OpSlashEquals, start));
                    }
                    _ => {
                        return Some(self.token(TokenType::OpSlash, start));
                    }
                }
            }
            // punctuation / operators
            '*' => {
                self.read_one();
                if self.peek() == Some('=') {
                    self.read_one();
                    return Some(self.token(TokenType::OpAsteriskEquals, start));
                }
                Some(self.token(TokenType::OpAsterisk, start))
            }
            '[' => {
                self.read_one();
                Some(self.token(TokenType::BraceOpen, start))
            }
            ']' => {
                self.read_one();
                Some(self.token(TokenType::BraceClose, start))
            }
            ':' => {
                self.read_one();
                if self.peek() == Some(':') {
                    self.read_one();
                    return Some(self.token(TokenType::ColonColon, start));
                }
                Some(self.token(TokenType::Colon, start))
            }
            ',' => {
                self.read_one();
                Some(self.token(TokenType::Comma, start))
            }
            '{' => {
                self.read_one();
                Some(self.token(TokenType::CurlyOpen, start))
            }
            '}' => {
                self.read_one();
                Some(self.token(TokenType::CurlyClose, start))
            }
            '.' => {
                self.read_one();
                if self.peek() == Some('.') {
                    self.read_one();
                    return Some(self.token(TokenType::OpDotDot, start));
                }
                Some(self.token(TokenType::OpDot, start))
            }
            '=' => {
                self.read_one();
                if self.peek() == Some('=') {
                    self.read_one();
                    return Some(self.token(TokenType::OpEqualsEquals, start));
                }
                Some(self.token(TokenType::OpEquals, start))
            }
            '!' => {
                self.read_one();
                if self.peek() == Some('=') {
                    self.read_one();
                    return Some(self.token(TokenType::OpExlmEquals, start));
                }
                Some(self.token(TokenType::Garbage, start))
            }
            '>' => {
                self.read_one();
                if self.peek() == Some('=') {
                    self.read_one();
                    return Some(self.token(TokenType::OpGreaterEquals, start));
                }
                Some(self.token(TokenType::OpGreater, start))
            }
            '<' => {
                self.read_one();
                if self.peek() == Some('=') {
                    self.read_one();
                    return Some(self.token(TokenType::OpLessEquals, start));
                }
                Some(self.token(TokenType::OpLess, start))
            }
            '-' => {
                self.read_one();
                if self.peek() == Some('=') {
                    self.read_one();
                    return Some(self.token(TokenType::OpMinusEquals, start));
                }
                Some(self.token(TokenType::OpMinus, start))
            }
            '%' => {
                self.read_one();
                if self.peek() == Some('=') {
                    self.read_one();
                    return Some(self.token(TokenType::OpPercentEquals, start));
                }
                Some(self.token(TokenType::OpPercent, start))
            }
            '&' => {
                self.read_one();
                if self.peek() == Some('&') {
                    self.read_one();
                    return Some(self.token(TokenType::OpAmpAmp, start));
                }
                Some(self.token(TokenType::Garbage, start))
            }
            '|' => {
                self.read_one();
                if self.peek() == Some('|') {
                    self.read_one();
                    return Some(self.token(TokenType::OpPipePipe, start));
                }
                Some(self.token(TokenType::Garbage, start))
            }
            '(' => {
                self.read_one();
                Some(self.token(TokenType::ParenOpen, start))
            }
            ')' => {
                self.read_one();
                Some(self.token(TokenType::ParenClose, start))
            }
            '+' => {
                self.read_one();
                if self.peek() == Some('=') {
                    self.read_one();
                    return Some(self.token(TokenType::OpPlusEquals, start));
                }
                Some(self.token(TokenType::OpPlus, start))
            }
            ';' => {
                self.read_one();
                Some(self.token(TokenType::Semicolon, start))
            }
            // number
            _ if c.is_numeric() => {
                let num = self.read_while(|c| c.is_numeric());
                if let Ok(num) = num.parse() {
                    Some(self.token(TokenType::Integer(num), start))
                } else {
                    Some(self.token(TokenType::Garbage, start))
                }
            }
            // name
            _ if c.is_alphabetic() || c == '_' => {
                let name = self.read_while(|c| c.is_alphanumeric() || c == '_');
                match name {
                    "break" => Some(self.token(TokenType::KeywordBreak, start)),
                    "continue" => Some(self.token(TokenType::KeywordContinue, start)),
                    "else" => Some(self.token(TokenType::KeywordElse, start)),
                    "extern" => Some(self.token(TokenType::KeywordExtern, start)),
                    "false" => Some(self.token(TokenType::KeywordFalse, start)),
                    "for" => Some(self.token(TokenType::KeywordFor, start)),
                    "func" => Some(self.token(TokenType::KeywordFunc, start)),
                    "if" => Some(self.token(TokenType::KeywordIf, start)),
                    "import" => Some(self.token(TokenType::KeywordImport, start)),
                    "let" => Some(self.token(TokenType::KeywordLet, start)),
                    "mut" => Some(self.token(TokenType::KeywordMut, start)),
                    "of" => Some(self.token(TokenType::KeywordOf, start)),
                    "return" => Some(self.token(TokenType::KeywordReturn, start)),
                    "struct" => Some(self.token(TokenType::KeywordStruct, start)),
                    "this" => Some(self.token(TokenType::KeywordThis, start)),
                    "true" => Some(self.token(TokenType::KeywordTrue, start)),
                    "while" => Some(self.token(TokenType::KeywordWhile, start)),
                    name => Some(self.name(name.to_string(), start)),
                }
            }
            // string literal
            '"' => {
                self.read_one();
                // TODO: escape sequences
                let name = self.read_while(|c| c != '"');
                self.read_one();
                Some(self.token(TokenType::StringLiteral(name.into()), start))
            }
            // char literal
            '\'' => {
                self.read_one();
                // TODO: escape sequences
                let name = self.read_while(|c| c != '\'');
                self.read_one();
                Some(self.token(TokenType::CharLiteral(name.into()), start))
            }
            _ => {
                self.read_one();
                Some(self.token(TokenType::Garbage, start))
            }
        }
    }
}

impl Iterator for TokenIterator<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.read_token()
    }
}
