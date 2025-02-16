use std::{
    ops::Range,
    str::{self, Chars, Utf8Error},
};

#[derive(Debug)]
pub enum TokenType {
    Colon,
    Comma,
    CurlyClose,
    CurlyOpen,
    Integer(u64),
    KeywordFalse,
    KeywordFunc,
    KeywordIf,
    KeywordLet,
    KeywordMut,
    KeywordReturn,
    KeywordTrue,
    Name(String),
    OpAsterisk,       // *
    OpAsteriskEquals, // *=
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
    OpPlus,           // +
    OpPlusEquals,     // +=
    OpSlash,          // /=
    OpSlashEquals,    // /=
    ParenClose,
    ParenOpen,
    Semicolon,
    StringLiteral(String),
    Garbage,
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
            ':' => {
                self.read_one();
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
            // keywords
            'f' => {
                let kw = self.read_while(|c| c.is_alphabetic());
                match kw {
                    "false" => Some(self.token(TokenType::KeywordFalse, start)),
                    "func" => Some(self.token(TokenType::KeywordFunc, start)),
                    _ => Some(self.name(kw.to_string(), start)),
                }
            }
            'i' => {
                let kw = self.read_while(|c| c.is_alphabetic());
                match kw {
                    "if" => Some(self.token(TokenType::KeywordIf, start)),
                    _ => Some(self.name(kw.to_string(), start)),
                }
            }
            'l' => {
                let kw = self.read_while(|c| c.is_alphabetic());
                match kw {
                    "let" => Some(self.token(TokenType::KeywordLet, start)),
                    _ => Some(self.name(kw.to_string(), start)),
                }
            }
            'm' => {
                let kw = self.read_while(|c| c.is_alphabetic());
                match kw {
                    "mut" => Some(self.token(TokenType::KeywordMut, start)),
                    _ => Some(self.name(kw.to_string(), start)),
                }
            }
            'r' => {
                let kw = self.read_while(|c| c.is_alphabetic());
                match kw {
                    "return" => Some(self.token(TokenType::KeywordReturn, start)),
                    _ => Some(self.name(kw.to_string(), start)),
                }
            }
            't' => {
                let kw = self.read_while(|c| c.is_alphabetic());
                match kw {
                    "true" => Some(self.token(TokenType::KeywordTrue, start)),
                    _ => Some(self.name(kw.to_string(), start)),
                }
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
            _ if c.is_alphabetic() => {
                let name = self.read_while(|c| c.is_alphanumeric());
                Some(self.name(name.to_string(), start))
            }
            // string literal
            '"' => {
                self.read_one();
                // TODO: escape sequences
                let name = self.read_while(|c| c != '"');
                self.read_one();
                Some(self.token(TokenType::StringLiteral(name.into()), start))
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
