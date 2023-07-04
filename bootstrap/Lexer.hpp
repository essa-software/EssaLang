#pragma once

#include <EssaUtil/GenericParser.hpp>
namespace ESL {

// LEXER

// Rules for adding tokens:
// 1. Name tokens what they looks like, not what they do, for example,
//    KeywordOf instead of KeywordIterate. This makes keywords easier
//    to reuse.
// 2. Keep this list alphabetically, with the blocks separated.
//     - The same about handlers in lex()
enum class TokenType {
    // Keywords
    KeywordBool,
    KeywordElse,
    KeywordFor,
    KeywordFunc,
    KeywordIf,
    KeywordImport,
    KeywordLet,
    KeywordMut,
    KeywordOf,
    KeywordReturn,
    KeywordString,
    KeywordStruct,
    KeywordU32,
    KeywordVoid,

    // Operators based on special characters
    Asterisk,      // *
    AsteriskEqual, // *=
    BraceOpen,     // [
    BraceClose,    // ]
    Colon,         // :
    Comma,         // ,
    CurlyOpen,     // {
    CurlyClose,    // }
    Dot,           // .
    DotDot,        // ..
    EqualEqual,    // ==
    EqualSign,     // =
    Minus,         // -
    MinusEqual,    // -=
    ParenOpen,     // (
    ParenClose,    // )
    PercentSign,   // %
    PercentEqual,  // %=
    Plus,          // +
    PlusEqual,     // +=
    Semicolon,     // ;
    Slash,         // /
    SlashEqual,    // /=

    // Miscellaneous
    Comment,
    Identifier,
    Number,
    StringLiteral,

    // End of file, added at the end. Makes overflowing the token
    // list much harder.
    Eof,

    // Everything else. Probably will be rejected at parsing stage.
    Garbage
};

using Token = Util::Token<TokenType>;

class Lexer : public Util::GenericLexer<TokenType> {
public:
    explicit Lexer(Util::ReadableStream& stream)
        : Util::GenericLexer<TokenType>(stream) {
    }

    Util::OsErrorOr<std::vector<Token>> lex();
};

}
