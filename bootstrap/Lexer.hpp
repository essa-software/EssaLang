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
    KeywordFor,
    KeywordFunc,
    KeywordLet,
    KeywordMut,
    KeywordOf,
    KeywordReturn,
    KeywordU32,

    // Operators based on special characters
    Asterisk,    // *
    Colon,       // :
    Comma,       // ,
    CurlyOpen,   // {
    CurlyClose,  // }
    Dot,         // .
    DotDot,      // ..
    EqualSign,   // =
    Minus,       // -
    ParenOpen,   // (
    ParenClose,  // )
    PercentSign, // %
    Plus,        // +
    PlusEqual,   // +=
    Semicolon,   // ;
    Slash,       // /

    // Miscellaneous
    Comment,
    Identifier,
    Number,
    StringLiteral,

    // End of file, added at the end. Makes overflowing the token
    // list much harder.
    Eof,

    // Everything else. Probably will be rejected at parting stage.
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
