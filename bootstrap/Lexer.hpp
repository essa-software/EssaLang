#pragma once

#include <EssaUtil/Config.hpp>
#include <EssaUtil/Enum.hpp>
#include <EssaUtil/GenericParser.hpp>
namespace ESL {

// LEXER

// Rules for adding tokens:
// 1. Name tokens what they looks like, not what they do, for example,
//    KeywordOf instead of KeywordIterate. This makes keywords easier
//    to reuse.
// 2. Keep this list alphabetically, with the blocks separated.
//     - The same about handlers in lex()
#define TOKENS(E)                                                      \
    /* Keywords */                                                     \
    E(KeywordBool, "bool")                                             \
    E(KeywordElse, "else")                                             \
    E(KeywordExtern, "extern")                                         \
    E(KeywordFor, "for")                                               \
    E(KeywordFunc, "func")                                             \
    E(KeywordIf, "if")                                                 \
    E(KeywordImport, "import")                                         \
    E(KeywordLet, "let")                                               \
    E(KeywordMut, "mut")                                               \
    E(KeywordOf, "of")                                                 \
    E(KeywordReturn, "return")                                         \
    E(KeywordString, "string")                                         \
    E(KeywordStruct, "struct")                                         \
    E(KeywordU32, "u32")                                               \
    E(KeywordVoid, "void")                                             \
    E(KeywordWhile, "while")                                           \
    /* Operators */                                                    \
    E(Asterisk, "*")                                                   \
    E(AsteriskEqual, "*=")                                             \
    E(BraceClose, "]")                                                 \
    E(BraceOpen, "[")                                                  \
    E(Colon, ":")                                                      \
    E(Comma, ",")                                                      \
    E(CurlyClose, "}")                                                 \
    E(CurlyOpen, "{")                                                  \
    E(Dot, ".")                                                        \
    E(DotDot, "..")                                                    \
    E(EqualEqual, "==")                                                \
    E(EqualSign, "=")                                                  \
    E(ExclEqual, "")                                                   \
    E(Greater, "")                                                     \
    E(GreaterEqual, "")                                                \
    E(Less, "")                                                        \
    E(LessEqual, "")                                                   \
    E(Minus, "-")                                                      \
    E(MinusEqual, "-=")                                                \
    E(ParenClose, ")")                                                 \
    E(ParenOpen, "(")                                                  \
    E(PercentEqual, "%=")                                              \
    E(PercentSign, "%")                                                \
    E(Plus, "+")                                                       \
    E(PlusEqual, "+=")                                                 \
    E(Semicolon, ";")                                                  \
    E(Slash, "/")                                                      \
    E(SlashEqual, "/=")                                                \
    /* Misc */                                                         \
    E(Comment, "Comment")                                              \
    E(Identifier, "Identifier")                                        \
    E(Number, "Number")                                                \
    E(StringLiteral, "StringLiteral")                                  \
    E(CharLiteral, "CharLiteral")                                      \
    /* End of file, added at the end. Makes overflowing                \
       the token list much harder. */                                  \
    E(Eof, "<EOF>")                                                    \
    /* Everything else. Probably will be rejected at parsing stage. */ \
    E(Garbage, "Garbage")

ESSA_ENUM(TokenType, TOKENS);

#define _TO_STRING(Name, String) \
    case Enum::Name:             \
        return String;

inline std::string_view token_to_string(TokenType tt) {
    using Enum = TokenType;
    switch (tt) {
        TOKENS(_TO_STRING);
    default:
        ESSA_UNREACHABLE;
    }
}

using Token = Util::Token<TokenType>;

class Lexer : public Util::GenericLexer<TokenType> {
public:
    explicit Lexer(Util::ReadableStream& stream)
        : Util::GenericLexer<TokenType>(stream) {
    }

    Util::OsErrorOr<std::vector<Token>> lex();
};

}
