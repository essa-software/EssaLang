#include "Lexer.hpp"
#include <EssaUtil/Config.hpp>

namespace ESL {

Util::OsErrorOr<std::vector<Token>> Lexer::lex() {
    std::vector<Token> tokens;
    while (!is_eof()) {
        TRY(consume_while([](uint8_t i) { return isspace(i); }));
        auto current = TRY(peek());
        if (!current) {
            break;
        }
        auto start = location();

        // Identifiers + Keywords
        if (isalpha(*current) || *current == '_') {
            auto string = TRY(consume_while([](uint8_t c) {
                return isalnum(c) || c == '_';
            }));
            if (string == "for") {
                tokens.push_back(create_token(TokenType::KeywordFor, "for", start));
            }
            else if (string == "func") {
                tokens.push_back(create_token(TokenType::KeywordFunc, "func", start));
            }
            else if (string == "let") {
                tokens.push_back(create_token(TokenType::KeywordLet, "let", start));
            }
            else if (string == "mut") {
                tokens.push_back(create_token(TokenType::KeywordMut, "mut", start));
            }
            else if (string == "of") {
                tokens.push_back(create_token(TokenType::KeywordOf, "of", start));
            }
            else if (string == "return") {
                tokens.push_back(create_token(TokenType::KeywordReturn, "return", start));
            }
            else if (string == "u32") {
                tokens.push_back(create_token(TokenType::KeywordU32, "u32", start));
            }
            else {
                tokens.push_back(create_token(TokenType::Identifier, string, start));
            }
        }
        else if (isdigit(*current)) {
            // TODO: Handle floats.
            auto number = TRY(consume_while([](uint8_t c) {
                return isdigit(c);
            }));
            tokens.push_back(create_token(TokenType::Number, number, start));
        }
        else if (*current == '"') {
            TRY(consume());
            auto string = TRY(consume_while([](uint8_t c) {
                return c != '"';
            }));
            if (!TRY(consume())) {
                fmt::print("TODO: Handle unclosed string literal\n");
            }
            tokens.push_back(create_token(TokenType::StringLiteral, string, start));
        }
        else if (*current == '/') {
            // Comments
            TRY(consume());
            if (TRY(peek()) == '/') {
                auto string = TRY(consume_until('\n'));
                tokens.push_back(create_token(TokenType::Comment, "/" + string, start));
            }
            else {
                tokens.push_back(create_token(TokenType::Slash, "/", start));
            }
        }
        else {
            TokenType operator_type = TokenType::Garbage;
            TRY(consume());
            switch (*current) {
            case '*':
                operator_type = TokenType::Asterisk;
                break;
            case ':':
                operator_type = TokenType::Colon;
                break;
            case ',':
                operator_type = TokenType::Comma;
                break;
            case '{':
                operator_type = TokenType::CurlyOpen;
                break;
            case '}':
                operator_type = TokenType::CurlyClose;
                break;
            case '.':
                if (TRY(peek()) == '.') {
                    TRY(consume());
                    operator_type = TokenType::DotDot;
                }
                operator_type = TokenType::Dot;
                break;
            case '=':
                operator_type = TokenType::EqualSign;
                break;
            case '-':
                operator_type = TokenType::Minus;
                break;
            case '(':
                operator_type = TokenType::ParenOpen;
                break;
            case ')':
                operator_type = TokenType::ParenClose;
                break;
            case '%':
                operator_type = TokenType::PercentSign;
                break;
            case '+':
                if (TRY(peek()) == '=') {
                    TRY(consume());
                    operator_type = TokenType::PlusEqual;
                }
                operator_type = TokenType::Plus;
                break;
            case ';':
                operator_type = TokenType::Semicolon;
                break;
            case '/':
                // Handled in comments
                ESSA_UNREACHABLE;
                break;
            default:
                break;
            }
            tokens.push_back(create_token(operator_type, { (char*)&*current, 1 }, start));
        }
    }
    tokens.push_back(create_token(TokenType::Eof, "<EOF>", location()));
    return tokens;
}

}
