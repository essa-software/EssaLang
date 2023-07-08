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
            if (string == "bool") {
                tokens.push_back(create_token(TokenType::KeywordBool, "bool", start));
            }
            else if (string == "else") {
                tokens.push_back(create_token(TokenType::KeywordElse, "else", start));
            }
            else if (string == "extern") {
                tokens.push_back(create_token(TokenType::KeywordExtern, "extern", start));
            }
            else if (string == "for") {
                tokens.push_back(create_token(TokenType::KeywordFor, "for", start));
            }
            else if (string == "func") {
                tokens.push_back(create_token(TokenType::KeywordFunc, "func", start));
            }
            else if (string == "if") {
                tokens.push_back(create_token(TokenType::KeywordIf, "if", start));
            }
            else if (string == "import") {
                tokens.push_back(create_token(TokenType::KeywordImport, "import", start));
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
            else if (string == "string") {
                tokens.push_back(create_token(TokenType::KeywordString, "string", start));
            }
            else if (string == "struct") {
                tokens.push_back(create_token(TokenType::KeywordStruct, "struct", start));
            }
            else if (string == "u32") {
                tokens.push_back(create_token(TokenType::KeywordU32, "u32", start));
            }
            else if (string == "void") {
                tokens.push_back(create_token(TokenType::KeywordVoid, "void", start));
            }
            else if (string == "while") {
                tokens.push_back(create_token(TokenType::KeywordWhile, "while", start));
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
            // TODO: Handle escaping
            TRY(consume());
            auto string = TRY(consume_while([](uint8_t c) {
                return c != '"';
            }));
            if (!TRY(consume())) {
                fmt::print("TODO: Handle unclosed string literal\n");
            }
            tokens.push_back(create_token(TokenType::StringLiteral, string, start));
        }
        else if (*current == '\'') {
            // TODO: Handle escaping
            TRY(consume());
            auto string = TRY(consume_while([](uint8_t c) {
                return c != '\'';
            }));
            if (!TRY(consume())) {
                fmt::print("TODO: Handle unclosed char literal\n");
            }
            tokens.push_back(create_token(TokenType::CharLiteral, string, start));
        }
        else if (*current == '/') {
            // Comments
            TRY(consume());
            if (TRY(peek()) == '/') {
                TRY(consume_until('\n'));
                // FIXME: Add an option for that
                // tokens.push_back(create_token(TokenType::Comment, "/" + string, start));
            }
            else if (TRY(peek()) == '=') {
                TRY(consume());
                tokens.push_back(create_token(TokenType::SlashEqual, "/=", start));
            }
            else {
                tokens.push_back(create_token(TokenType::Slash, "/", start));
            }
        }
        else {
            TokenType operator_type = TokenType::Garbage;
            TRY(consume());
            std::string value { (char*)&*current, 1 };
            switch (*current) {
            case '*':
                if (TRY(peek()) == '=') {
                    TRY(consume());
                    value += '=';
                    operator_type = TokenType::AsteriskEqual;
                    break;
                }
                operator_type = TokenType::Asterisk;
                break;
            case '[':
                operator_type = TokenType::BraceOpen;
                break;
            case ']':
                operator_type = TokenType::BraceClose;
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
                    value += '.';
                    operator_type = TokenType::DotDot;
                    break;
                }
                operator_type = TokenType::Dot;
                break;
            case '=':
                if (TRY(peek()) == '=') {
                    TRY(consume());
                    value += '=';
                    operator_type = TokenType::EqualEqual;
                    break;
                }
                operator_type = TokenType::EqualSign;
                break;
            case '!':
                if (TRY(peek()) == '=') {
                    TRY(consume());
                    value += '=';
                    operator_type = TokenType::ExclEqual;
                    break;
                }
                // FIXME: Use as negation etc.
                operator_type = TokenType::Garbage;
                break;
            case '>':
                if (TRY(peek()) == '=') {
                    TRY(consume());
                    value += '=';
                    operator_type = TokenType::GreaterEqual;
                    break;
                }
                operator_type = TokenType::Greater;
                break;
            case '<':
                if (TRY(peek()) == '=') {
                    TRY(consume());
                    value += '=';
                    operator_type = TokenType::LessEqual;
                    break;
                }
                operator_type = TokenType::Less;
                break;
            case '-':
                if (TRY(peek()) == '=') {
                    TRY(consume());
                    value += '=';
                    operator_type = TokenType::MinusEqual;
                    break;
                }
                operator_type = TokenType::Minus;
                break;
            case '(':
                operator_type = TokenType::ParenOpen;
                break;
            case ')':
                operator_type = TokenType::ParenClose;
                break;
            case '%':
                if (TRY(peek()) == '=') {
                    TRY(consume());
                    value += '=';
                    operator_type = TokenType::PercentEqual;
                    break;
                }
                operator_type = TokenType::PercentSign;
                break;
            case '+':
                if (TRY(peek()) == '=') {
                    TRY(consume());
                    value += '=';
                    operator_type = TokenType::PlusEqual;
                    break;
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
            tokens.push_back(create_token(operator_type, Util::UString { value }, start));
        }
    }
    tokens.push_back(create_token(TokenType::Eof, "<EOF>", location()));
    return tokens;
}

}
