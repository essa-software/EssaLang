#include <EssaUtil/Error.hpp>
#include <EssaUtil/GenericParser.hpp>
#include <EssaUtil/Stream.hpp>
#include <EssaUtil/Stream/File.hpp>
#include <cctype>
#include <map>
#include <memory>
#include <type_traits>

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
    Colon,      // :
    Comma,      // ,
    CurlyOpen,  // {
    CurlyClose, // }
    Dot,        // .
    DotDot,     // ..
    EqualSign,  // =
    Minus,      // -
    ParenOpen,  // (
    ParenClose, // )
    Plus,       // +
    PlusEqual,  // +=
    Semicolon,  // ;

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

    Util::OsErrorOr<std::vector<Token>> lex() {
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
            }
            else {
                TokenType operator_type = TokenType::Garbage;
                TRY(consume());
                switch (*current) {
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
                default:
                    break;
                }
                tokens.push_back(create_token(operator_type, { (char*)&*current, 1 }, start));
            }
        }
        tokens.push_back(create_token(TokenType::Eof, "<EOF>", location()));
        return tokens;
    }
};

// TODO: PARSER
namespace Parser {

struct ParsedType {
    enum class Primitive {
        U32
    };

    Primitive primitive;
};

struct ParsedBlock;

struct ParsedParameter {
    ParsedType type;
    Util::UString name;
};

struct ParsedFunctionDeclaration {
    Util::UString name;
    ParsedType return_type;
    std::vector<ParsedParameter> parameters;
    std::unique_ptr<ParsedBlock> body;
};

struct ParsedIntegerLiteral {
    int64_t value;
};
struct ParsedStringLiteral {
    Util::UString value;
};
struct ParsedIdentifier {
    Util::UString id;
};
struct ParsedExpression;
struct ParsedCall {
    Util::UString name; // FIXME: This should be an expression
    std::vector<ParsedExpression> arguments;
};

struct ParsedExpression {
    // Workaround for C++ needing class declarations before usage
    std::variant<
        ParsedIntegerLiteral,
        ParsedStringLiteral,
        ParsedIdentifier,
        ParsedCall>
        expression;
};

struct ParsedVariableDeclaration {
    bool is_mut;
    Util::UString name;
    std::optional<ParsedType> type;
    ParsedExpression initializer;
};

struct ParsedReturnStatement {
    std::optional<ParsedExpression> value;
};

using ParsedStatement = std::variant<
    ParsedVariableDeclaration,
    ParsedReturnStatement,
    ParsedExpression>;

struct ParsedBlock {
    std::vector<ParsedStatement> statements;
};

struct ParsedFile {
    std::vector<ParsedFunctionDeclaration> function_declarations;
};

class Parser : public Util::GenericParser<TokenType> {
public:
    explicit Parser(std::vector<Token> tokens)
        : Util::GenericParser<TokenType>(std::move(tokens)) { }

    Util::ParseErrorOr<ParsedFile> parse_file();

private:
    Util::ParseErrorOr<std::unique_ptr<ParsedBlock>> parse_block();
    Util::ParseErrorOr<ParsedStatement> parse_statement();
    Util::ParseErrorOr<ParsedType> parse_type();
    Util::ParseErrorOr<ParsedFunctionDeclaration> parse_function_declaration();
    Util::ParseErrorOr<ParsedVariableDeclaration> parse_variable_declaration();
    Util::ParseErrorOr<ParsedReturnStatement> parse_return_statement();
    Util::ParseErrorOr<ParsedExpression> parse_expression();
    Util::ParseErrorOr<ParsedCall> parse_call_arguments(Util::UString id);
};

template<typename... Deps>
inline constexpr bool dependent_false = false;

Util::ParseErrorOr<ParsedFile> Parser::parse_file() {
    ParsedFile file;
    while (true) {
        if (next_token_is(TokenType::Eof)) {
            break;
        }
        while (next_token_is(TokenType::Comment)) {
            get();
        }
        auto keyword = peek();
        if (keyword->type() == TokenType::KeywordFunc) {
            file.function_declarations.push_back(TRY(parse_function_declaration()));
        }
        else {
            return error("Invalid top-level declaration");
        }
    }
    return file;
}

Util::ParseErrorOr<std::unique_ptr<ParsedBlock>> Parser::parse_block() {
    TRY(expect(TokenType::CurlyOpen));
    auto block = std::make_unique<ParsedBlock>();
    while (true) {
        if (next_token_is(TokenType::CurlyClose))
            break;
        block->statements.push_back(TRY(parse_statement()));
    }
    TRY(expect(TokenType::CurlyClose));
    return block;
}

Util::ParseErrorOr<ParsedStatement> Parser::parse_statement() {
    auto token = peek();
    if (token->type() == TokenType::KeywordLet || token->type() == TokenType::KeywordMut) {
        return TRY(parse_variable_declaration());
    }
    if (token->type() == TokenType::KeywordReturn) {
        return TRY(parse_return_statement());
    }
    auto expr = TRY(parse_expression());
    TRY(expect(TokenType::Semicolon));
    return expr;
}

Util::ParseErrorOr<ParsedType> Parser::parse_type() {
    auto token = get();
    if (token->type() == TokenType::KeywordU32) {
        return ParsedType { .primitive = ParsedType::Primitive::U32 };
    }
    return error_in_already_read("Invalid type (TODO: Custom types)");
}

Util::ParseErrorOr<ParsedFunctionDeclaration> Parser::parse_function_declaration() {
    get(); // `func`

    ParsedFunctionDeclaration declaration;
    declaration.name = Util::UString { TRY(expect(TokenType::Identifier)).value() };

    TRY(expect(TokenType::ParenOpen));
    // TODO: Parameters
    TRY(expect(TokenType::ParenClose));

    // Return type
    if (next_token_is(TokenType::Colon)) {
        get();
        declaration.return_type = TRY(parse_type());
    }

    declaration.body = TRY(parse_block());

    return declaration;
}

Util::ParseErrorOr<ParsedVariableDeclaration> Parser::parse_variable_declaration() {
    auto let_or_mut = get();
    ParsedVariableDeclaration decl {
        .is_mut = let_or_mut->type() == TokenType::KeywordMut ? true : false,
        .name = "",
        .type = {},
        .initializer = {},
    };

    decl.name = Util::UString { TRY(expect(TokenType::Identifier)).value() };

    if (!next_token_is(TokenType::Colon)) {
        return decl;
    }
    get(); // :

    decl.type = TRY(parse_type());

    if (!next_token_is(TokenType::EqualSign)) {
        return decl;
    }
    get(); // =
    decl.initializer = TRY(parse_expression());
    TRY(expect(TokenType::Semicolon));
    return decl;
}

Util::ParseErrorOr<ParsedReturnStatement> Parser::parse_return_statement() {
    get(); // return

    if (next_token_is(TokenType::Semicolon)) {
        return ParsedReturnStatement {};
    }
    auto value = TRY(parse_expression());
    TRY(expect(TokenType::Semicolon));
    return ParsedReturnStatement { .value = std::move(value) };
}

Util::ParseErrorOr<ParsedExpression> Parser::parse_expression() {
    auto token = get();
    if (token->type() == TokenType::Number) {
        try {
            return ParsedExpression { .expression = ParsedIntegerLiteral { .value = std::stoll(token->value()) } };
        } catch (...) {
            return error_in_already_read("Invalid integer literal");
        }
    }
    if (token->type() == TokenType::StringLiteral) {
        return ParsedExpression { .expression = ParsedStringLiteral { .value = Util::UString { token->value() } } };
    }
    if (token->type() == TokenType::Identifier) {
        Util::UString id { token->value() };
        if (next_token_is(TokenType::ParenOpen)) {
            return ParsedExpression { .expression = TRY(parse_call_arguments(std::move(id))) };
        }
        return ParsedExpression { .expression = ParsedIdentifier { .id = std::move(id) } };
    }
    return expected("expression", *token);
}

Util::ParseErrorOr<ParsedCall> Parser::parse_call_arguments(Util::UString id) {
    ParsedCall call { .name = std::move(id), .arguments = {} };
    get(); // (
    if (next_token_is(TokenType::ParenClose)) {
        get(); // )
        return call;
    }
    while (true) {
        call.arguments.push_back(TRY(parse_expression()));
        if (!next_token_is(TokenType::Comma)) {
            break;
        }
        get();
    }
    TRY(expect(TokenType::ParenClose));
    return call;
}

}

// TODO: TYPECHECKER
// TODO: CODEGEN

Util::OsErrorOr<void> run_esl(std::string const& file_name) {
    auto stream = TRY(Util::ReadableFileStream::open(file_name));

    Lexer lexer { stream };
    auto tokens = TRY(lexer.lex());

    for (auto const& token : tokens) {
        fmt::print("{} {}\n", (int)token.type(), token.value());
    }

    Parser::Parser parser { tokens };
    auto parsed_file = parser.parse_file();
    if (parsed_file.is_error()) {
        auto error = parsed_file.release_error();
        fmt::print("Parse error: {} at {}:{}\n", error.message, error.location.start.line + 1, error.location.start.column + 1);
    }

    return {};
}

int main(int argc, char** argv) {
    if (argc < 2) {
        fmt::print("Usage: esl <file name>\n");
        return 1;
    }
    auto err = run_esl(argv[1]);
    if (err.is_error()) {
        auto error = err.release_error();
        fmt::print("Failed to open esl: {}: {}\n", error.function, strerror(error.error));
        return 1;
    }
    return 0;
}
