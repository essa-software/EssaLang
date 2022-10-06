#include "Parser.hpp"
#include <type_traits>

namespace ESL::Parser {

void ParsedFile::print() const {
    for (auto const& decl : function_declarations) {
        decl.print();
    }
}

void ParsedParameter::print() const {
    fmt::print("{}: ", name.encode());
    type.print();
}

void ParsedFunctionDeclaration::print() const {
    fmt::print("func {}(", name.encode());
    for (size_t s = 0; s < parameters.size(); s++) {
        parameters[s].print();
        if (s != parameters.size() - 1) {
            fmt::print(", ");
        }
    }
    fmt::print(")");
    if (return_type) {
        fmt::print(": ");
        return_type->print();
    }
    fmt::print(" ");
    body->print(0);
}

void ParsedType::print() const {
    switch (primitive) {
    case Primitive::U32:
        fmt::print("u32");
        break;
    default:
        break;
    }
}

void indent(size_t depth) {
    for (size_t s = 0; s < depth; s++) {
        fmt::print("    ");
    }
}

void ParsedVariableDeclaration::print(size_t depth) const {
    indent(depth);
    fmt::print("{} {}", is_mut ? "mut" : "let", name.encode());
    if (type) {
        fmt::print(": ");
        type->print();
    }
    fmt::print(" = ");
    initializer.print(0);
    fmt::print(";");
}

void ParsedIntegerLiteral::print(size_t depth) const {
    indent(depth);
    fmt::print("{}", value);
}

void ParsedStringLiteral::print(size_t depth) const {
    indent(depth);
    fmt::print("\"{}\"", value.encode());
}

void ParsedIdentifier::print(size_t depth) const {
    indent(depth);
    fmt::print("{}", id.encode());
}

void ParsedCall::print(size_t depth) const {
    indent(depth);
    fmt::print("{}(", name.encode());
    for (size_t s = 0; s < arguments.size(); s++) {
        arguments[s].print(0);
        if (s != arguments.size() - 1) {
            fmt::print(", ");
        }
    }
    fmt::print(")");
}

void ParsedReturnStatement::print(size_t depth) const {
    indent(depth);
    fmt::print("return ");
    if (value) {
        value->print(0);
    }
    fmt::print(";");
}

void ParsedExpression::print(size_t depth) const {
    std::visit([depth](auto const& v) -> void {
        v.print(depth);
    },
        expression);
}

void ParsedBlock::print(size_t depth) const {
    fmt::print("{{\n");
    for (auto const& stmt : statements) {
        std::visit([depth](auto const& v) {
            v.print(depth + 1);
            if constexpr (std::is_same_v<std::remove_cvref_t<decltype(v)>, ParsedExpression>) {
                fmt::print(";");
            }
        },
            stmt);
        fmt::print("\n");
    }
    indent(depth);
    fmt::print("}}\n");
}

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
    if (next_token_is(TokenType::ParenClose)) {
        get();
    }
    else {
        while (true) {
            auto name = TRY(expect(TokenType::Identifier));
            TRY(expect(TokenType::Colon));
            auto type = TRY(parse_type());
            declaration.parameters.push_back(ParsedParameter { .type = std::move(type), .name = Util::UString { name.value() } });
            if (!next_token_is(TokenType::Comma))
                break;
        }
        TRY(expect(TokenType::ParenClose));
    }

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
