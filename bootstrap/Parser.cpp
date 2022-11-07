#include "Parser.hpp"
#include <EssaUtil/Config.hpp>
#include <type_traits>

namespace ESL::Parser {

void ParsedFile::print() const {
    for (auto const& mod : modules) {
        mod.print();
    }
}

void ParsedModule::print() const {
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
    fmt::print("{}", name.encode());
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

void ParsedBinaryExpression::print(size_t depth) const {
    indent(depth);
    fmt::print("(");
    lhs.print(0);
    fmt::print("{}", operator_to_string(operator_));
    rhs.print(0);
    fmt::print(")");
}

std::string ParsedBinaryExpression::operator_to_string(Operator op) {
    switch (op) {
    case Operator::Add:
        return "+";
    case Operator::Subtract:
        return "-";
    case Operator::Multiply:
        return "*";
    case Operator::Divide:
        return "/";
    case Operator::Modulo:
        return "%";
    case Operator::IsEqual:
        return "==";
    case Operator::Assign:
        return "=";
    case Operator::AssignAdd:
        return "+=";
    case Operator::AssignSubtract:
        return "-=";
    case Operator::AssignMultiply:
        return "*=";
    case Operator::AssignDivide:
        return "/*=";
    case Operator::AssignModulo:
        return "%=";
    case Operator::Invalid:
        ESSA_UNREACHABLE;
    }
    ESSA_UNREACHABLE;
}

void ParsedReturnStatement::print(size_t depth) const {
    indent(depth);
    fmt::print("return ");
    if (value) {
        value->print(0);
    }
    fmt::print(";");
}

void ParsedIfStatement::print(size_t depth) const {
    indent(depth);
    fmt::print("if (");
    condition.print(depth);
    fmt::print(")\n");
    then_clause->print(depth + 1);
}

void ParsedExpression::print(size_t depth) const {
    std::visit([depth](auto const& v) -> void {
        v->print(depth);
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
    file.modules.resize(2);

    // Hardcode prelude for now
    file.modules[0].function_declarations.push_back(ParsedFunctionDeclaration { .name = "print", .return_type = ParsedType { .name = "void" }, .body = nullptr });

    // Parse root
    while (true) {
        if (next_token_is(TokenType::Eof)) {
            break;
        }
        while (next_token_is(TokenType::Comment)) {
            get();
        }
        auto keyword = peek();
        if (keyword->type() == TokenType::KeywordFunc) {
            file.modules[1].function_declarations.push_back(TRY(parse_function_declaration()));
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
        while (next_token_is(TokenType::Comment)) {
            get();
        }
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
    if (token->type() == TokenType::KeywordIf) {
        return TRY(parse_if_statement());
    }
    auto expr = TRY(parse_expression(0));
    TRY(expect(TokenType::Semicolon));
    return expr;
}

Util::ParseErrorOr<ParsedType> Parser::parse_type() {
    auto token = get();
    if (token->type() == TokenType::KeywordU32) {
        return ParsedType { .name = "u32" };
    }
    return error_in_already_read("Invalid type (TODO: Custom types)");
}

Util::ParseErrorOr<ParsedFunctionDeclaration> Parser::parse_function_declaration() {
    get(); // `func`

    ParsedFunctionDeclaration declaration;
    declaration.name = Util::UString { TRY(expect(TokenType::Identifier)).value() };
    declaration.name_range = range(offset() - 1, 1);

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
        .range = {},
    };
    decl.range.start = range(offset(), 1).start;

    decl.name = Util::UString { TRY(expect(TokenType::Identifier)).value() };

    if (next_token_is(TokenType::Colon)) {
        get(); // :
        decl.type = TRY(parse_type());
    }

    if (!next_token_is(TokenType::EqualSign)) {
        return decl;
    }
    get(); // =
    decl.initializer = TRY(parse_expression(0));
    TRY(expect(TokenType::Semicolon));
    decl.range.end = range(offset() - 1, 1).end;
    return decl;
}

Util::ParseErrorOr<ParsedReturnStatement> Parser::parse_return_statement() {
    Util::SourceRange range;
    range.start = this->range(offset(), 1).start;

    get(); // return

    if (next_token_is(TokenType::Semicolon)) {
        range.end = this->range(offset() - 1, 1).end;
        return ParsedReturnStatement { .value = {}, .range = range };
    }

    auto value = TRY(parse_expression(0));
    TRY(expect(TokenType::Semicolon));
    range.end = this->range(offset() - 1, 1).end;
    return ParsedReturnStatement { .value = std::move(value), .range = range };
}

namespace Precedence {
constexpr int Multiplicative = 15;
constexpr int Additive = 10;
constexpr int Comparison = 7;
constexpr int Assignment = 5;
}

Util::ParseErrorOr<ParsedIfStatement> Parser::parse_if_statement() {
    get(); // if

    TRY(expect(TokenType::ParenOpen));
    auto condition = TRY(parse_expression(Precedence::Assignment + 1));
    TRY(expect(TokenType::ParenClose));

    auto then_clause = TRY(parse_block());
    return ParsedIfStatement { .condition = std::move(condition), .then_clause = std::move(then_clause) };
}

static int precedence(ParsedBinaryExpression::Operator op) {
    switch (op) {
    case ParsedBinaryExpression::Operator::Multiply:
    case ParsedBinaryExpression::Operator::Divide:
    case ParsedBinaryExpression::Operator::Modulo:
        return Precedence::Multiplicative;
    case ParsedBinaryExpression::Operator::Add:
    case ParsedBinaryExpression::Operator::Subtract:
        return Precedence::Additive;
    case ParsedBinaryExpression::Operator::IsEqual:
        return Precedence::Comparison;
    case ParsedBinaryExpression::Operator::Assign:
    case ParsedBinaryExpression::Operator::AssignAdd:
    case ParsedBinaryExpression::Operator::AssignSubtract:
    case ParsedBinaryExpression::Operator::AssignMultiply:
    case ParsedBinaryExpression::Operator::AssignDivide:
    case ParsedBinaryExpression::Operator::AssignModulo:
        return Precedence::Assignment;
    case ParsedBinaryExpression::Operator::Invalid:
        return 100000;
    }
    ESSA_UNREACHABLE;
}

static ParsedBinaryExpression::Operator token_to_binary_operator(TokenType token) {
    switch (token) {
    case TokenType::Plus:
        return ParsedBinaryExpression::Operator::Add;
    case TokenType::Minus:
        return ParsedBinaryExpression::Operator::Subtract;
    case TokenType::Asterisk:
        return ParsedBinaryExpression::Operator::Multiply;
    case TokenType::Slash:
        return ParsedBinaryExpression::Operator::Divide;
    case TokenType::PercentSign:
        return ParsedBinaryExpression::Operator::Modulo;
    case TokenType::EqualEqual:
        return ParsedBinaryExpression::Operator::IsEqual;
    case TokenType::EqualSign:
        return ParsedBinaryExpression::Operator::Assign;
    case TokenType::PlusEqual:
        return ParsedBinaryExpression::Operator::AssignAdd;
    case TokenType::MinusEqual:
        return ParsedBinaryExpression::Operator::AssignSubtract;
    case TokenType::AsteriskEqual:
        return ParsedBinaryExpression::Operator::AssignMultiply;
    case TokenType::SlashEqual:
        return ParsedBinaryExpression::Operator::AssignDivide;
    case TokenType::PercentEqual:
        return ParsedBinaryExpression::Operator::AssignModulo;
    default:
        return ParsedBinaryExpression::Operator::Invalid;
    }
}

Util::ParseErrorOr<ParsedExpression> Parser::parse_operand(ParsedExpression lhs, int min_precedence) {
    auto current_operator = token_to_binary_operator(peek()->type());
    if (current_operator == ParsedBinaryExpression::Operator::Invalid) {
        return lhs;
    }

    while (true) {
        auto operator_range = range(offset(), 1);
        current_operator = token_to_binary_operator(peek()->type());
        if (current_operator == ParsedBinaryExpression::Operator::Invalid) {
            return lhs;
        }
        auto current_precedence = precedence(current_operator);
        if (current_precedence <= min_precedence) {
            return lhs;
        }
        get();

        auto rhs = TRY(parse_expression(current_precedence));

        auto next_operator = token_to_binary_operator(peek()->type());
        auto next_precedence = precedence(next_operator);
        if (current_precedence >= next_precedence) {
            lhs = ParsedExpression {
                .expression = std::make_unique<ParsedBinaryExpression>(ParsedBinaryExpression {
                    .lhs = std::move(lhs),
                    .operator_ = current_operator,
                    .rhs = std::move(rhs),
                    .operator_range = operator_range,
                }),
            };
        }
        else {
            lhs = ParsedExpression {
                .expression = std::make_unique<ParsedBinaryExpression>(ParsedBinaryExpression {
                    .lhs = std::move(lhs),
                    .operator_ = current_operator,
                    .rhs = TRY(parse_operand(std::move(rhs), current_precedence)),
                    .operator_range = operator_range,
                }),
            };
        }
    }
}

Util::ParseErrorOr<ParsedExpression> Parser::parse_primary_expression() {
    auto token = get();
    if (token->type() == TokenType::Number) {
        try {
            return ParsedExpression { .expression = std::make_unique<ParsedIntegerLiteral>(ParsedIntegerLiteral { .value = std::stoll(token->value()) }) };
        } catch (...) {
            return error_in_already_read("Invalid integer literal");
        }
    }
    if (token->type() == TokenType::StringLiteral) {
        return ParsedExpression { .expression = std::make_unique<ParsedStringLiteral>(ParsedStringLiteral { .value = Util::UString { token->value() } }) };
    }
    if (token->type() == TokenType::Identifier) {
        Util::UString id { token->value() };
        if (next_token_is(TokenType::ParenOpen)) {
            auto name_range = range(offset() - 1, 1);
            auto call = TRY(parse_call_arguments(std::move(id)));
            call->name_range = name_range;
            return ParsedExpression { .expression = std::move(call) };
        }
        return ParsedExpression {
            .expression = std::make_unique<ParsedIdentifier>(ParsedIdentifier {
                .id = std::move(id),
                .range = range(offset() - 1, 1),
            })
        };
    }
    return expected("expression", *token);
}

Util::ParseErrorOr<ParsedExpression> Parser::parse_expression(int min_precedence) {
    auto lhs = TRY(parse_primary_expression());
    return TRY(parse_operand(std::move(lhs), min_precedence));
}

Util::ParseErrorOr<std::unique_ptr<ParsedCall>> Parser::parse_call_arguments(Util::UString id) {
    get(); // (
    if (next_token_is(TokenType::ParenClose)) {
        get(); // )
        return std::make_unique<ParsedCall>(ParsedCall { .name = std::move(id), .arguments = {} });
    }
    std::vector<ParsedExpression> arguments;
    while (true) {
        arguments.push_back(TRY(parse_expression(0)));
        if (!next_token_is(TokenType::Comma)) {
            break;
        }
        get();
    }
    TRY(expect(TokenType::ParenClose));
    return std::make_unique<ParsedCall>(ParsedCall { .name = std::move(id), .arguments = std::move(arguments) });
}

}
