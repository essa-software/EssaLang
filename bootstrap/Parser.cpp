#include "Parser.hpp"
#include <EssaUtil/Config.hpp>
#include <EssaUtil/ErrorMappers.hpp>
#include <EssaUtil/GenericParser.hpp>
#include <EssaUtil/SourceLocation.hpp>
#include <type_traits>

namespace ESL::Parser {

void ParsedFile::print() const {
    module.print();
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
    fmt::print("{}", to_string().encode());
}

Util::UString ParsedType::to_string() const {
    return std::visit(
        Util::Overloaded {
            [](ParsedUnqualifiedType const& t) { return t.name; },
            [](ParsedArrayType const& t) {
                return Util::UString::format("[{}]", t.size);
            } },
        type);
}

void ParsedUnqualifiedType::print() const {
    fmt::print("{}", name.encode());
}

void ParsedArrayIndex::print(size_t depth) const {
    array.print(depth);
    fmt::print("[");
    index.print(depth);
    fmt::print("]");
}

void ParsedMemberAccess::print(size_t depth) const {
    object.print(depth);
    fmt::print(".{}", member.encode());
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

void ParsedInlineArray::print(size_t depth) const {
    indent(depth);
    fmt::print("[...TODO...]");
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
    case Operator::Range:
        return "..";
    case Operator::Multiply:
        return "*";
    case Operator::Divide:
        return "/";
    case Operator::Modulo:
        return "%";
    case Operator::Add:
        return "+";
    case Operator::Subtract:
        return "-";
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
    if (else_clause) {
        fmt::print("else {{ TODO }}\n");
    }
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

void ParsedForStatement::print(size_t depth) const {
    indent(depth);
    fmt::print("for (TODO in TODO) {{ TODO }}");
}

Util::ParseErrorOr<ParsedFile> Parser::parse_file() {
    ParsedFile file;

    // Parse root
    while (true) {
        if (next_token_is(TokenType::Eof)) {
            break;
        }
        while (next_token_is(TokenType::Comment)) {
            get();
        }
        auto keyword = peek();
        if (keyword->type() == TokenType::KeywordFunc || keyword->type() == TokenType::KeywordExtern) {
            file.module.function_declarations.push_back(TRY(parse_function_declaration()));
        }
        else if (keyword->type() == TokenType::KeywordStruct) {
            file.module.struct_declarations.push_back(TRY(parse_struct_declaration()));
        }
        else if (keyword->type() == TokenType::KeywordImport) {
            file.module.imports.push_back(TRY(parse_import()));
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
    if (token->type() == TokenType::KeywordFor) {
        return TRY(parse_for_statement());
    }
    auto expr = TRY(parse_expression(0));
    TRY(expect(TokenType::Semicolon));
    return expr;
}

Util::ParseErrorOr<ParsedArrayType> Parser::parse_array_type() {
    get(); // [
    auto size_token = TRY(expect(TokenType::Number));
    auto size = TRY(size_token.value().parse<size_t>().map_error(Util::OsToParseError(size_token.range())));
    TRY(expect(TokenType::BraceClose)); // ]

    return ParsedArrayType { .size = size, .type = std::make_unique<ParsedType>(TRY(parse_type())) };
}

Util::ParseErrorOr<ParsedInlineArray> Parser::parse_inline_array() {
    auto start = this->offset();
    get(); // [
    auto elements = TRY(parse_expression_list(TokenType::BraceClose));
    TRY(expect(TokenType::BraceClose)); // ]
    return ParsedInlineArray { .elements = std::move(elements), .range = this->range(start, this->offset() - start) };
}

Util::ParseErrorOr<ParsedType> Parser::parse_type() {
    auto token = peek();

    if (token->type() == Token::Type::BraceOpen) {
        auto start = this->offset();
        return ParsedType { .type = TRY(parse_array_type()), .range = this->range(start, this->offset() - start) };
    }

    get();

    auto range = this->range(this->offset() - 1, 1);

    if (token->type() == TokenType::KeywordBool) {
        return ParsedType { .type = ParsedUnqualifiedType { .name = "bool" }, .range = range };
    }
    if (token->type() == TokenType::KeywordString) {
        return ParsedType { .type = ParsedUnqualifiedType { .name = "string" }, .range = range };
    }
    if (token->type() == TokenType::KeywordU32) {
        return ParsedType { .type = ParsedUnqualifiedType { .name = "u32" }, .range = range };
    }
    if (token->type() == TokenType::KeywordVoid) {
        return ParsedType { .type = ParsedUnqualifiedType { .name = "void" }, .range = range };
    }
    return ParsedType {
        .type = ParsedUnqualifiedType { .name = token->value() },
        .range = range,
    };
}

Util::ParseErrorOr<ParsedFunctionDeclaration> Parser::parse_function_declaration() {
    bool extern_ = get()->type() == TokenType::KeywordExtern; // `func` | `extern`
    if (extern_) {
        TRY(expect(TokenType::KeywordFunc));
    }

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

    if (extern_) {
        TRY(expect(TokenType::Semicolon));
    }
    else {
        declaration.body = TRY(parse_block());
    }

    return declaration;
}

Util::ParseErrorOr<ParsedStructDeclaration> Parser::parse_struct_declaration() {
    // 'struct' name '{' fields... '}'

    get(); // "struct"
    auto name = TRY(expect(TokenType::Identifier));

    std::vector<ParsedStructDeclaration::Field> fields;

    TRY(expect(TokenType::CurlyOpen));

    while (true) {
        if (next_token_is(TokenType::CurlyClose)) {
            break;
        }
        // name ':' type ';'
        auto name = TRY(expect(TokenType::Identifier));
        TRY(expect(TokenType::Colon));
        auto type = TRY(parse_type());
        TRY(expect(TokenType::Semicolon));

        fields.push_back(ParsedStructDeclaration::Field { .name = name.value(), .type = std::move(type) });
    }

    // Note: We won't exit loop unless we get '}', so this shouldn't fail
    MUST(expect(TokenType::CurlyClose));

    return ParsedStructDeclaration {
        name.value(),
        std::move(fields),
    };
}

// 'import' name ';'
Util::ParseErrorOr<ParsedImport> Parser::parse_import() {
    get();
    auto name = TRY(expect(TokenType::Identifier)).value();
    TRY(expect(TokenType::Semicolon));
    return ParsedImport { .module = std::move(name) };
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
constexpr int Range = 20;
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

    if (peek()->type() == TokenType::KeywordElse) {
        get(); // else
        if (peek()->type() == TokenType::KeywordIf) {
            // else if
            return ParsedIfStatement { .condition = std::move(condition),
                .then_clause = std::move(then_clause),
                .else_clause = std::make_unique<ParsedStatement>(TRY(parse_if_statement())) };
        }
        auto else_clause = TRY(parse_block());
        return ParsedIfStatement { .condition = std::move(condition),
            .then_clause = std::move(then_clause),
            .else_clause = std::make_unique<ParsedStatement>(std::move(*else_clause)) };
    }

    return ParsedIfStatement { .condition = std::move(condition), .then_clause = std::move(then_clause), .else_clause = {} };
}

Util::ParseErrorOr<ParsedForStatement> Parser::parse_for_statement() {
    get(); // for

    TRY(expect(TokenType::ParenOpen));

    TRY(expect(TokenType::KeywordLet));
    auto variable_name = TRY(expect(TokenType::Identifier));
    TRY(expect(TokenType::KeywordOf));
    auto iterable_start = this->range(offset() - 1, 1).end;
    auto iterable = TRY(parse_expression(0));
    auto iterable_end = this->range(offset() - 1, 1).end;
    TRY(expect(TokenType::ParenClose));

    auto block = TRY(parse_block());
    return ParsedForStatement {
        .variable = variable_name.value(),
        .iterable = std::move(iterable),
        .block = std::move(block),
        .iterable_range = { .start = iterable_start, .end = iterable_end }
    };
}

static int precedence(ParsedBinaryExpression::Operator op) {
    switch (op) {
    case ParsedBinaryExpression::Operator::Range:
        return Precedence::Range;
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
    case TokenType::DotDot:
        return ParsedBinaryExpression::Operator::Range;
    case TokenType::Asterisk:
        return ParsedBinaryExpression::Operator::Multiply;
    case TokenType::Slash:
        return ParsedBinaryExpression::Operator::Divide;
    case TokenType::PercentSign:
        return ParsedBinaryExpression::Operator::Modulo;
    case TokenType::EqualEqual:
        return ParsedBinaryExpression::Operator::IsEqual;
    case TokenType::Plus:
        return ParsedBinaryExpression::Operator::Add;
    case TokenType::Minus:
        return ParsedBinaryExpression::Operator::Subtract;
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

Util::ParseErrorOr<ParsedExpression> Parser::parse_primary_or_postfix_expression() {
    auto expr = TRY(parse_primary_expression());

    while (true) {
        if (peek()->type() == TokenType::BraceOpen) {
            auto start = this->offset();
            get();
            auto index = TRY(parse_expression(0));
            TRY(expect(TokenType::BraceClose));
            expr = ParsedExpression {
                .expression = std::make_unique<ParsedArrayIndex>(ParsedArrayIndex {
                    .array = std::move(expr),
                    .index = std::move(index),
                    .range = this->range(start, this->offset() - start),
                }),
            };
        }
        else if (peek()->type() == TokenType::Dot) {
            get();
            auto name = TRY(expect(TokenType::Identifier));
            expr = ParsedExpression {
                .expression = std::make_unique<ParsedMemberAccess>(ParsedMemberAccess {
                    .object = std::move(expr),
                    .member = name.value(),
                }),
            };
        }
        else {
            break;
        }
    }
    return expr;
}

Util::ParseErrorOr<ParsedExpression> Parser::parse_primary_expression() {
    auto token = peek();
    if (token->type() == TokenType::Number) {
        token = get();
        return ParsedExpression { .expression = std::make_unique<ParsedIntegerLiteral>(ParsedIntegerLiteral {
                                      .value = TRY(token->value().parse<int64_t>().map_error(Util::OsToParseError { token->range() })) }) };
    }
    if (token->type() == TokenType::StringLiteral) {
        token = get();
        return ParsedExpression { .expression = std::make_unique<ParsedStringLiteral>(ParsedStringLiteral { .value = Util::UString { token->value() } }) };
    }
    if (token->type() == TokenType::Identifier) {
        token = get();
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
    if (token->type() == TokenType::BraceOpen) {
        return ParsedExpression { .expression = std::make_unique<ParsedInlineArray>(TRY(parse_inline_array())) };
    }
    return expected("expression", *token);
}

Util::ParseErrorOr<ParsedExpression> Parser::parse_expression(int min_precedence) {
    auto lhs = TRY(parse_primary_or_postfix_expression());
    return TRY(parse_operand(std::move(lhs), min_precedence));
}

Util::ParseErrorOr<std::vector<ParsedExpression>> Parser::parse_expression_list(TokenType end_token) {
    std::vector<ParsedExpression> expressions;
    while (true) {
        if (peek()->type() == end_token) {
            break;
        }
        expressions.push_back(TRY(parse_expression(0)));
        if (!next_token_is(TokenType::Comma)) {
            break;
        }
        get();
    }
    return expressions;
}

Util::ParseErrorOr<std::unique_ptr<ParsedCall>> Parser::parse_call_arguments(Util::UString id) {
    get(); // (
    std::vector<ParsedExpression> arguments = TRY(parse_expression_list(TokenType::ParenClose));
    TRY(expect(TokenType::ParenClose));
    return std::make_unique<ParsedCall>(ParsedCall { .name = std::move(id), .arguments = std::move(arguments), .name_range {} });
}

std::string Parser::token_type_to_string(TokenType type) const {
    return fmt::format("'{}'", ESL::token_to_string(type));
}

}
