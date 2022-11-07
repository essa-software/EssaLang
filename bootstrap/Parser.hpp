#pragma once

#include "Lexer.hpp"
#include <EssaUtil/GenericParser.hpp>
#include <EssaUtil/UString.hpp>
#include <memory>
#include <variant>

namespace ESL::Parser {

struct ParsedType {
    Util::UString name;

    void print() const;
};

struct ParsedBlock;

struct ParsedParameter {
    ParsedType type;
    Util::UString name;

    void print() const;
};

struct ParsedFunctionDeclaration {
    Util::UString name;
    std::optional<ParsedType> return_type;
    std::vector<ParsedParameter> parameters;
    std::unique_ptr<ParsedBlock> body;

    Util::SourceRange name_range;

    void print() const;
};

struct ParsedIntegerLiteral {
    int64_t value;

    void print(size_t depth) const;
};
struct ParsedStringLiteral {
    Util::UString value;

    void print(size_t depth) const;
};
struct ParsedIdentifier {
    Util::UString id;

    Util::SourceRange range;

    void print(size_t depth) const;
};

struct ParsedIntegerLiteral;
struct ParsedStringLiteral;
struct ParsedIdentifier;
struct ParsedBinaryExpression;
struct ParsedCall;

struct ParsedExpression {
    // Workaround for C++ needing class declarations before usage
    std::variant<
        std::unique_ptr<ParsedIntegerLiteral>,
        std::unique_ptr<ParsedStringLiteral>,
        std::unique_ptr<ParsedIdentifier>,
        std::unique_ptr<ParsedBinaryExpression>,
        std::unique_ptr<ParsedCall>>
        expression;

    void print(size_t depth) const;
};

struct ParsedCall {
    Util::UString name; // FIXME: This should be an expression
    std::vector<ParsedExpression> arguments;

    Util::SourceRange name_range;

    void print(size_t depth) const;
};

struct ParsedBinaryExpression {
    enum class Operator {
        Add,
        Subtract,
        Multiply,
        Divide,
        Modulo,
        IsEqual,
        Assign,
        AssignAdd,
        AssignSubtract,
        AssignMultiply,
        AssignDivide,
        AssignModulo,
        Invalid
    };

    static std::string operator_to_string(Operator);

    ParsedExpression lhs;
    Operator operator_;
    ParsedExpression rhs;

    Util::SourceRange operator_range;

    bool is_comparison() const {
        return operator_ == Operator::IsEqual;
    }

    bool is_assignment() const {
        return operator_ == Operator::Assign
            || operator_ == Operator::AssignAdd
            || operator_ == Operator::AssignSubtract
            || operator_ == Operator::AssignMultiply
            || operator_ == Operator::AssignDivide
            || operator_ == Operator::AssignModulo;
    }

    void print(size_t depth) const;
};

struct ParsedVariableDeclaration {
    bool is_mut;
    Util::UString name;
    std::optional<ParsedType> type;
    ParsedExpression initializer;

    Util::SourceRange range;

    void print(size_t depth) const;
};

struct ParsedReturnStatement {
    std::optional<ParsedExpression> value;

    Util::SourceRange range;

    void print(size_t depth) const;
};

struct ParsedIfStatement {
    ParsedExpression condition;
    std::unique_ptr<ParsedBlock> then_clause;

    void print(size_t depth) const;
};

using ParsedStatement = std::variant<
    ParsedVariableDeclaration,
    ParsedReturnStatement,
    ParsedExpression,
    ParsedIfStatement>;

struct ParsedBlock {
    std::vector<ParsedStatement> statements;

    void print(size_t depth) const;
};

struct ParsedModule {
    std::vector<ParsedFunctionDeclaration> function_declarations;

    void print() const;
};

struct ParsedFile {
    std::vector<ParsedModule> modules;

    void print() const;
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
    Util::ParseErrorOr<ParsedIfStatement> parse_if_statement();
    Util::ParseErrorOr<ParsedExpression> parse_expression(int min_precedence);
    Util::ParseErrorOr<ParsedExpression> parse_primary_expression();
    Util::ParseErrorOr<ParsedExpression> parse_operand(ParsedExpression lhs, int min_precedence);
    Util::ParseErrorOr<std::unique_ptr<ParsedCall>> parse_call_arguments(Util::UString id);
};

}
