#pragma once

#include "Lexer.hpp"
#include <EssaUtil/GenericParser.hpp>
#include <EssaUtil/UString.hpp>
#include <list>
#include <memory>
#include <variant>

namespace ESL::Parser {

struct ParsedType;

struct ParsedArrayType {
    size_t size; // TODO: This should be an expression
    std::unique_ptr<ParsedType> type;
};

struct ParsedUnqualifiedType {
    Util::UString name;

    void print() const;
};

struct ParsedType {
    std::variant<
        ParsedUnqualifiedType,
        ParsedArrayType>
        type;

    Util::SourceRange range;

    Util::UString to_string() const;
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
    std::unique_ptr<ParsedBlock> body; // null for extern functions
    bool has_this_parameter;

    Util::SourceRange name_range;

    void print() const;
};

struct ParsedStructDeclaration {
    struct Field {
        Util::UString name;
        ParsedType type;
    };

    ParsedStructDeclaration(const ParsedStructDeclaration&) = default;
    ParsedStructDeclaration(ParsedStructDeclaration&&) noexcept = default;
    ParsedStructDeclaration& operator=(const ParsedStructDeclaration&) = default;
    ParsedStructDeclaration& operator=(ParsedStructDeclaration&&) noexcept = default;

    // WTF WHY IS THIS NEEDED ?????
    ParsedStructDeclaration(Util::UString _name, std::vector<Field> _fields, std::vector<ParsedFunctionDeclaration> _methods)
        : name(std::move(_name))
        , fields(std::move(_fields))
        , methods(std::move(_methods)) { }

    Util::UString name;
    std::vector<Field> fields;
    std::vector<ParsedFunctionDeclaration> methods;
};

struct ParsedImport {
    Util::UString module;
};

struct ParsedIntegerLiteral {
    int64_t value;

    void print(size_t depth) const;
};
struct ParsedStringLiteral {
    Util::UString value;

    void print(size_t depth) const;
};

struct ParsedBoolLiteral {
    bool value;

    void print(size_t depth) const;
};

struct ParsedIdentifier {
    Util::UString id;

    Util::SourceRange range;

    void print(size_t depth) const;
};

struct ParsedInlineArray;
struct ParsedBinaryExpression;
struct ParsedArrayIndex;
struct ParsedMemberAccess;
struct ParsedCall;

struct ParsedExpression {
    // Workaround for C++ needing class declarations before usage
    std::variant<
        std::unique_ptr<ParsedIntegerLiteral>,
        std::unique_ptr<ParsedStringLiteral>,
        std::unique_ptr<ParsedBoolLiteral>,
        std::unique_ptr<ParsedInlineArray>,
        std::unique_ptr<ParsedIdentifier>,
        std::unique_ptr<ParsedBinaryExpression>,
        std::unique_ptr<ParsedArrayIndex>,
        std::unique_ptr<ParsedMemberAccess>,
        std::unique_ptr<ParsedCall>>
        expression;

    void print(size_t depth) const;
};

struct ParsedInlineArray {
    std::vector<ParsedExpression> elements;
    Util::SourceRange range;

    void print(size_t depth) const;
};

struct ParsedCall {
    ParsedExpression callable;
    std::vector<ParsedExpression> arguments;

    Util::SourceRange callable_range;

    void print(size_t depth) const;
};

struct ParsedBinaryExpression {
    enum class Operator {
        Range,
        Multiply,
        Divide,
        Modulo,
        Add,
        Subtract,
        IsEqual,
        IsNotEqual,
        IsLess,
        IsLessEq,
        IsGreater,
        IsGreaterEq,
        LogicalAnd,
        LogicalOr,
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
        return operator_ == Operator::IsEqual
            || operator_ == Operator::IsNotEqual
            || operator_ == Operator::IsLess
            || operator_ == Operator::IsLessEq
            || operator_ == Operator::IsGreater
            || operator_ == Operator::IsGreaterEq;
    }

    bool is_logical() const {
        return operator_ == Operator::LogicalAnd
            || operator_ == Operator::LogicalOr;
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

struct ParsedArrayIndex {
    ParsedExpression array;
    ParsedExpression index;

    Util::SourceRange range;

    void print(size_t depth) const;
};

struct ParsedMemberAccess {
    ParsedExpression object;
    Util::UString member;

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

struct ParsedIfStatement;
struct ParsedForStatement;
struct ParsedWhileStatement;

struct ParsedBreakOrContinueStatement {
    enum class Type {
        Break,
        Continue
    } type;
    Util::SourceRange range;

    void print(size_t depth) const;
};

using ParsedStatement = std::variant<
    ParsedVariableDeclaration,
    ParsedReturnStatement,
    ParsedBreakOrContinueStatement,
    ParsedExpression,
    ParsedIfStatement,
    ParsedForStatement,
    ParsedWhileStatement,
    ParsedBlock>;

struct ParsedIfStatement {
    ParsedExpression condition;
    std::unique_ptr<ParsedBlock> then_clause;
    std::unique_ptr<ParsedStatement> else_clause;

    void print(size_t depth) const;
};

struct ParsedForStatement {
    Util::UString variable;
    ParsedExpression iterable;
    std::unique_ptr<ParsedBlock> block;

    Util::SourceRange iterable_range;

    void print(size_t depth) const;
};

struct ParsedWhileStatement {
    ParsedExpression condition;
    std::unique_ptr<ParsedBlock> block;

    void print(size_t depth) const;
};

struct ParsedBlock {
    std::vector<ParsedStatement> statements;

    void print(size_t depth) const;
};

struct ParsedModule {
    std::vector<ParsedFunctionDeclaration> function_declarations;
    std::vector<ParsedStructDeclaration> struct_declarations;
    std::vector<ParsedImport> imports;

    void print() const;
};

struct ParsedFile {
    ParsedModule module;

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
    Util::ParseErrorOr<ParsedArrayType> parse_array_type();
    Util::ParseErrorOr<ParsedInlineArray> parse_inline_array();
    Util::ParseErrorOr<ParsedType> parse_type();
    Util::ParseErrorOr<ParsedFunctionDeclaration> parse_function_declaration();
    Util::ParseErrorOr<ParsedStructDeclaration> parse_struct_declaration();
    Util::ParseErrorOr<ParsedImport> parse_import();
    Util::ParseErrorOr<ParsedVariableDeclaration> parse_variable_declaration();
    Util::ParseErrorOr<ParsedReturnStatement> parse_return_statement();
    Util::ParseErrorOr<ParsedIfStatement> parse_if_statement();
    Util::ParseErrorOr<ParsedForStatement> parse_for_statement();
    Util::ParseErrorOr<ParsedWhileStatement> parse_while_statement();
    Util::ParseErrorOr<ParsedExpression> parse_expression(int min_precedence);
    Util::ParseErrorOr<ParsedExpression> parse_primary_or_postfix_expression();
    Util::ParseErrorOr<ParsedExpression> parse_primary_expression();
    Util::ParseErrorOr<ParsedExpression> parse_operand(ParsedExpression lhs, int min_precedence);
    Util::ParseErrorOr<std::vector<ParsedExpression>> parse_expression_list(TokenType end_token);

    virtual std::string token_type_to_string(TokenType type) const override;
};

}
