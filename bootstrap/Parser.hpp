#pragma once

#include "Lexer.hpp"
#include <EssaUtil/GenericParser.hpp>
#include <EssaUtil/UString.hpp>
#include <memory>
#include <variant>

namespace ESL::Parser {

struct ParsedType {
    enum class Primitive {
        U32
    };

    Primitive primitive;

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

    void print(size_t depth) const;
};
struct ParsedExpression;
struct ParsedCall {
    Util::UString name; // FIXME: This should be an expression
    std::vector<ParsedExpression> arguments;

    void print(size_t depth) const;
};

struct ParsedExpression {
    // Workaround for C++ needing class declarations before usage
    std::variant<
        ParsedIntegerLiteral,
        ParsedStringLiteral,
        ParsedIdentifier,
        ParsedCall>
        expression;

    void print(size_t depth) const;
};

struct ParsedVariableDeclaration {
    bool is_mut;
    Util::UString name;
    std::optional<ParsedType> type;
    ParsedExpression initializer;

    void print(size_t depth) const;
};

struct ParsedReturnStatement {
    std::optional<ParsedExpression> value;

    void print(size_t depth) const;
};

using ParsedStatement = std::variant<
    ParsedVariableDeclaration,
    ParsedReturnStatement,
    ParsedExpression>;

struct ParsedBlock {
    std::vector<ParsedStatement> statements;

    void print(size_t depth) const;
};

struct ParsedFile {
    std::vector<ParsedFunctionDeclaration> function_declarations;

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
    Util::ParseErrorOr<ParsedExpression> parse_expression();
    Util::ParseErrorOr<ParsedCall> parse_call_arguments(Util::UString id);
};

template<typename... Deps>
inline constexpr bool dependent_false = false;

}
