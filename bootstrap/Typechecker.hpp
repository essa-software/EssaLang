#pragma once

#include "Parser.hpp"
#include <EssaUtil/GenericParser.hpp>
#include <EssaUtil/UString.hpp>
#include <compare>
#include <map>

namespace ESL::Typechecker {

struct Type {
    enum class Primitive {
        Unknown,
        Void,
        U32,
        String
    };
    Primitive type;

    Util::UString name() const {
        switch (type) {
        case Primitive::Unknown:
            return "unknown";
        case Primitive::Void:
            return "void";
        case Primitive::U32:
            return "u32";
        case Primitive::String:
            return "string";
        }
        ESSA_UNREACHABLE;
    }
};

template<class TAG>
struct Id {
public:
    explicit Id(size_t id)
        : m_id(id) { }

    auto id() const { return m_id; }

    bool operator==(Id const& other) const = default;
    std::strong_ordering operator<=>(Id const& other) const = default;

private:
    size_t m_id = 0;
};

using TypeId = Id<struct TypeId_TAG>;
using VarId = Id<struct VarId_TAG>;
using FunctionId = Id<struct FunctionId_TAG>;
using ScopeId = Id<struct ScopeId_TAG>;

struct ResolvedIdentifier {
    enum class Type {
        Variable,
        Function,
        Invalid
    };
    Type type;
    size_t id;
};

struct CheckedProgram;

struct CheckedExpression {
    struct Call {
        FunctionId function_id;
        std::vector<CheckedExpression> arguments;
    };
    struct BinaryExpression {
        std::unique_ptr<CheckedExpression> lhs;
        Parser::ParsedBinaryExpression::Operator operator_;
        std::unique_ptr<CheckedExpression> rhs;
    };
    struct UnsignedIntegerLiteral {
        TypeId type_id;
        uint64_t value;
    };
    struct StringLiteral {
        Util::UString value;
    };
    TypeId type_id;
    std::variant<Call,
        BinaryExpression,
        UnsignedIntegerLiteral,
        StringLiteral,
        ResolvedIdentifier,
        std::monostate>
        expression;

    static CheckedExpression invalid(CheckedProgram const& program);
};

struct CheckedVariable {
    Util::UString name;
    TypeId type_id;
    bool is_mut = false;
    std::optional<CheckedExpression> initializer;

    void print(CheckedProgram const& program) const;
};

struct Scope {
    std::optional<ScopeId> parent;
    std::map<Util::UString, VarId> variables {};
};

struct CheckedVariableDeclaration {
    VarId var_id;
};

struct CheckedParameter {
    VarId var_id;
};

struct CheckedReturnStatement {
    std::optional<CheckedExpression> expression;
};

struct CheckedStatement {
    std::variant<
        CheckedVariableDeclaration,
        CheckedExpression,
        CheckedReturnStatement>
        statement;
};

struct CheckedBlock {
    ScopeId scope_id { 0 };
    std::vector<CheckedStatement> statements {};
};

struct CheckedFunction {
    Util::UString name;
    std::vector<std::pair<Util::UString, CheckedParameter>> parameters;
    CheckedBlock body;
    TypeId return_type;
};

struct CheckedProgram {
    std::map<Util::UString, FunctionId> function_to_id;

    TypeId unknown_type_id { 0 };
    TypeId void_type_id { 0 };
    TypeId u32_type_id { 0 };
    TypeId string_type_id { 0 };

    CheckedProgram()
        : m_scopes {}
        , m_global_scope((m_scopes.emplace_back(), m_scopes[0])) {
        unknown_type_id = { add_type(Type { .type = Type::Primitive::Unknown }) };
        void_type_id = { add_type(Type { .type = Type::Primitive::Void }) };
        u32_type_id = { add_type(Type { .type = Type::Primitive::U32 }) };
        string_type_id = { add_type(Type { .type = Type::Primitive::String }) };
    }

    std::unique_ptr<CheckedFunction> const& get_function(FunctionId id) const {
        return m_functions[id.id()];
    }
    std::unique_ptr<CheckedFunction>& get_function(FunctionId id) {
        return m_functions[id.id()];
    }
    auto const& functions() const { return m_functions; }

    Type const& get_type(TypeId id) const {
        return m_types[id.id()];
    }
    Type& get_type(TypeId id) {
        return m_types[id.id()];
    }

    Scope const& get_scope(ScopeId id) const {
        return m_scopes[id.id()];
    }
    Scope& get_scope(ScopeId id) {
        return m_scopes[id.id()];
    }

    CheckedVariable const& get_variable(VarId id) const {
        return m_variables[id.id()];
    }
    CheckedVariable& get_variable(VarId id) {
        return m_variables[id.id()];
    }

    TypeId resolve_type(Util::UString const& name) {
        if (name == "u32") {
            return u32_type_id;
        }
        if (name == "string") {
            return string_type_id;
        }
        return unknown_type_id;
    }

    FunctionId add_function() {
        m_functions.push_back({});
        return FunctionId { m_functions.size() - 1 };
    }

    TypeId add_type(Type type) {
        m_types.push_back(std::move(type));
        return TypeId { m_types.size() - 1 };
    }

    ScopeId add_scope(ScopeId parent) {
        m_scopes.emplace_back(Scope { .parent = parent });
        return ScopeId { m_scopes.size() - 1 };
    }

    VarId add_variable(CheckedVariable var) {
        m_variables.push_back(std::move(var));
        return VarId { m_variables.size() - 1 };
    }

    Util::UString type_name(TypeId id) {
        return get_type(id).name();
    }

    void print() const;

private:
    std::vector<std::unique_ptr<CheckedFunction>> m_functions;
    std::vector<Type> m_types;
    std::vector<Scope> m_scopes;
    std::vector<CheckedVariable> m_variables;

    Scope& m_global_scope;
};

class Typechecker {
public:
    Typechecker(Parser::ParsedFile parsed_file)
        : m_parsed_file(std::move(parsed_file)) { }

    CheckedProgram const& typecheck();

    struct Error {
        std::string message;
        Util::SourceRange range;
    };

    std::vector<Error> errors() { return m_errors; }

private:
    void error(std::string message, Util::SourceRange range) {
        m_errors.push_back({ std::move(message), range });
    }

    CheckedFunction typecheck_function(Parser::ParsedFunctionDeclaration const&);
    CheckedBlock typecheck_block(Parser::ParsedBlock const&);
    CheckedParameter typecheck_parameter(Parser::ParsedParameter const&);
    CheckedExpression typecheck_expression(Parser::ParsedExpression const&);
    CheckedExpression typecheck_binary_expression(Parser::ParsedBinaryExpression const&);

    enum class TypeCompatibility {
        Assignment
    };
    bool check_type_compatibility(TypeCompatibility mode, TypeId lhs, TypeId rhs, Util::SourceRange range);
    ResolvedIdentifier resolve_identifier(Util::UString const& id, Util::SourceRange);

    CheckedFunction const& get_function(FunctionId id);

    Parser::ParsedFile m_parsed_file;
    CheckedProgram m_program;
    std::vector<Error> m_errors;
    CheckedFunction* m_current_function = nullptr;
    ScopeId m_current_scope { 0 };
};

}
