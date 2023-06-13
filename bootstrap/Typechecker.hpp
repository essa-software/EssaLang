#pragma once

#include "Parser.hpp"
#include <EssaUtil/Config.hpp>
#include <EssaUtil/GenericParser.hpp>
#include <EssaUtil/UString.hpp>
#include <EssaUtil/UStringBuilder.hpp>
#include <compare>
#include <map>

namespace ESL::Typechecker {

struct Type {
    enum class Primitive {
        Unknown,
        Void,
        U32,
        Bool,
        String,
        Range
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
        case Primitive::Bool:
            return "bool";
        case Primitive::String:
            return "string";
        case Primitive::Range:
            return "range";
        }
        ESSA_UNREACHABLE;
    }

    bool is_iterable() const { return type == Primitive::Range; }

    Util::UString iterable_value_type() const {
        switch (type) {
        case Primitive::Range:
            return "u32";
        default:
            return "unknown";
        }
    }
};

template<class TAG>
struct Id {
public:
    explicit Id() = default;

    explicit Id(size_t module, size_t id)
        : m_module(module)
        , m_id(id) { }

    auto module() const { return m_module; }
    auto id() const { return m_id; }

    bool operator==(Id const& other) const = default;
    std::strong_ordering operator<=>(Id const& other) const = default;

private:
    size_t m_module = 0;
    size_t m_id = 0;
};

using TypeId = Id<struct TypeId_TAG>;
using VarId = Id<struct VarId_TAG>;
using FunctionId = Id<struct FunctionId_TAG>;
using ScopeId = Id<struct ScopeId_TAG>;

// Type with all its qualifiers
struct QualifiedType {
    TypeId type_id;
    bool is_mut = false;
};

struct ResolvedIdentifier {
    enum class Type {
        Variable,
        Function,
        Invalid
    };
    Type type = Type::Invalid;
    size_t module;
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
    QualifiedType type;
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
    QualifiedType type;
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

struct CheckedStatement;

struct CheckedBlock {
    ScopeId scope_id {};
    std::vector<CheckedStatement> statements;
};

struct CheckedIfStatement {
    CheckedExpression condition;
    CheckedBlock then_clause;
    std::unique_ptr<CheckedStatement> else_clause;
};

struct CheckedForStatement {
    Util::UString variable_name;
    CheckedExpression iterable;
    TypeId value_type_id;
    CheckedBlock block;
};

struct CheckedStatement {
    std::variant<
        CheckedVariableDeclaration,
        CheckedExpression,
        CheckedReturnStatement,
        CheckedIfStatement,
        CheckedForStatement,
        CheckedBlock>
        statement;
};

struct CheckedFunction {
    Util::UString name;
    std::vector<std::pair<Util::UString, CheckedParameter>> parameters;
    std::optional<CheckedBlock> body; // No body = extern function (or just not checked yet)
    TypeId return_type;
    ScopeId argument_scope_id;
};

struct Module {
    std::map<Util::UString, FunctionId> function_to_id;

    Module(size_t id)
        : m_id(id)
        , m_scopes {}
        , m_global_scope((m_scopes.emplace_back(), m_scopes[0])) { }

    auto const& functions() const { return m_functions; }

    std::unique_ptr<CheckedFunction> const& get_function(size_t id) const {
        return m_functions[id];
    }
    std::unique_ptr<CheckedFunction>& get_function(size_t id) {
        return m_functions[id];
    }

    Type const& get_type(size_t id) const {
        return m_types[id];
    }
    Type& get_type(size_t id) {
        return m_types[id];
    }

    Scope const& get_scope(size_t id) const {
        return m_scopes[id];
    }
    Scope& get_scope(size_t id) {
        return m_scopes[id];
    }

    CheckedVariable const& get_variable(size_t id) const {
        return m_variables[id];
    }
    CheckedVariable& get_variable(size_t id) {
        return m_variables[id];
    }

    FunctionId add_function() {
        m_functions.push_back({});
        // fmt::print("add_function {}:{}\n", m_id, m_functions.size() - 1);
        return FunctionId { m_id, m_functions.size() - 1 };
    }

    TypeId add_type(Type type) {
        m_types.push_back(std::move(type));
        return TypeId { m_id, m_types.size() - 1 };
    }

    ScopeId add_scope(ScopeId parent) {
        m_scopes.emplace_back(Scope { .parent = parent });
        return ScopeId { m_id, m_scopes.size() - 1 };
    }

    VarId add_variable(CheckedVariable var) {
        m_variables.push_back(std::move(var));
        // fmt::print("Add variable: {}.{}\n", m_id, m_variables.size() - 1);
        return VarId { m_id, m_variables.size() - 1 };
    }

private:
    size_t m_id {};

    std::vector<std::unique_ptr<CheckedFunction>> m_functions;
    std::vector<Type> m_types;
    std::vector<Scope> m_scopes;
    std::vector<CheckedVariable> m_variables;

    Scope& m_global_scope;
};

struct CheckedProgram {
    TypeId unknown_type_id {};
    TypeId void_type_id {};
    TypeId u32_type_id {};
    TypeId bool_type_id {};
    TypeId string_type_id {};
    TypeId range_type_id {};

    CheckedProgram()
        : m_prelude_module(0)
        , m_root_module(1) {
        unknown_type_id = { m_prelude_module.add_type(Type { .type = Type::Primitive::Unknown }) };
        void_type_id = { m_prelude_module.add_type(Type { .type = Type::Primitive::Void }) };
        u32_type_id = { m_prelude_module.add_type(Type { .type = Type::Primitive::U32 }) };
        bool_type_id = { m_prelude_module.add_type(Type { .type = Type::Primitive::Bool }) };
        string_type_id = { m_prelude_module.add_type(Type { .type = Type::Primitive::String }) };
        range_type_id = { m_prelude_module.add_type(Type { .type = Type::Primitive::Range }) };
    }

    Module const& module(size_t id) const {
        if (id == 0) {
            return m_prelude_module;
        }
        if (id == 1) {
            return m_root_module;
        }
        ESSA_UNREACHABLE;
    }

    Module& module(size_t id) {
        if (id == 0) {
            return m_prelude_module;
        }
        if (id == 1) {
            return m_root_module;
        }
        ESSA_UNREACHABLE;
    }

    std::unique_ptr<CheckedFunction> const& get_function(FunctionId id) const {
        return module(id.module()).get_function(id.id());
    }
    std::unique_ptr<CheckedFunction>& get_function(FunctionId id) {
        return module(id.module()).get_function(id.id());
    }

    Type const& get_type(TypeId id) const {
        return module(id.module()).get_type(id.id());
    }
    Type& get_type(TypeId id) {
        return module(id.module()).get_type(id.id());
    }

    Scope const& get_scope(ScopeId id) const {
        return module(id.module()).get_scope(id.id());
    }
    Scope& get_scope(ScopeId id) {
        return module(id.module()).get_scope(id.id());
    }

    CheckedVariable const& get_variable(VarId id) const {
        return module(id.module()).get_variable(id.id());
    }
    CheckedVariable& get_variable(VarId id) {
        return module(id.module()).get_variable(id.id());
    }

    TypeId resolve_type(Util::UString const& name) {
        if (name == "u32") {
            return u32_type_id;
        }
        if (name == "string") {
            return string_type_id;
        }
        if (name == "void") {
            return void_type_id;
        }
        return unknown_type_id;
    }

    Util::UString type_name(TypeId id) {
        return get_type(id).name();
    }

    Util::UString type_name(QualifiedType const& type) {
        Util::UStringBuilder builder;
        if (type.is_mut) {
            builder.append("mut ");
        }
        builder.append(type_name(type.type_id));
        return builder.release_string();
    }

    void print() const;

private:
    Module m_prelude_module;
    Module m_root_module;
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

    // First pass: only signature (return value & args)
    CheckedFunction typecheck_function(Parser::ParsedFunctionDeclaration const&);
    // Second pass: body
    void typecheck_function_body(CheckedFunction&, Parser::ParsedFunctionDeclaration const&);
    CheckedStatement typecheck_statement(Parser::ParsedStatement const&);
    CheckedBlock typecheck_block(Parser::ParsedBlock const&);
    CheckedParameter typecheck_parameter(Parser::ParsedParameter const&);
    CheckedExpression typecheck_expression(Parser::ParsedExpression const&);
    CheckedExpression typecheck_binary_expression(Parser::ParsedBinaryExpression const&);

    enum class TypeCompatibility {
        Assignment,
        Comparison
    };
    bool check_type_compatibility(TypeCompatibility mode, TypeId lhs, TypeId rhs, std::optional<Util::SourceRange> range);
    ResolvedIdentifier resolve_identifier(Util::UString const& id, Util::SourceRange);

    CheckedFunction const& get_function(FunctionId id);

    Parser::ParsedFile m_parsed_file;
    CheckedProgram m_program;
    std::vector<Error> m_errors;
    Module* m_current_checked_module = nullptr;
    CheckedFunction* m_current_function = nullptr;
    ScopeId m_current_scope {};
};

}
