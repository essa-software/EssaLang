#pragma once

#include "Parser.hpp"
#include <EssaUtil/Config.hpp>
#include <EssaUtil/GenericParser.hpp>
#include <EssaUtil/UString.hpp>
#include <EssaUtil/UStringBuilder.hpp>
#include <compare>
#include <map>

namespace ESL::Typechecker {

struct Type;

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
using StructId = Id<struct StructId_TAG>;
using ScopeId = Id<struct ScopeId_TAG>;
using ModuleId = size_t;

struct CheckedProgram;

struct PrimitiveType {
    enum Primitive {
        Unknown,
        Void,
        U32,
        Bool,
        String,
        Range,
        EmptyArray,
    };
    Primitive type;

    Util::UString name(CheckedProgram const&) const {
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
        case Primitive::EmptyArray:
            return "<empty array>";
        }
        ESSA_UNREACHABLE;
    }

    std::optional<TypeId> iterable_type(CheckedProgram const& program) const;
    bool operator==(PrimitiveType const&) const = default;
};

struct ArrayType {
    TypeId inner;
    size_t size;

    Util::UString name(CheckedProgram const&) const;

    std::optional<TypeId> iterable_type(CheckedProgram const& program) const;
    bool operator==(ArrayType const&) const = default;
};

struct FunctionType {
    Util::UString name(CheckedProgram const&) const;
    std::optional<TypeId> iterable_type(CheckedProgram const&) const { return {}; }
    bool operator==(FunctionType const&) const = default;

    FunctionId function;
};

struct StructType {
    Util::UString name(CheckedProgram const&) const;
    std::optional<TypeId> iterable_type(CheckedProgram const&) const { return {}; }
    bool operator==(StructType const&) const = default;

    StructId id;
};

// Hack to disable default construction of Type because of std::variant
// "inheriting" default ctor of its first alternative (??????)
struct Nc {
    Nc() = delete;
    Util::UString name(CheckedProgram const&) const { return ""; }
    std::optional<TypeId> iterable_type(CheckedProgram const&) const { return {}; }
    bool operator==(Nc const&) const = default;
};

struct Type {
    std::variant<
        Nc,
        ArrayType,
        FunctionType,
        PrimitiveType,
        StructType>
        type;

    Util::UString name(CheckedProgram const& program) const {
        return std::visit([&](auto const& t) { return t.name(program); }, type);
    }
    std::optional<TypeId> iterable_type(CheckedProgram const& program) const {
        return std::visit([&](auto const& t) { return t.iterable_type(program); }, type);
    }

    bool operator==(Type const&) const = default;
};

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
    struct ArrayIndex {
        std::unique_ptr<CheckedExpression> array;
        std::unique_ptr<CheckedExpression> index;
    };
    struct MemberAccess {
        std::unique_ptr<CheckedExpression> object;
        Util::UString member;
    };
    struct UnsignedIntegerLiteral {
        TypeId type_id;
        uint64_t value;
    };
    struct StringLiteral {
        Util::UString value;
    };
    struct BoolLiteral {
        bool value;
    };
    // FIXME: This should not be separate. Make this working with type inference + generics ??
    struct EmptyInlineArray { };
    struct InlineArray {
        TypeId element_type_id;
        std::vector<CheckedExpression> elements;
    };
    QualifiedType type;
    std::variant<Call,
        BinaryExpression,
        ArrayIndex,
        MemberAccess,
        UnsignedIntegerLiteral,
        StringLiteral,
        BoolLiteral,
        EmptyInlineArray,
        InlineArray,
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

struct CheckedBreakOrContinueStatement {
    Parser::ParsedBreakOrContinueStatement::Type type;
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

struct CheckedWhileStatement {
    CheckedExpression condition;
    CheckedBlock block;
};

struct CheckedStatement {
    std::variant<
        CheckedVariableDeclaration,
        CheckedExpression,
        CheckedBreakOrContinueStatement,
        CheckedReturnStatement,
        CheckedIfStatement,
        CheckedForStatement,
        CheckedWhileStatement,
        CheckedBlock>
        statement;
};

template<class T>
using Ref = std::reference_wrapper<T>;

struct CheckedStruct {
    Util::UString name;
    struct Field {
        Util::UString name;
        TypeId type;
        bool operator==(Field const&) const = default;
    };
    std::vector<Field> fields;

    struct Method {
        Util::UString name;
        FunctionId function;
    };
    std::vector<Method> methods;

    TypeId type_id;

    std::optional<Ref<Field const>> find_field(Util::UString const&) const;
    std::optional<Ref<Method const>> find_method(Util::UString const&) const;
};

struct CheckedFunction {
    std::optional<StructId> declaration_scope;
    Util::UString name;
    std::vector<std::pair<Util::UString, CheckedParameter>> parameters;
    std::optional<CheckedBlock> body; // No body = extern function (or just not checked yet)
    TypeId return_type;
    ScopeId argument_scope_id;

    Util::UString mangled_name(CheckedProgram const&) const;
};

struct Module {
    std::string path;
    std::map<Util::UString, TypeId> type_to_id;
    std::map<Util::UString, FunctionId> function_to_id;

    Module(size_t id)
        : m_id(id)
        , m_scopes {}
        , m_global_scope((m_scopes.emplace_back(), m_scopes[0])) { }

    size_t id() const { return m_id; }

    auto const& structs() const { return m_structs; }
    auto const& functions() const { return m_functions; }

    std::unique_ptr<CheckedFunction> const& get_function(size_t id) const {
        assert(id < m_functions.size());
        return m_functions[id];
    }
    std::unique_ptr<CheckedFunction>& get_function(size_t id) {
        assert(id < m_functions.size());
        return m_functions[id];
    }

    Parser::ParsedFunctionDeclaration const& get_parsed_function_declaration(size_t id) const {
        auto it = m_id_to_parsed_function_decl.find(id);
        assert(it != m_id_to_parsed_function_decl.end());
        return *it->second;
    }

    std::unique_ptr<CheckedStruct> const& get_struct(size_t id) const {
        assert(id < m_structs.size());
        return m_structs[id];
    }
    std::unique_ptr<CheckedStruct>& get_struct(size_t id) {
        assert(id < m_structs.size());
        return m_structs[id];
    }

    Type const& get_type(size_t id) const {
        assert(id < m_types.size());
        return m_types[id];
    }
    Type& get_type(size_t id) {
        assert(id < m_types.size());
        return m_types[id];
    }

    Scope const& get_scope(size_t id) const {
        assert(id < m_scopes.size());
        return m_scopes[id];
    }
    Scope& get_scope(size_t id) {
        assert(id < m_scopes.size());
        return m_scopes[id];
    }

    CheckedVariable const& get_variable(size_t id) const {
        assert(id < m_variables.size());
        return m_variables[id];
    }
    CheckedVariable& get_variable(size_t id) {
        assert(id < m_variables.size());
        return m_variables[id];
    }

    FunctionId add_function(CheckedFunction&& checked, Parser::ParsedFunctionDeclaration const& parsed) {
        m_functions.push_back(std::make_unique<CheckedFunction>(std::move(checked)));
        add_parsed_function_declaration(m_functions.size() - 1, parsed);
        return FunctionId { m_id, m_functions.size() - 1 };
    }

    StructId add_struct(CheckedStruct&& s) {
        m_structs.push_back(std::make_unique<CheckedStruct>(std::move(s)));
        // fmt::print("add_function {}:{}\n", m_id, m_functions.size() - 1);
        return StructId { m_id, m_structs.size() - 1 };
    }
    TypeId add_type(Type type) {
        m_types.push_back(std::move(type));
        return TypeId { m_id, m_types.size() - 1 };
    }

    TypeId get_or_add_type(Type type) {
        for (size_t i = 0; i < m_types.size(); ++i) {
            if (m_types[i] == type) {
                return TypeId { m_id, i };
            }
        }
        return add_type(std::move(type));
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

    void add_imported_module(Module& mod) {
        m_imported_modules.push_back(mod);
    }

    auto& imported_modules() const { return m_imported_modules; }

private:
    void add_parsed_function_declaration(size_t id, Parser::ParsedFunctionDeclaration const& decl) {
        m_id_to_parsed_function_decl.insert({ id, &decl });
    }

    size_t m_id {};

    std::vector<std::unique_ptr<CheckedStruct>> m_structs;
    std::vector<std::unique_ptr<CheckedFunction>> m_functions;
    std::vector<Type> m_types;
    std::vector<Scope> m_scopes;
    std::vector<CheckedVariable> m_variables;
    std::vector<Ref<Module>> m_imported_modules;
    std::map<size_t, Parser::ParsedFunctionDeclaration const*> m_id_to_parsed_function_decl;

    Scope& m_global_scope;
};

struct CheckedProgram {
    TypeId unknown_type_id {};
    TypeId void_type_id {};
    TypeId u32_type_id {};
    TypeId bool_type_id {};
    TypeId string_type_id {};
    TypeId range_type_id {};
    TypeId empty_array_type_id {};

    struct Mod {
        ModuleId id;
        Module& module;
    };
    Mod add_module() {
        auto& module = m_modules.emplace_back(std::make_unique<Module>(m_modules.size()));
        return { m_modules.size() - 1, *module };
    }

    Module const& module(ModuleId id) const {
        assert(id < m_modules.size());
        return *m_modules[id];
    }

    Module& module(ModuleId id) {
        assert(id < m_modules.size());
        return *m_modules[id];
    }

    auto const& modules() const { return m_modules; }

    std::unique_ptr<CheckedFunction> const& get_function(FunctionId id) const {
        return module(id.module()).get_function(id.id());
    }
    std::unique_ptr<CheckedFunction>& get_function(FunctionId id) {
        return module(id.module()).get_function(id.id());
    }

    std::unique_ptr<CheckedStruct> const& get_struct(StructId id) const {
        return module(id.module()).get_struct(id.id());
    }
    std::unique_ptr<CheckedStruct>& get_struct(StructId id) {
        return module(id.module()).get_struct(id.id());
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

    Util::UString type_name(TypeId id) const {
        return get_type(id).name(*this);
    }

    Util::UString type_name(QualifiedType const& type) const {
        Util::UStringBuilder builder;
        if (type.is_mut) {
            builder.append("mut ");
        }
        builder.append(type_name(type.type_id));
        return builder.release_string();
    }

    void print() const;

private:
    std::vector<std::unique_ptr<Module>> m_modules;
};

class Typechecker {
public:
    explicit Typechecker(Parser::ParsedFile parsed_file)
        : m_parsed_files()
        , m_entry_point(*m_parsed_files.emplace_back(std::make_unique<Parser::ParsedFile>(std::move(parsed_file)))) { }

    CheckedProgram const& typecheck(std::string root_path);

    struct Error {
        std::string message;
        Util::SourceRange range;
    };

    std::vector<Error> errors() { return m_errors; }

private:
    void error(std::string message, Util::SourceRange range) {
        m_errors.push_back({ std::move(message), range });
    }

    std::optional<Ref<Module>> load_module(Util::UString const& name);
    void typecheck_module(Module&, Parser::ParsedModule const& parsed_module);
    CheckedStruct typecheck_struct(Parser::ParsedStructDeclaration const&);
    void typecheck_struct_methods(StructId, Parser::ParsedStructDeclaration const&);
    // First pass: only signature (return value, args, decl scope)
    CheckedFunction typecheck_function(Parser::ParsedFunctionDeclaration const&, std::optional<StructId> declaration_scope);
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
    TypeId resolve_type(Parser::ParsedType const& type);

    FunctionId add_function(Parser::ParsedFunctionDeclaration const&, std::optional<StructId>);

    std::vector<std::unique_ptr<Parser::ParsedFile>> m_parsed_files;
    std::map<std::filesystem::path, Module*> m_modules_by_path;
    Parser::ParsedFile& m_entry_point;
    CheckedProgram m_program;
    std::vector<Error> m_errors;
    Module* m_current_checked_module = nullptr;
    CheckedFunction* m_current_function = nullptr;
    ScopeId m_current_scope {};
    bool m_is_in_loop = false;
};

}
