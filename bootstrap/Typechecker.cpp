#include "Typechecker.hpp"

#include "Esl.hpp"
#include "Parser.hpp"
#include <EssaUtil/Config.hpp>
#include <EssaUtil/ScopeGuard.hpp>
#include <EssaUtil/TemporaryChange.hpp>
#include <cstdlib>
#include <filesystem>
#include <fmt/format.h>
#include <variant>

namespace ESL::Typechecker {

Util::UString SliceType::name(CheckedProgram const& program) const {
    return Util::UString::format("[]{}{}", elements_are_mut ? "mut " : "", program.get_type(inner).name(program).encode());
}

Util::UString ArrayType::name(CheckedProgram const& program) const {
    return Util::UString::format("[{}]{}", size, program.get_type(inner).name(program).encode());
}

Util::UString FunctionType::name(CheckedProgram const& program) const {
    return Util::UString::format("<func {}>", program.get_function(function)->name.encode());
}

Util::UString StructType::name(CheckedProgram const& program) const {
    return program.get_struct(id)->name;
}

std::optional<TypeId> PrimitiveType::iterable_type(CheckedProgram const& program) const {
    return type == PrimitiveType::Range || type == PrimitiveType::String ? std::optional(program.u32_type_id) : std::nullopt;
}

std::optional<TypeId> SliceType::iterable_type(CheckedProgram const&) const {
    return inner;
}

std::optional<TypeId> ArrayType::iterable_type(CheckedProgram const&) const {
    return inner;
}

std::optional<Ref<CheckedStruct::Field const>> CheckedStruct::find_field(Util::UString const& name) const {
    // TODO: Optimize it
    for (auto const& field : fields) {
        if (field.name == name) {
            return field;
        }
    }
    return std::nullopt;
}

std::optional<Ref<CheckedStruct::Method const>> CheckedStruct::find_method(Util::UString const& name) const {
    // TODO: Optimize it
    for (auto const& method : methods) {
        if (method.name == name) {
            return method;
        }
    }
    return std::nullopt;
}

Util::UString CheckedFunction::mangled_name(CheckedProgram const& program) const {
    if (!declaration_scope) {
        return name;
    }
    return Util::UString::format("___{}_{}", program.get_struct(*declaration_scope)->name.encode(), name.encode());
}

std::optional<Ref<Module>> Typechecker::load_module(Util::UString const& name) {
    // fmt::print("load_module({})\n", name.encode());

    namespace fs = std::filesystem;

    auto local_path = fs::absolute((fs::path(m_current_checked_module->path).parent_path() / name.encode())).lexically_normal() += ".esl";
    // fmt::print("{} exists?? {}\n", local_path.string(), fs::exists(local_path));
    auto builtin_path = fs::absolute((fs::path(ESL_SOURCE_DIR) / "lib" / name.encode())).lexically_normal() += ".esl";
    // fmt::print("{} exists?? {}\n", builtin_path.string(), fs::exists(builtin_path));

    auto path = fs::exists(local_path) ? local_path : builtin_path;
    if (!fs::exists(path)) {
        // FIXME: range
        error(fmt::format("Module '{}' not found", name.encode()), {});
        return {};
    }

    if (auto it = m_modules_by_path.find(path); it != m_modules_by_path.end()) {
        return *it->second;
    }

    auto& mod = m_program.add_module().module;
    m_modules_by_path.insert({ path, &mod });
    mod.path = path;

    bool is_prelude = name == "prelude";
    if (is_prelude) {
        // fmt::print("SETTING UP PRELUDE. THIS SHOULD BE RUN ONLY ONCE.\n");
        auto create_primitive = [&](PrimitiveType::Primitive primitive) -> TypeId {
            return { mod.add_type(Type { .type = PrimitiveType { .type = primitive } }) };
        };
        m_program.unknown_type_id = create_primitive(PrimitiveType::Unknown);
        m_program.void_type_id = create_primitive(PrimitiveType::Void);
        m_program.u32_type_id = create_primitive(PrimitiveType::U32);
        m_program.bool_type_id = create_primitive(PrimitiveType::Bool);
        m_program.string_type_id = create_primitive(PrimitiveType::String);
        m_program.range_type_id = create_primitive(PrimitiveType::Range);
        // FIXME: This is a hack to allow default-initialization using empty array syntax.
        m_program.empty_array_type_id = create_primitive(PrimitiveType::EmptyArray);
    }

    // fmt::print("try load mod from {}\n", mod.path);
    auto maybe_parsed_file = parse_file(mod.path);
    if (maybe_parsed_file.is_error_of_type<Util::ParseError>()) {
        auto error = maybe_parsed_file.release_error_of_type<Util::ParseError>();
        // fmt::print(stderr, "module loaded from {} err at {}\n", path.string(), error.location.start.offset);
        m_errors.push_back(Error {
            .message = error.message,
            .range = error.location,
            .file_name = mod.path,
        });
        return {};
    }
    else if (maybe_parsed_file.is_error_of_type<Util::OsError>()) {
        auto error = maybe_parsed_file.release_error_of_type<Util::OsError>();
        fmt::print(stderr, "Failed to load module (from file {}): {}\n", path.string(), error);
        return {};
    }
    auto& parsed_file = *m_parsed_files.emplace_back(std::make_unique<Parser::ParsedFile>(maybe_parsed_file.release_value()));
    if (!is_prelude) {
        auto prelude = load_module("prelude");
        if (!prelude) {
            fmt::print(stderr, "Panic: failed to load prelude!!!! Errors encountered:\n");
            for (auto const& error : m_errors) {
                fmt::print(stderr, "- {}\n", error.message);
            }
            abort();
        }
        mod.add_imported_module(prelude->get());
    }
    // FIXME: This will stack overflow on circular imports
    typecheck_module(mod, parsed_file.module);
    // fmt::print("YAY loaded and typechecked {}\n", mod.path);
    return mod;
}

void Typechecker::typecheck_module(Module& module, Parser::ParsedModule const& parsed_module) {
    auto old_checked_module = m_current_checked_module;
    m_current_checked_module = &module;
    Util::ScopeGuard guard = [&]() {
        m_current_checked_module = old_checked_module;
    };

    // 1. Bring in imported stuff
    for (auto const& import : parsed_module.imports) {
        auto mod = load_module(import.module);
        if (!mod) {
            continue;
        }
        module.add_imported_module(mod->get());
    }

    // 2. Typecheck structs
    for (auto& struct_ : parsed_module.struct_declarations) {
        auto struct_id = module.add_struct(typecheck_struct(struct_));
        auto type_id = module.add_type(Type { .type = StructType { .id = struct_id } });
        module.type_to_id.insert({ struct_.name, type_id });
        typecheck_struct_methods(struct_id, struct_);
    }

    // 3. Typecheck global function signatures
    for (auto const& func : parsed_module.function_declarations) {
        auto func_id = add_function(func, {});
        module.function_to_id.insert({ func.name, func_id });
    }

    // 4. Typecheck function bodies
    {
        size_t id = 0;
        for (auto& func : m_current_checked_module->functions()) {
            typecheck_function_body(*func, module.get_parsed_function_declaration(id));
            id++;
        }
    }
}

CheckedProgram const& Typechecker::typecheck(std::string root_path) {
    // This balances the parsed file added in constructor.
    auto& root_module = m_program.add_module().module;
    root_module.path = std::move(root_path);

    m_current_checked_module = &root_module;
    auto prelude = load_module("prelude");
    if (!prelude) {
        fmt::print(stderr, "Panic: failed to load prelude from root module!!!! Errors encountered:\n");
        for (auto const& error : m_errors) {
            fmt::print(stderr, "- {}\n", error.message);
        }
        abort();
    }
    root_module.add_imported_module(prelude->get());

    typecheck_module(root_module, m_entry_point.module);
    return m_program;
}

#define SCOPED_SCOPE(scope)                                      \
    auto __old_scope = m_current_scope;                          \
    m_current_scope = (scope);                                   \
    Util::ScopeGuard scope_guard {                               \
        [__old_scope, this]() { m_current_scope = __old_scope; } \
    }

CheckedStruct Typechecker::typecheck_struct(Parser::ParsedStructDeclaration const& struct_) {
    std::vector<CheckedStruct::Field> fields;

    for (auto const& field : struct_.fields) {
        auto type_id = resolve_type(field.type);
        if (type_id == m_program.unknown_type_id) {
            error(fmt::format("Invalid type '{}' in field declaration", field.type.to_string().encode()), field.type.range);
        }
        fields.push_back(CheckedStruct::Field {
            .name = field.name,
            .type = type_id,
        });
    }

    return CheckedStruct {
        .name = struct_.name,
        .fields = std::move(fields),
        .methods = {},
        .type_id = TypeId(),
    };
}

void Typechecker::typecheck_struct_methods(StructId struct_id, Parser::ParsedStructDeclaration const& parsed_struct) {
    auto& struct_ = *m_program.get_struct(struct_id);
    for (auto const& method : parsed_struct.methods) {
        auto function_id = add_function(method, struct_id);
        struct_.methods.push_back(CheckedStruct::Method {
            .name = method.name,
            .function = function_id,
        });
    }
}

CheckedFunction Typechecker::typecheck_function(Parser::ParsedFunctionDeclaration const& function, std::optional<StructId> declaration_scope) {
    TypeId return_type = [&function, this]() {
        TypeId type_id { m_program.unknown_type_id };
        if (function.return_type) {
            type_id = resolve_type(*function.return_type);
            if (function.name == "main") {
                // TODO: Change it to i32.
                if (type_id != m_program.u32_type_id) {
                    error("main() function must return u32", function.name_range);
                }
            }
        }
        else {
            if (function.name == "main") {
                return m_program.u32_type_id;
            }
            else {
                // TODO: Infer type from return statements if possible
                // error("Function must have a return type", { function.name_range });
                return m_program.void_type_id;
            }
        }
        return type_id;
    }();

    CheckedFunction checked_function {
        .declaration_scope = declaration_scope,
        .name = function.name,
        .parameters = {},
        .body = {},
        .return_type = return_type,
        .argument_scope_id = m_current_checked_module->add_scope(ScopeId {}),
    };
    m_current_function = &checked_function;
    Util::ScopeGuard guard = [this]() {
        m_current_function = nullptr;
    };
    SCOPED_SCOPE(checked_function.argument_scope_id);

    if (function.has_this_parameter) {
        if (!declaration_scope) {
            error("'this' may be a parameter only for methods", function.this_param_range);
        }
        else {
            auto type_id = m_program.get_struct(*declaration_scope)->type_id;
            auto var_id = m_current_checked_module->add_variable(CheckedVariable {
                .name = "this",
                .type = { .type_id = type_id, .is_mut = false },
                .initializer = {},
            });
            m_program.get_scope(checked_function.argument_scope_id).variables.insert({ "this", var_id });
            checked_function.parameters.push_back({ "this", CheckedParameter { .var_id = var_id } });
        }
    }

    for (auto const& param : function.parameters) {
        checked_function.parameters.push_back(std::make_pair(param.name, typecheck_parameter(param)));
    }
    return checked_function;
}

void Typechecker::typecheck_function_body(CheckedFunction& checked_function, Parser::ParsedFunctionDeclaration const& function) {
    m_current_function = &checked_function;
    Util::ScopeGuard guard = [this]() {
        m_current_function = nullptr;
    };

    SCOPED_SCOPE(checked_function.argument_scope_id);
    if (function.body) {
        checked_function.body = typecheck_block(*function.body);
    }
}

CheckedStatement Typechecker::typecheck_statement(Parser::ParsedStatement const& stmt) {
    return std::visit(
        Util::Overloaded {
            [&](Parser::ParsedVariableDeclaration const& value) -> CheckedStatement {
                auto initializer = typecheck_expression(value.binding.initializer);

                auto type_id = m_program.unknown_type_id;
                if (!value.binding.type) {
                    type_id = initializer.type.type_id;
                }
                else {
                    type_id = resolve_type(*value.binding.type);
                }

                if (type_id == m_program.unknown_type_id) {
                    error(fmt::format("Invalid type '{}' in variable declaration", value.binding.type ? value.binding.type->to_string().encode() : "<not inferred>"), value.binding.type ? value.binding.type->range : value.binding.name_range);
                }

                CheckedVariable variable {
                    .name = value.binding.name,
                    .type = { .type_id = type_id, .is_mut = value.is_mut },
                    .initializer = std::move(initializer),
                };

                if (type_id != m_program.unknown_type_id) {
                    check_type_compatibility(TypeCompatibility::Assignment, type_id, initializer.type.type_id, value.binding.initializer.range);
                }
                auto var_id = m_current_checked_module->add_variable(std::move(variable));
                m_program.get_scope(m_current_scope).variables.insert({ value.binding.name, var_id });
                return CheckedStatement {
                    .statement = CheckedVariableDeclaration { .var_id = var_id }
                };
            },
            [&](Parser::ParsedReturnStatement const& value) -> CheckedStatement {
                if (m_current_function->return_type == m_program.void_type_id && value.value) {
                    error("Void function cannot return a value", value.range);
                }
                if (m_current_function->return_type != m_program.void_type_id && !value.value) {
                    error("Return without value in a non-void function", value.range);
                }
                return CheckedStatement {
                    .statement = CheckedReturnStatement {
                        .expression = value.value ? typecheck_expression(*value.value) : std::optional<CheckedExpression> {},
                    },
                };
            },
            [&](Parser::ParsedBreakOrContinueStatement const& value) -> CheckedStatement {
                if (!m_is_in_loop) {
                    error(fmt::format("{} may occur only in a loop",
                              value.type == Parser::ParsedBreakOrContinueStatement::Type::Break ? "break" : "continue"),
                        value.range);
                }
                return CheckedStatement {
                    .statement = CheckedBreakOrContinueStatement { .type = value.type }
                };
            },
            [&](Parser::ParsedIfStatement const& value) -> CheckedStatement {
                auto condition = typecheck_expression(value.condition);
                if (!check_type_compatibility(TypeCompatibility::Comparison, m_program.bool_type_id, condition.type.type_id, {})) {
                    error("If statement's condition must be a bool", value.condition.range);
                }
                return CheckedStatement {
                    .statement = CheckedIfStatement {
                        .condition = typecheck_expression(value.condition),
                        .then_clause = typecheck_block(*value.then_clause),
                        .else_clause = value.else_clause ? std::make_unique<CheckedStatement>(typecheck_statement(*value.else_clause)) : nullptr },
                };
            },
            [&](Parser::ParsedForStatement const& value) -> CheckedStatement {
                auto iterable = typecheck_expression(value.iterable);
                auto container_type = m_program.get_type(iterable.type.type_id);
                auto value_type = container_type.iterable_type(m_program);
                if (!value_type) {
                    error(fmt::format("'{}' is not an iterable type", container_type.name(m_program).encode()), value.iterable_range);
                }

                auto scope_id = m_current_checked_module->add_scope(m_current_scope);
                SCOPED_SCOPE(scope_id);
                auto variable = m_current_checked_module->add_variable({
                    .name = value.variable,
                    .type = { .type_id = *value_type, .is_mut = false },
                    .initializer = {},
                });
                m_program.get_scope(m_current_scope).variables.insert({ value.variable, variable });
                Util::TemporaryChange change { m_is_in_loop, true };
                auto block = typecheck_block(*value.block);
                return CheckedStatement {
                    .statement = CheckedForStatement {
                        .variable_name = value.variable,
                        .iterable = std::move(iterable),
                        .value_type_id = *value_type,
                        .block = std::move(block),
                    }
                };
            },
            [&](Parser::ParsedWhileStatement const& value) -> CheckedStatement {
                auto condition = typecheck_expression(value.condition);
                if (!check_type_compatibility(TypeCompatibility::Comparison, m_program.bool_type_id, condition.type.type_id, {})) {
                    error("While statement's condition must be a bool", {});
                }
                Util::TemporaryChange change { m_is_in_loop, true };
                auto block = typecheck_block(*value.block);
                return CheckedStatement {
                    .statement = CheckedWhileStatement {
                        .condition = std::move(condition),
                        .block = std::move(block),
                    }
                };
            },
            [&](Parser::ParsedBlock const& value) -> CheckedStatement {
                return CheckedStatement {
                    .statement = typecheck_block(value),
                };
            },
            [&](Parser::ParsedExpression const& value) -> CheckedStatement {
                auto expr = typecheck_expression(value);
                if (!expr.can_be_discarded(m_program)) {
                    error("This expression cannot be discarded", value.range);
                }
                return CheckedStatement { .statement = std::move(expr) };
            },
        },
        stmt);
}

CheckedBlock Typechecker::typecheck_block(Parser::ParsedBlock const& block) {
    assert(m_current_function);
    CheckedBlock checked_block {
        .scope_id = m_current_checked_module->add_scope(m_current_scope)
    };
    SCOPED_SCOPE(checked_block.scope_id);

    for (auto const& stmt : block.statements) {
        checked_block.statements.push_back(typecheck_statement(stmt));
    }
    return checked_block;
}

CheckedParameter Typechecker::typecheck_parameter(Parser::ParsedParameter const& parameter) {
    auto& scope = m_program.get_scope(m_current_function->argument_scope_id);

    auto type = resolve_type(parameter.type);
    if (type == m_program.unknown_type_id) {
        error(fmt::format("Invalid type '{}' in function parameter", parameter.type.to_string().encode()),
            parameter.type.range);
    }
    auto var_id = m_current_checked_module->add_variable(CheckedVariable {
        .name = parameter.name,
        .type = { .type_id = type, .is_mut = false },
        .initializer = {},
    });
    scope.variables.insert({ parameter.name, var_id });

    return CheckedParameter { .var_id = var_id };
}

CheckedExpression Typechecker::typecheck_expression(Parser::ParsedExpression const& expression) {
    auto expr = std::visit(
        Util::Overloaded {
            [&](std::unique_ptr<Parser::ParsedIntegerLiteral> const& integer_literal) {
                assert(integer_literal->value >= 0);
                return CheckedExpression {
                    .type = { .type_id = m_program.u32_type_id, .is_mut = false },
                    .expression = CheckedExpression::UnsignedIntegerLiteral {
                        .type_id = m_program.u32_type_id,
                        .value = static_cast<uint64_t>(integer_literal->value),
                    },
                };
            },
            [&](std::unique_ptr<Parser::ParsedStringLiteral> const& string_literal) {
                return CheckedExpression {
                    .type = { .type_id = m_program.string_type_id, .is_mut = false },
                    .expression = CheckedExpression::StringLiteral { .value = string_literal->value },
                };
            },
            [&](std::unique_ptr<Parser::ParsedBoolLiteral> const& bool_literal) {
                return CheckedExpression {
                    .type = { .type_id = m_program.bool_type_id, .is_mut = false },
                    .expression = CheckedExpression::BoolLiteral { .value = bool_literal->value },
                };
            },
            [&](std::unique_ptr<Parser::ParsedInlineArray> const& array) {
                if (array->elements.empty()) {
                    return CheckedExpression {
                        .type = { .type_id = m_program.empty_array_type_id, .is_mut = false },
                        .expression = CheckedExpression::EmptyInlineArray {}
                    };
                }

                std::vector<CheckedExpression> elements;

                // Check base type
                std::optional<TypeId> element_type;
                for (auto const& el : array->elements) {
                    auto checked_expr = typecheck_expression(el);
                    if (!element_type) {
                        element_type = checked_expr.type.type_id;
                    }
                    else if (*element_type != checked_expr.type.type_id) {
                        error("All elements of an inline array must have the same type", expression.range);
                        return CheckedExpression::invalid(m_program);
                    }
                    elements.push_back(std::move(checked_expr));
                }

                return CheckedExpression {
                    .type = {
                        .type_id = m_current_checked_module->get_or_add_type(Type {
                            .type = ArrayType {
                                .inner = *element_type,
                                .size = array->elements.size(),
                            },
                        }),
                        .is_mut = false,
                    },
                    .expression = CheckedExpression::InlineArray {
                        .element_type_id = *element_type,
                        .elements = std::move(elements),
                    },
                };
            },
            [&](std::unique_ptr<Parser::ParsedIdentifier> const& identifier) {
                auto resolved_id = resolve_identifier(identifier->id, expression.range);
                switch (resolved_id.type) {
                case ResolvedIdentifier::Type::Variable:
                    return CheckedExpression {
                        .type = m_program.get_variable(VarId { resolved_id.module, resolved_id.id }).type,
                        .expression = std::move(resolved_id),
                    };
                case ResolvedIdentifier::Type::Function: {
                    auto func_type = m_current_checked_module->get_or_add_type(Type {
                        .type = FunctionType {
                            .function = FunctionId { resolved_id.module, resolved_id.id },
                        },
                    });
                    return CheckedExpression {
                        .type = QualifiedType { .type_id = func_type, .is_mut = false },
                        .expression = std::move(resolved_id),
                    };
                }
                case ResolvedIdentifier::Type::Invalid:
                    return CheckedExpression::invalid(m_program);
                }
                ESSA_UNREACHABLE;
            },
            [&](std::unique_ptr<Parser::ParsedBinaryExpression> const& expr) -> CheckedExpression {
                return typecheck_binary_expression(*expr);
            },
            [&](std::unique_ptr<Parser::ParsedArrayIndex> const& expr) -> CheckedExpression {
                auto array = typecheck_expression(expr->array);
                auto index = typecheck_expression(expr->index);

                CheckedExpression::ArrayIndex array_expression {
                    .array = std::make_unique<CheckedExpression>(std::move(array)),
                    .index = std::make_unique<CheckedExpression>(std::move(index)),
                };

                if (index.type.type_id == m_program.u32_type_id) {
                    auto array_type = m_program.get_type(array.type.type_id);
                    return std::visit(
                        Util::Overloaded {
                            [&](ArrayType const& type) {
                                return CheckedExpression {
                                    .type = QualifiedType { .type_id = type.inner, .is_mut = array.type.is_mut },
                                    .expression = std::move(array_expression),
                                };
                            },
                            [&](PrimitiveType const& primitive) {
                                if (primitive.type == PrimitiveType::String) {
                                    return CheckedExpression {
                                        .type = QualifiedType { .type_id = m_program.u32_type_id, .is_mut = array.type.is_mut },
                                        .expression = std::move(array_expression),
                                    };
                                }
                                if (!(std::holds_alternative<PrimitiveType>(array_type.type) && std::get<PrimitiveType>(array_type.type).type == PrimitiveType::Unknown)) {
                                    // TODO: Better range
                                    error(fmt::format("Not found operator[](u32) for '{}'", array_type.name(m_program).encode()),
                                        { expr->array.range.end, expression.range.end });
                                }
                                return CheckedExpression::invalid(m_program);
                            },
                            [&](auto const&) {
                                if (!(std::holds_alternative<PrimitiveType>(array_type.type) && std::get<PrimitiveType>(array_type.type).type == PrimitiveType::Unknown)) {
                                    // TODO: Better range
                                    error(fmt::format("Not found operator[](u32) for '{}'", array_type.name(m_program).encode()),
                                        { expr->array.range.end, expression.range.end });
                                }
                                return CheckedExpression::invalid(m_program);
                            },
                        },
                        array_type.type);
                }
                else if (index.type.type_id == m_program.range_type_id) {
                    auto array_type = m_program.get_type(array.type.type_id);

                    return std::visit(
                        Util::Overloaded {
                            [&](ArrayType const& type) -> CheckedExpression {
                                auto slice = m_current_checked_module->get_or_add_type(Type {
                                    .type = SliceType {
                                        .inner = type.inner,
                                        .elements_are_mut = array.type.is_mut,
                                    },
                                });
                                return CheckedExpression {
                                    .type = QualifiedType { .type_id = slice, .is_mut = array.type.is_mut },
                                    .expression = std::move(array_expression),
                                };
                            },
                            [&](auto const&) -> CheckedExpression {
                                if (!(std::holds_alternative<PrimitiveType>(array_type.type) && std::get<PrimitiveType>(array_type.type).type == PrimitiveType::Unknown)) {
                                    // TODO: Better range
                                    error(fmt::format("Not found operator[](range) for '{}'", array_type.name(m_program).encode()),
                                        { expr->array.range.end, expression.range.end });
                                }
                                return CheckedExpression::invalid(m_program);
                            },
                        },
                        array_type.type);
                }
                else {
                    error("Array index must be an unsigned integer or a range", expr->index.range);
                    return CheckedExpression::invalid(m_program);
                }
                return CheckedExpression::invalid(m_program);
            },
            [&](std::unique_ptr<Parser::ParsedMemberAccess> const& access) {
                auto object = typecheck_expression(access->object);
                auto& type = m_program.get_type(object.type.type_id);
                if (!std::holds_alternative<StructType>(type.type)) {
                    if (!(std::holds_alternative<PrimitiveType>(type.type) && std::get<PrimitiveType>(type.type).type == PrimitiveType::Unknown)) {
                        error(fmt::format("Invalid member access on non-struct type '{}'", type.name(m_program).encode()), access->dot_range);
                    }
                    return CheckedExpression::invalid(m_program);
                }

                auto& struct_ = m_program.get_struct(std::get<StructType>(type.type).id);
                auto field = struct_->find_field(access->member);
                if (!field) {
                    auto method = struct_->find_method(access->member);
                    if (!method) {
                        error(fmt::format("No such field or method '{}' in struct '{}'", access->member.encode(), type.name(m_program).encode()), access->member_name_range);
                        return CheckedExpression::invalid(m_program);
                    }
                    auto function_type = FunctionType { .function = method->get().function };
                    auto type_id = m_current_checked_module->get_or_add_type(Type { .type = function_type });
                    return CheckedExpression {
                        .type = QualifiedType { .type_id = type_id, .is_mut = false },
                        .expression = CheckedExpression::MemberAccess { .object = std::make_unique<CheckedExpression>(std::move(object)), .member = access->member },
                    };
                }
                return CheckedExpression {
                    .type = QualifiedType { .type_id = field->get().type, .is_mut = object.type.is_mut },
                    .expression = CheckedExpression::MemberAccess { .object = std::make_unique<CheckedExpression>(std::move(object)), .member = access->member },
                };
            },
            [&](std::unique_ptr<Parser::ParsedCall> const& call) {
                auto callable = typecheck_expression(call->callable);
                auto callable_type = m_program.get_type(callable.type.type_id);
                if (!std::holds_alternative<FunctionType>(callable_type.type)) {
                    if (!(std::holds_alternative<PrimitiveType>(callable_type.type) && std::get<PrimitiveType>(callable_type.type).type == PrimitiveType::Unknown)) {
                        error("Expression is not callable", call->callable.range);
                    }
                    return CheckedExpression::invalid(m_program);
                }
                auto& function_type = std::get<FunctionType>(callable_type.type);
                auto& function = *m_program.get_function(function_type.function);

                bool is_method_call = std::holds_alternative<CheckedExpression::MemberAccess>(callable.expression);

                // Hack because of print() using varargs which we don't support yet.
                bool skip_typechecking_args = !is_method_call && function.name == "print";

                std::vector<CheckedExpression> arguments;

                size_t c = 0;
                if (is_method_call) {
                    // The first expected argument type is an object, add it
                    // here and skip in the loop below.
                    c++;
                    arguments.push_back(std::move(*std::get<CheckedExpression::MemberAccess>(callable.expression).object));
                }

                for (auto const& arg : call->arguments) {
                    if (skip_typechecking_args) {
                        arguments.push_back(typecheck_expression(arg));
                    }
                    else {
                        if (c >= function.parameters.size()) {
                            error(fmt::format("Too many arguments for call to '{}'", function.name.encode()), call->callable.range);
                            break;
                        }
                        auto checked_arg = typecheck_expression(arg);
                        auto expected_type = m_program.get_variable(function.parameters[c].second.var_id).type.type_id;
                        if (!check_type_compatibility(TypeCompatibility::Assignment, expected_type, checked_arg.type.type_id, {})) {
                            error(fmt::format("Cannot convert '{}' to '{}' for function argument",
                                      m_program.type_name(checked_arg.type.type_id).encode(), m_program.type_name(expected_type).encode()),
                                arg.range);
                        }
                        arguments.push_back(std::move(checked_arg));
                    }
                    c++;
                }

                return CheckedExpression {
                    .type = { .type_id = function.return_type, .is_mut = false },
                    .expression = CheckedExpression::Call {
                        .function_id = function_type.function,
                        .arguments = std::move(arguments),
                    },
                };
            } },
        expression.expression);
    return expr;
}

CheckedExpression Typechecker::typecheck_binary_expression(Parser::ParsedBinaryExpression const& expression) {
    CheckedExpression::BinaryExpression checked_expression;
    checked_expression.operator_ = expression.operator_;
    checked_expression.lhs = std::make_unique<CheckedExpression>(typecheck_expression(expression.lhs));
    checked_expression.rhs = std::make_unique<CheckedExpression>(typecheck_expression(expression.rhs));

    if (expression.operator_ == Parser::ParsedBinaryExpression::Operator::Range) {
        if (checked_expression.lhs->type.type_id != m_program.u32_type_id) {
            error("Begin of a range must be an integer", expression.operator_range);
            return CheckedExpression::invalid(m_program);
        }
        if (checked_expression.rhs->type.type_id != m_program.u32_type_id) {
            error("End of a range must be an integer", expression.operator_range);
            return CheckedExpression::invalid(m_program);
        }
        return { .type = { .type_id = m_program.range_type_id, .is_mut = false }, .expression = std::move(checked_expression) };
    }

    if (expression.is_assignment()) {
        if (!checked_expression.lhs->type.is_mut) {
            error("Cannot assign to non-mutable expression", expression.operator_range);
            return CheckedExpression::invalid(m_program);
        }
        if (!check_type_compatibility(TypeCompatibility::Assignment, checked_expression.lhs->type.type_id, checked_expression.rhs->type.type_id, expression.operator_range))
            return CheckedExpression::invalid(m_program);
        return { .type = { .type_id = m_program.bool_type_id, .is_mut = false }, .expression = std::move(checked_expression) };
    }

    if (expression.is_logical()) {
        if (checked_expression.lhs->type.type_id != m_program.bool_type_id) {
            error("Left-hand-side of logical operator must be a bool", expression.lhs.range);
        }
        if (checked_expression.rhs->type.type_id != m_program.bool_type_id) {
            error("Right-hand-side of logical operator must be a bool", expression.rhs.range);
        }
        return { .type = { .type_id = m_program.bool_type_id, .is_mut = false }, .expression = std::move(checked_expression) };
    }

    if (expression.is_comparison()) {
        if (!check_type_compatibility(TypeCompatibility::Comparison, checked_expression.lhs->type.type_id, checked_expression.rhs->type.type_id, expression.operator_range))
            return CheckedExpression::invalid(m_program);
        return { .type = { .type_id = m_program.bool_type_id, .is_mut = false }, .expression = std::move(checked_expression) };
    }

    if (checked_expression.lhs->type.type_id != checked_expression.rhs->type.type_id) {
        error(fmt::format("Could not find operator '{}' for '{}' and '{}'",
                  Parser::ParsedBinaryExpression::operator_to_string(expression.operator_),
                  m_program.type_name(checked_expression.rhs->type).encode(),
                  m_program.type_name(checked_expression.lhs->type).encode()),
            expression.operator_range);
    }

    return { .type = checked_expression.lhs->type, .expression = std::move(checked_expression) };
}

bool Typechecker::check_type_compatibility(TypeCompatibility mode, TypeId lhs, TypeId rhs, std::optional<Util::SourceRange> range) {
    switch (mode) {
    case TypeCompatibility::Assignment: {
        auto lhs_type = m_program.get_type(lhs);
        // Special case: empty array can be assigned to any array/struct
        // FIXME: Add struct initializer instead of abusing empty array expression
        if ((std::holds_alternative<ArrayType>(lhs_type.type) || std::holds_alternative<StructType>(lhs_type.type))
            && rhs == m_program.empty_array_type_id) {
            return true;
        }
        if (lhs != rhs) {
            if (range) {
                error(fmt::format("Cannot convert '{}' to '{}' in assignment",
                          m_program.type_name(rhs).encode(),
                          m_program.type_name(lhs).encode()),
                    *range);
            }
            return false;
        }
        break;
    }
    case TypeCompatibility::Comparison: {
        if (lhs != rhs) {
            if (range) {
                error(fmt::format("Cannot compare '{}' with '{}'",
                          m_program.type_name(rhs).encode(),
                          m_program.type_name(lhs).encode()),
                    *range);
            }
            return false;
        }
        break;
    }
    }
    return true;
}

ResolvedIdentifier Typechecker::resolve_identifier(Util::UString const& id, Util::SourceRange range) {
    auto* scope = &m_program.get_scope(m_current_scope);
    // fmt::print("resolve identifier {} in {}.{}\n", id.encode(), m_current_scope.module(), m_current_scope.id());
    do {
        if (auto it = scope->variables.find(id); it != scope->variables.end()) {
            return ResolvedIdentifier { .type = ResolvedIdentifier::Type::Variable, .module = it->second.module(), .id = it->second.id() };
        }
        scope = scope->parent ? &m_program.get_scope(*scope->parent) : nullptr;
    } while (scope);

    // 2. Maybe it is a function...
    for (auto const& func : m_current_checked_module->function_to_id) {
        if (func.first == id) {
            return { .type = ResolvedIdentifier::Type::Function, .module = func.second.module(), .id = func.second.id() };
        }
    }

    // 3. Maybe it is a function in an imported module...
    for (auto const& module : m_current_checked_module->imported_modules()) {
        for (auto const& func : module.get().function_to_id) {
            if (func.first == id) {
                return { .type = ResolvedIdentifier::Type::Function, .module = func.second.module(), .id = func.second.id() };
            }
        }
    }
    error(fmt::format("'{}' is not declared", id.encode()), range);
    return {};
}

FunctionId Typechecker::add_function(Parser::ParsedFunctionDeclaration const& parsed_function, std::optional<StructId> scope) {
    auto func = typecheck_function(parsed_function, scope);
    return m_current_checked_module->add_function(std::move(func), parsed_function);
}

TypeId Typechecker::resolve_type(Parser::ParsedType const& type) {
    return std::visit(
        Util::Overloaded {
            [&](Parser::ParsedUnqualifiedType const& type) {
                if (type.name == "bool") {
                    return m_program.bool_type_id;
                }
                if (type.name == "range") {
                    return m_program.range_type_id;
                }
                if (type.name == "string") {
                    return m_program.string_type_id;
                }
                if (type.name == "u32") {
                    return m_program.u32_type_id;
                }
                if (type.name == "void") {
                    return m_program.void_type_id;
                }
                assert(m_current_checked_module);
                auto custom_type = m_current_checked_module->type_to_id.find(type.name);
                if (custom_type == m_current_checked_module->type_to_id.end()) {
                    return m_program.unknown_type_id;
                }
                return custom_type->second;
            },
            [&](Parser::ParsedArrayType const& type) {
                if (type.size) {
                    Type checked_type {
                        .type = ArrayType {
                            .inner = resolve_type(*type.type),
                            .size = *type.size,
                        },
                    };
                    assert(m_current_checked_module);
                    return m_current_checked_module->get_or_add_type(checked_type);
                }
                else {
                    Type checked_type {
                        .type = SliceType {
                            .inner = resolve_type(*type.type),
                        }
                    };
                    assert(m_current_checked_module);
                    return m_current_checked_module->get_or_add_type(checked_type);
                }
            },
        },
        type.type);
}

CheckedExpression CheckedExpression::invalid(CheckedProgram const& program) {
    return CheckedExpression { .type = { .type_id = program.unknown_type_id, .is_mut = false }, .expression = std::monostate {} };
}

bool CheckedExpression::Call::can_be_discarded(CheckedProgram const& program) const {
    auto return_type = program.get_function(function_id)->return_type;
    return return_type == program.void_type_id
        || return_type == program.unknown_type_id;
}

bool CheckedExpression::BinaryExpression::can_be_discarded(CheckedProgram const&) const {
    switch (operator_) {
    case Parser::ParsedBinaryExpression::Operator::Assign:
    case Parser::ParsedBinaryExpression::Operator::AssignAdd:
    case Parser::ParsedBinaryExpression::Operator::AssignSubtract:
    case Parser::ParsedBinaryExpression::Operator::AssignMultiply:
    case Parser::ParsedBinaryExpression::Operator::AssignDivide:
    case Parser::ParsedBinaryExpression::Operator::AssignModulo:
        return true;
    default:
        return false;
    }
}

bool CheckedExpression::can_be_discarded(CheckedProgram const& program) const {
    return std::visit(
        Util::Overloaded {
            [](std::monostate const&) -> bool {
                return true;
            },
            [&](auto const& expr) -> bool {
                return expr.can_be_discarded(program);
            },
        },
        expression);
}

void CheckedVariable::print(CheckedProgram const& program) const {
    fmt::print("{} {}: {} = <expr>", type.is_mut ? "mut" : "let", name.encode(), program.get_type(type.type_id).name(program).encode());
}

void CheckedProgram::print() const {
    auto print_module = [&](Module const& mod) {
        fmt::print("Imports: ");
        for (auto const& imp : mod.imported_modules()) {
            fmt::print("{}, ", imp.get().path);
        }
        fmt::print("\n");
        fmt::print("Structs:\n");
        for (auto const& struct_ : mod.structs()) {
            fmt::print("struct {} {{\n", struct_->name.encode());
            for (auto const& field : struct_->fields) {
                fmt::print("    {}: {}\n", field.name.encode(), type_name(field.type).encode());
            }
            for (auto const& method : struct_->methods) {
                fmt::print("    func {}(...)\n", method.name.encode());
            }
            fmt::print("}}\n");
        }
        fmt::print("Functions:\n");
        for (auto const& func : mod.functions()) {
            if (!func) {
                fmt::print("    <NULL>\n");
                continue;
            }
            fmt::print("    func {}(...) -> {}\n", func->mangled_name(*this).encode(), type_name(func->return_type).encode());
            if (!func->body) {
                fmt::print("        <EXTERN>\n");
                continue;
            }
            for (auto const& stmt : func->body->statements) {
                fmt::print("        ");
                std::visit(
                    Util::Overloaded {
                        [&](CheckedVariableDeclaration const& decl) {
                            get_variable(decl.var_id).print(*this);
                        },
                        [](auto const&) {
                            fmt::print("<expr>");
                        },
                    },
                    stmt.statement);
                fmt::print("\n");
            }
        }
    };
    for (auto const& mod : m_modules) {
        fmt::print("-- Module {}\n", mod->path);
        print_module(*mod);
    }
}

}
