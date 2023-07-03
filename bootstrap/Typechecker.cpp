#include "Typechecker.hpp"

#include "Parser.hpp"
#include <EssaUtil/Config.hpp>
#include <EssaUtil/ScopeGuard.hpp>
#include <variant>

namespace ESL::Typechecker {

Util::UString ArrayType::name(CheckedProgram const& program) const {
    return Util::UString::format("[{}]{}", size, program.get_type(inner).name(program).encode());
}

Util::UString StructType::name(CheckedProgram const& program) const {
    return program.get_struct(id)->name;
}

std::optional<TypeId> PrimitiveType::iterable_type(CheckedProgram const& program) const {
    return type == PrimitiveType::Range ? std::optional(program.u32_type_id) : std::nullopt;
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

CheckedProgram const& Typechecker::typecheck() {
    for (size_t s = 0; s < m_parsed_file.modules.size(); s++) {
        m_current_checked_module = &m_program.module(s);

        // 1. Typecheck structs
        for (auto& struct_ : m_parsed_file.modules[s].struct_declarations) {
            auto struct_id = m_current_checked_module->add_struct(typecheck_struct(struct_));
            fmt::print("Adding type: {} -> {}\n", struct_.name.encode(), struct_id.id());
            auto type_id = m_current_checked_module->add_type(Type { .type = StructType { .id = struct_id } });
            m_current_checked_module->type_to_id.insert({ struct_.name, type_id });
        }

        // 2. Add all functions so that they can be referenced
        for (auto const& func : m_parsed_file.modules[s].function_declarations) {
            auto function_id = m_current_checked_module->add_function();
            m_current_checked_module->function_to_id.insert({ func.name, function_id });
        }

        // 3. Typecheck functions (first pass)
        for (auto& func : m_current_checked_module->function_to_id) {
            get_function(func.second);
        }

        // 4. Typecheck function bodies.
        for (auto& func : m_current_checked_module->function_to_id) {
            auto& checked_function = *m_program.get_function(func.second);
            typecheck_function_body(checked_function, m_parsed_file.modules[func.second.module()].function_declarations[func.second.id()]);
        }
    }
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
        auto type_id = m_program.resolve_type(field.type);
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
    };
}

CheckedFunction Typechecker::typecheck_function(Parser::ParsedFunctionDeclaration const& function) {
    TypeId return_type = [&function, this]() {
        TypeId type_id { m_program.unknown_type_id };
        if (function.return_type) {
            type_id = m_program.resolve_type(*function.return_type);
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
        .name = function.name,
        .parameters = {},
        .body = {},
        .return_type = return_type,
        .argument_scope_id = m_current_checked_module->add_scope(ScopeId {}),
    };
    m_current_function = &checked_function;

    SCOPED_SCOPE(checked_function.argument_scope_id);

    std::vector<std::pair<Util::UString, CheckedParameter>> parameters;
    for (auto const& param : function.parameters) {
        checked_function.parameters.push_back(std::make_pair(param.name, typecheck_parameter(param)));
    }
    return checked_function;
}

void Typechecker::typecheck_function_body(CheckedFunction& checked_function, Parser::ParsedFunctionDeclaration const& function) {
    SCOPED_SCOPE(checked_function.argument_scope_id);
    if (function.body) {
        checked_function.body = typecheck_block(*function.body);
    }
}

CheckedStatement Typechecker::typecheck_statement(Parser::ParsedStatement const& stmt) {
    return std::visit(
        Util::Overloaded {
            [&](Parser::ParsedVariableDeclaration const& value) -> CheckedStatement {
                auto initializer = typecheck_expression(value.initializer);

                auto type_id = m_program.unknown_type_id;
                if (!value.type) {
                    type_id = initializer.type.type_id;
                }
                else {
                    type_id = m_program.resolve_type(*value.type);
                }

                if (type_id == m_program.unknown_type_id) {
                    error(fmt::format("Invalid type '{}' in variable declaration", value.type->to_string().encode()), value.range);
                }

                CheckedVariable variable {
                    .name = value.name,
                    .type = { .type_id = type_id, .is_mut = value.is_mut },
                    .initializer = std::move(initializer),
                };

                if (type_id != m_program.unknown_type_id) {
                    check_type_compatibility(TypeCompatibility::Assignment, type_id, initializer.type.type_id, value.range);
                }
                auto var_id = m_current_checked_module->add_variable(std::move(variable));
                m_program.get_scope(m_current_scope).variables.insert({ value.name, var_id });
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
            [&](Parser::ParsedIfStatement const& value) -> CheckedStatement {
                auto condition = typecheck_expression(value.condition);
                // TODO: range
                if (!check_type_compatibility(TypeCompatibility::Comparison, m_program.bool_type_id, condition.type.type_id, {})) {
                    error("If statement's condition must be a bool", {});
                    return {};
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
                    return CheckedStatement {};
                }

                auto scope_id = m_current_checked_module->add_scope(m_current_scope);
                SCOPED_SCOPE(scope_id);
                auto variable = m_current_checked_module->add_variable({
                    .name = value.variable,
                    .type = { .type_id = *value_type, .is_mut = false },
                    .initializer = {},
                });
                m_program.get_scope(m_current_scope).variables.insert({ value.variable, variable });
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
            [&](Parser::ParsedBlock const& value) -> CheckedStatement {
                return CheckedStatement {
                    .statement = typecheck_block(value),
                };
            },
            [&](Parser::ParsedExpression const& value) -> CheckedStatement {
                return CheckedStatement { .statement = typecheck_expression(value) };
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

    auto var_id = m_current_checked_module->add_variable(CheckedVariable {
        .name = parameter.name,
        .type = { .type_id = m_program.resolve_type(parameter.type), .is_mut = false },
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
                        error("All elements of an inline array must have the same type", array->range);
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
                auto resolved_id = resolve_identifier(identifier->id, identifier->range);
                if (resolved_id.type == ResolvedIdentifier::Type::Invalid) {
                    return CheckedExpression::invalid(m_program);
                }
                if (resolved_id.type != ResolvedIdentifier::Type::Variable) {
                    error(fmt::format("Identifier '{}' must refer to a variable", identifier->id.encode()), identifier->range);
                    return CheckedExpression::invalid(m_program);
                }
                assert(resolved_id.type == ResolvedIdentifier::Type::Variable);
                // fmt::print("VAR {} {}.{}\n", identifier->id.encode(), resolved_id.module, resolved_id.id);
                return CheckedExpression {
                    .type = m_program.get_variable(VarId { resolved_id.module, resolved_id.id }).type,
                    .expression = std::move(resolved_id),
                };
            },
            [&](std::unique_ptr<Parser::ParsedBinaryExpression> const& expr) -> CheckedExpression {
                return typecheck_binary_expression(*expr);
            },
            [&](std::unique_ptr<Parser::ParsedArrayIndex> const& expr) -> CheckedExpression {
                auto array = typecheck_expression(expr->array);
                auto index = typecheck_expression(expr->index);
                if (index.type.type_id != m_program.u32_type_id) {
                    error("Array index must be an unsigned integer", expr->range);
                    return CheckedExpression::invalid(m_program);
                }
                auto array_type = m_program.get_type(array.type.type_id);
                if (!std::holds_alternative<ArrayType>(array_type.type)) {
                    // TODO: Better range
                    error("Indexed expression is not an array", expr->range);
                    return CheckedExpression::invalid(m_program);
                }
                return CheckedExpression {
                    .type = QualifiedType { .type_id = std::get<ArrayType>(array_type.type).inner, .is_mut = array.type.is_mut },
                    .expression = CheckedExpression::ArrayIndex {
                        .array = std::make_unique<CheckedExpression>(std::move(array)),
                        .index = std::make_unique<CheckedExpression>(std::move(index)),
                    },
                };
            },
            [&](std::unique_ptr<Parser::ParsedMemberAccess> const& access) {
                auto object = typecheck_expression(access->object);
                auto& type = m_program.get_type(object.type.type_id);
                if (!std::holds_alternative<StructType>(type.type)) {
                    // FIXME: range
                    error(fmt::format("Invalid member access on non-struct type '{}'", type.name(m_program).encode()), {});
                    return CheckedExpression::invalid(m_program);
                }

                auto& struct_ = m_program.get_struct(std::get<StructType>(type.type).id);
                auto field = struct_->find_field(access->member);
                if (!field) {
                    // FIXME: range
                    error(fmt::format("No such field '{}' in struct '{}'", access->member.encode(), type.name(m_program).encode()), {});
                    return CheckedExpression::invalid(m_program);
                }

                return CheckedExpression {
                    .type = QualifiedType { .type_id = field->get().type, .is_mut = object.type.is_mut },
                    .expression = CheckedExpression::MemberAccess { .object = std::make_unique<CheckedExpression>(std::move(object)), .member = access->member },
                };
            },
            [&](std::unique_ptr<Parser::ParsedCall> const& call) {
                auto identifier = resolve_identifier(call->name, call->name_range);
                if (identifier.type != ResolvedIdentifier::Type::Function) {
                    error(fmt::format("'{}' is not callable", call->name.encode()), call->name_range);
                    return CheckedExpression::invalid(m_program);
                }
                // TODO: Typecheck if arguments matches
                std::vector<CheckedExpression> arguments;
                for (auto const& arg : call->arguments) {
                    arguments.push_back(typecheck_expression(arg));
                }

                auto& function = get_function(FunctionId { identifier.module, identifier.id });
                return CheckedExpression {
                    .type = { .type_id = function.return_type, .is_mut = false },
                    .expression = CheckedExpression::Call {
                        .function_id = FunctionId { identifier.module, identifier.id },
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

    // 2. Maybe it is function...
    for (auto const& func : m_current_checked_module->function_to_id) {
        if (func.first == id) {
            return { .type = ResolvedIdentifier::Type::Function, .module = func.second.module(), .id = func.second.id() };
        }
    }

    // 3. Maybe it is function in prelude...
    for (auto const& func : m_program.module(0).function_to_id) {
        if (func.first == id) {
            return { .type = ResolvedIdentifier::Type::Function, .module = func.second.module(), .id = func.second.id() };
        }
    }
    error(fmt::format("'{}' is not declared", id.encode()), range);
    return {};
}

CheckedFunction const& Typechecker::get_function(FunctionId id) {
    // Use the fact that indices in parsed functions
    // correspond to indices in checked functions
    auto& function_ref = m_program.get_function(id);
    if (!function_ref) {
        function_ref = std::make_unique<CheckedFunction>(typecheck_function(m_parsed_file.modules[id.module()].function_declarations[id.id()]));
    }
    return *function_ref;
}

TypeId CheckedProgram::resolve_type(Parser::ParsedType const& type) {
    return std::visit(
        Util::Overloaded {
            [&](Parser::ParsedUnqualifiedType const& type) {
                if (type.name == "bool") {
                    return bool_type_id;
                }
                if (type.name == "range") {
                    return range_type_id;
                }
                if (type.name == "string") {
                    return string_type_id;
                }
                if (type.name == "u32") {
                    return u32_type_id;
                }
                if (type.name == "void") {
                    return void_type_id;
                }
                auto custom_type = m_root_module.type_to_id.find(type.name);
                if (custom_type == m_root_module.type_to_id.end()) {
                    return unknown_type_id;
                }
                return custom_type->second;
            },
            [&](Parser::ParsedArrayType const& type) {
                Type checked_type {
                    .type = ArrayType {
                        .inner = resolve_type(*type.type),
                        .size = type.size,
                    },
                };
                return m_root_module.get_or_add_type(checked_type);
            },
        },
        type.type);
}

CheckedExpression CheckedExpression::invalid(CheckedProgram const& program) {
    return CheckedExpression { .type = { .type_id = program.unknown_type_id, .is_mut = false }, .expression = std::monostate {} };
}

void CheckedVariable::print(CheckedProgram const& program) const {
    fmt::print("{} {}: {} = <expr>", type.is_mut ? "mut" : "let", name.encode(), program.get_type(type.type_id).name(program).encode());
}

void CheckedProgram::print() const {
    auto print_module = [&](Module const& mod) {
        fmt::print("Functions:\n");
        for (auto const& func : mod.functions()) {
            if (!func) {
                fmt::print("    <NULL>\n");
                continue;
            }
            fmt::print("    func {}(...) -> {}\n", func->name.encode(), get_type(func->return_type).name(*this).encode());
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
    print_module(m_prelude_module);
    print_module(m_root_module);
}
}
