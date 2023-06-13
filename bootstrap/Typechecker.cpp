#include "Typechecker.hpp"
#include "bootstrap/Parser.hpp"
#include <EssaUtil/Config.hpp>
#include <EssaUtil/ScopeGuard.hpp>

namespace ESL::Typechecker {

CheckedProgram const& Typechecker::typecheck() {
    for (size_t s = 0; s < m_parsed_file.modules.size(); s++) {
        m_current_checked_module = &m_program.module(s);

        // 1. Add all functions so that they can be referenced
        for (auto const& func : m_parsed_file.modules[s].function_declarations) {
            auto function_id = m_current_checked_module->add_function();
            m_current_checked_module->function_to_id.insert({ func.name, function_id });
        }

        // 2. Typecheck functions and their bodies.
        for (auto& func : m_current_checked_module->function_to_id) {
            get_function(func.second);
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

CheckedFunction Typechecker::typecheck_function(Parser::ParsedFunctionDeclaration const& function) {
    TypeId return_type = [&function, this]() {
        TypeId type_id { m_program.unknown_type_id };
        if (function.return_type) {
            type_id = m_program.resolve_type(function.return_type->name);
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
    if (function.body) {
        checked_function.body = typecheck_block(*function.body);
    }
    return checked_function;
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
                    type_id = m_program.resolve_type(value.type->name);
                }

                CheckedVariable variable {
                    .name = value.name,
                    .type = { .type_id = type_id, .is_mut = value.is_mut },
                    .initializer = std::move(initializer),
                };

                check_type_compatibility(TypeCompatibility::Assignment, type_id, initializer.type.type_id, value.range);
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
                auto iterable_type = m_program.get_type(iterable.type.type_id);
                if (!iterable_type.is_iterable()) {
                    error(fmt::format("'{}' is not an iterable type", iterable_type.name().encode()), value.iterable_range);
                    return CheckedStatement {};
                }

                auto iterable_value_type = m_program.resolve_type(iterable_type.iterable_value_type());

                auto scope_id = m_current_checked_module->add_scope(m_current_scope);
                SCOPED_SCOPE(scope_id);
                auto variable = m_current_checked_module->add_variable({
                    .name = value.variable,
                    .type = { .type_id = iterable_value_type, .is_mut = false },
                    .initializer = {},
                });
                m_program.get_scope(m_current_scope).variables.insert({ value.variable, variable });
                auto block = typecheck_block(*value.block);
                return CheckedStatement {
                    .statement = CheckedForStatement {
                        .variable_name = value.variable,
                        .iterable = std::move(iterable),
                        .value_type_id = iterable_value_type,
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
        .type = { .type_id = m_program.resolve_type(parameter.type.name), .is_mut = false },
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

CheckedExpression CheckedExpression::invalid(CheckedProgram const& program) {
    return CheckedExpression { .type = { .type_id = program.unknown_type_id, .is_mut = false }, .expression = std::monostate {} };
}

void CheckedVariable::print(CheckedProgram const& program) const {
    fmt::print("{} {}: {} = <expr>", type.is_mut ? "mut" : "let", name.encode(), program.get_type(type.type_id).name().encode());
}

void CheckedProgram::print() const {
    auto print_module = [&](Module const& mod) {
        fmt::print("Functions:\n");
        for (auto const& func : mod.functions()) {
            if (!func) {
                fmt::print("    <NULL>\n");
                continue;
            }
            fmt::print("    func {}(...) -> {}\n", func->name.encode(), get_type(func->return_type).name().encode());
            if (!func->body) {
                fmt::print("        <EXTERN>\n");
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
