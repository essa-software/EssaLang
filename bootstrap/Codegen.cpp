#include "Codegen.hpp"

#include "Parser.hpp"
#include "Typechecker.hpp"
#include <EssaUtil/Config.hpp>
#include <EssaUtil/Error.hpp>

namespace ESL::Codegen {

Util::OsErrorOr<void> CodeGenerator::codegen() {
    TRY(codegen_prelude());

    for (auto const& mod : m_program.modules()) {
        for (auto const& struct_ : mod->structs()) {
            TRY(codegen_struct(*struct_));
        }
    }
    for (auto const& mod : m_program.modules()) {
        for (auto const& function : mod->functions()) {
            if (!function->body) {
                TRY(m_writer.write("// EXTERN: "));
                TRY(codegen_function_declaration(*function));
                TRY(m_writer.write("\n"));
                continue;
            }
            TRY(codegen_function_declaration(*function));
            TRY(m_writer.write(";\n"));
        }
    }
    for (auto const& mod : m_program.modules()) {
        for (auto const& function : mod->functions()) {
            if (!function->body) {
                continue;
            }
            TRY(codegen_function(*function));
        }
    }

    TRY(codegen_epilogue());

    return {};
}

Util::OsErrorOr<void> CodeGenerator::codegen_prelude() {
    m_writer.writeff("// Generated by esl\n");
    m_writer.writeff("#include <runtime/Builtins.hpp>\n");
    return {};
}

Util::OsErrorOr<void> CodeGenerator::codegen_epilogue() {
    TRY(m_writer.write("int main() { return (int)___esl_main(); }\n\n"));
    return {};
}

Util::OsErrorOr<void> CodeGenerator::codegen_struct(Typechecker::CheckedStruct const& struct_) {
    m_writer.writeff("struct {} {{\n", struct_.name.encode());

    for (auto const& field : struct_.fields) {
        TRY(m_writer.write("    "));
        TRY(codegen_type(m_program.get_type(field.type)));
        m_writer.writeff(" {} = {{}};\n", field.name.encode());
    }

    TRY(m_writer.write("};\n"));
    return {};
}

Util::OsErrorOr<void> CodeGenerator::codegen_function_declaration(Typechecker::CheckedFunction const& function) {
    TRY(codegen_type(m_program.get_type(function.return_type)));
    m_writer.writeff(" {}(", function.name == "main" ? "___esl_main" : function.name.encode());
    for (size_t s = 0; s < function.parameters.size(); s++) {
        auto const& param = function.parameters[s];
        auto const& var = m_program.get_variable(param.second.var_id);
        TRY(codegen_qualified_type(var.type));
        m_writer.writeff(" {}", var.name.encode());
        if (s != function.parameters.size() - 1) {
            TRY(m_writer.write(", "));
        }
    }
    m_writer.writeff(")");
    return {};
}

Util::OsErrorOr<void> CodeGenerator::codegen_function(Typechecker::CheckedFunction const& function) {
    TRY(codegen_function_declaration(function));
    assert(function.body);
    m_writer.writeff(" {{\n");
    TRY(codegen_block(*function.body));
    if (function.name == "main") {
        m_writer.writeff("\nreturn 0;\n");
    }
    m_writer.writeff("}}\n\n");
    return {};
}

Util::OsErrorOr<void> CodeGenerator::codegen_type(Typechecker::Type const& type) {
    return std::visit(
        Util::Overloaded {
            [&](Typechecker::Nc const&) -> Util::OsErrorOr<void> {
                ESSA_UNREACHABLE;
            },
            [&](Typechecker::PrimitiveType const& primitive) -> Util::OsErrorOr<void> {
                switch (primitive.type) {
                case Typechecker::PrimitiveType::Unknown:
                    TRY(m_writer.write("/*unknown*/"));
                    break;
                case Typechecker::PrimitiveType::Void:
                    TRY(m_writer.write("void"));
                    break;
                case Typechecker::PrimitiveType::Bool:
                    TRY(m_writer.write("bool"));
                    break;
                case Typechecker::PrimitiveType::U32:
                    TRY(m_writer.write("uint32_t"));
                    break;
                case Typechecker::PrimitiveType::String:
                    TRY(m_writer.write("Util::UString"));
                    break;
                case Typechecker::PrimitiveType::Range:
                    TRY(m_writer.write("___Esl::Range"));
                    break;
                case Typechecker::PrimitiveType::EmptyArray:
                    TRY(m_writer.write("___Esl::EmptyArray"));
                    break;
                }
                return {};
            },
            [&](Typechecker::ArrayType const& array) -> Util::OsErrorOr<void> {
                TRY(m_writer.write("std::array<"));
                TRY(codegen_type(m_program.get_type(array.inner)));
                m_writer.writeff(", {}>", array.size);
                return {};
            },
            [&](Typechecker::StructType const& struct_) -> Util::OsErrorOr<void> {
                // Struct type is codegen'd first
                m_writer.writeff("{}", struct_.name(m_program).encode());
                return {};
            },
        },
        type.type);
    return {};
}

Util::OsErrorOr<void> CodeGenerator::codegen_qualified_type(Typechecker::QualifiedType const& type) {
    TRY(codegen_type(m_program.get_type(type.type_id)));
    if (!type.is_mut) {
        TRY(m_writer.write(" const"));
    }
    return {};
}

Util::OsErrorOr<void> CodeGenerator::codegen_statement(Typechecker::CheckedStatement const& stmt) {
    return std::visit(
        Util::Overloaded {
            [&](Typechecker::CheckedVariableDeclaration const& decl) -> Util::OsErrorOr<void> {
                auto const& var = m_program.get_variable(decl.var_id);
                TRY(codegen_qualified_type(var.type));
                m_writer.writeff(" {}", var.name.encode());
                if (var.initializer) {
                    m_writer.writeff(" = ");
                    TRY(codegen_expression(*var.initializer));
                }
                TRY(m_writer.write(";\n"));
                return {};
            },
            [&](Typechecker::CheckedExpression const& expr) -> Util::OsErrorOr<void> {
                TRY(codegen_expression(expr));
                TRY(m_writer.write(";\n"));
                return {};
            },
            [&](Typechecker::CheckedBreakOrContinueStatement const& stmt) -> Util::OsErrorOr<void> {
                TRY(m_writer.write(Util::UString(stmt.type == Parser::ParsedBreakOrContinueStatement::Type::Break ? "break" : "continue")));
                TRY(m_writer.write(";\n"));
                return {};
            },
            [&](Typechecker::CheckedReturnStatement const& stmt) -> Util::OsErrorOr<void> {
                TRY(m_writer.write("return "));
                if (stmt.expression) {
                    TRY(codegen_expression(*stmt.expression));
                }
                TRY(m_writer.write(";\n"));
                return {};
            },
            [&](Typechecker::CheckedIfStatement const& stmt) -> Util::OsErrorOr<void> {
                TRY(m_writer.write("if ("));
                TRY(codegen_expression(stmt.condition));
                TRY(m_writer.write(")"));
                TRY(codegen_block(stmt.then_clause));
                if (stmt.else_clause) {
                    TRY(m_writer.write("else\n"));
                    TRY(codegen_statement(*stmt.else_clause));
                }
                return {};
            },
            [&](Typechecker::CheckedForStatement const& stmt) -> Util::OsErrorOr<void> {
                TRY(m_writer.write("for ("));
                TRY(codegen_type(m_program.get_type(stmt.value_type_id)));
                m_writer.writeff(" {} : ", stmt.variable_name.encode());
                TRY(codegen_expression(stmt.iterable));
                TRY(m_writer.write(")"));
                TRY(codegen_block(stmt.block));
                return {};
            },
            [&](Typechecker::CheckedWhileStatement const& stmt) -> Util::OsErrorOr<void> {
                TRY(m_writer.write("while ("));
                TRY(codegen_expression(stmt.condition));
                TRY(m_writer.write(")"));
                TRY(codegen_block(stmt.block));
                return {};
            },
            [&](Typechecker::CheckedBlock const& stmt) -> Util::OsErrorOr<void> {
                TRY(codegen_block(stmt));
                return {};
            },
        },
        stmt.statement);
}

Util::OsErrorOr<void> CodeGenerator::codegen_block(Typechecker::CheckedBlock const& block) {
    TRY(m_writer.write("{\n"));
    for (auto const& stmt : block.statements) {
        TRY(codegen_statement(stmt));
    }
    TRY(m_writer.write("}\n"));
    return {};
}

Util::OsErrorOr<void> CodeGenerator::codegen_expression(Typechecker::CheckedExpression const& expression) {
    TRY(std::visit(
        Util::Overloaded {
            [&](Typechecker::CheckedExpression::Call const& call) -> Util::OsErrorOr<void> {
                auto const& func = m_program.get_function(call.function_id);
                m_writer.writeff("{}(", func->name.encode());
                for (size_t s = 0; s < call.arguments.size(); s++) {
                    if (func->name == "print" && s == 0 && call.arguments[s].type.type_id == m_program.string_type_id) {
                        m_writer.writeff("\"{}\"", std::get<Typechecker::CheckedExpression::StringLiteral>(call.arguments[s].expression).value.encode());
                    }
                    else {
                        TRY(codegen_expression(call.arguments[s]));
                    }
                    if (s != call.arguments.size() - 1)
                        TRY(m_writer.write(", "));
                }
                m_writer.writeff(")", func->name.encode());
                return {};
            },
            [&](Typechecker::CheckedExpression::BinaryExpression const& expr) -> Util::OsErrorOr<void> {
                TRY(codegen_binary_expression(expr));
                return {};
            },
            [&](Typechecker::CheckedExpression::ArrayIndex const& expr) -> Util::OsErrorOr<void> {
                m_writer.writeff("___Esl::{}((", expr.array->type.is_mut ? "safe_array_access_mut" : "safe_array_access");
                TRY(codegen_expression(*expr.array));
                TRY(m_writer.write("), ("));
                TRY(codegen_expression(*expr.index));
                TRY(m_writer.write("))"));
                return {};
            },
            [&](Typechecker::CheckedExpression::MemberAccess const& expr) -> Util::OsErrorOr<void> {
                TRY(codegen_expression(*expr.object));
                m_writer.writeff(".{}", expr.member.encode());
                return {};
            },
            [&](Typechecker::CheckedExpression::UnsignedIntegerLiteral const& expr) -> Util::OsErrorOr<void> {
                m_writer.writeff("{}ull", expr.value);
                return {};
            },
            [&](Typechecker::CheckedExpression::StringLiteral const& expr) -> Util::OsErrorOr<void> {
                // TODO: Escape
                m_writer.writeff("Util::UString{{\"{}\"}}", expr.value.encode());
                return {};
            },
            [&](Typechecker::CheckedExpression::BoolLiteral const& expr) -> Util::OsErrorOr<void> {
                m_writer.writeff("{}", expr.value);
                return {};
            },
            [&](Typechecker::CheckedExpression::EmptyInlineArray const&) -> Util::OsErrorOr<void> {
                TRY(m_writer.write("___Esl::EmptyArray{}"));
                return {};
            },
            [&](Typechecker::CheckedExpression::InlineArray const& array) -> Util::OsErrorOr<void> {
                auto type = m_program.get_type(array.element_type_id);
                m_writer.writeff("std::array<");
                TRY(codegen_type(type));
                m_writer.writeff(", {}>{{", array.elements.size());
                for (size_t s = 0; s < array.elements.size(); s++) {
                    TRY(codegen_expression(array.elements[s]));
                    if (s != array.elements.size() - 1)
                        TRY(m_writer.write(", "));
                }
                TRY(m_writer.write("}"));
                return {};
            },
            [&](Typechecker::ResolvedIdentifier const& expr) -> Util::OsErrorOr<void> {
                switch (expr.type) {
                case Typechecker::ResolvedIdentifier::Type::Variable:
                    m_writer.writeff("{}", m_program.get_variable(Typechecker::VarId { expr.module, expr.id }).name.encode());
                    break;
                case Typechecker::ResolvedIdentifier::Type::Function:
                    m_writer.writeff("{}", m_program.get_function(Typechecker::FunctionId { expr.module, expr.id })->name.encode());
                    break;
                case Typechecker::ResolvedIdentifier::Type::Invalid:
                    ESSA_UNREACHABLE;
                }
                return {};
            },
            [&](std::monostate) -> Util::OsErrorOr<void> {
                m_writer.writeff("((void)0)");
                return {};
            },
        },
        expression.expression));
    return {};
}

Util::OsErrorOr<void> CodeGenerator::codegen_binary_expression(Typechecker::CheckedExpression::BinaryExpression const& expression) {
    TRY(m_writer.write("("));

    if (expression.operator_ == Parser::ParsedBinaryExpression::Operator::Range) {
        TRY(m_writer.write("___Esl::Range{"));
        TRY(codegen_expression(*expression.lhs));
        TRY(m_writer.write(","));
        TRY(codegen_expression(*expression.rhs));
        TRY(m_writer.write("})"));
        return {};
    }

    TRY(codegen_expression(*expression.lhs));
    switch (expression.operator_) {
    case Parser::ParsedBinaryExpression::Operator::Range:
        ESSA_UNREACHABLE;
    case Parser::ParsedBinaryExpression::Operator::Multiply:
        TRY(m_writer.write("*"));
        break;
    case Parser::ParsedBinaryExpression::Operator::Divide:
        TRY(m_writer.write("/"));
        break;
    case Parser::ParsedBinaryExpression::Operator::Modulo:
        TRY(m_writer.write("%"));
        break;
    case Parser::ParsedBinaryExpression::Operator::Add:
        TRY(m_writer.write("+"));
        break;
    case Parser::ParsedBinaryExpression::Operator::Subtract:
        TRY(m_writer.write("-"));
        break;
    case Parser::ParsedBinaryExpression::Operator::IsEqual:
        TRY(m_writer.write("=="));
        break;
    case Parser::ParsedBinaryExpression::Operator::IsNotEqual:
        TRY(m_writer.write("!="));
        break;
    case Parser::ParsedBinaryExpression::Operator::IsLess:
        TRY(m_writer.write("<"));
        break;
    case Parser::ParsedBinaryExpression::Operator::IsLessEq:
        TRY(m_writer.write("<="));
        break;
    case Parser::ParsedBinaryExpression::Operator::IsGreater:
        TRY(m_writer.write(">"));
        break;
    case Parser::ParsedBinaryExpression::Operator::IsGreaterEq:
        TRY(m_writer.write(">="));
        break;
    case Parser::ParsedBinaryExpression::Operator::Assign:
        TRY(m_writer.write("="));
        break;
    case Parser::ParsedBinaryExpression::Operator::AssignAdd:
        TRY(m_writer.write("+="));
        break;
    case Parser::ParsedBinaryExpression::Operator::AssignSubtract:
        TRY(m_writer.write("-="));
        break;
    case Parser::ParsedBinaryExpression::Operator::AssignMultiply:
        TRY(m_writer.write("*="));
        break;
    case Parser::ParsedBinaryExpression::Operator::AssignDivide:
        TRY(m_writer.write("/="));
        break;
    case Parser::ParsedBinaryExpression::Operator::AssignModulo:
        TRY(m_writer.write("%="));
        break;
    case Parser::ParsedBinaryExpression::Operator::Invalid:
        ESSA_UNREACHABLE;
    }
    TRY(codegen_expression(*expression.rhs));
    TRY(m_writer.write(")"));
    return {};
}
}
