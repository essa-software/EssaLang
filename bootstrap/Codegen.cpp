#include "Codegen.hpp"
#include "Typechecker.hpp"
#include <EssaUtil/Config.hpp>
#include <EssaUtil/Error.hpp>

namespace ESL::Codegen {

Util::OsErrorOr<void> CodeGenerator::codegen() {
    TRY(codegen_prelude());

    auto codegen_module = [&](Typechecker::Module const& mod) -> Util::OsErrorOr<void> {
        for (auto const& function : mod.functions()) {
            TRY(codegen_function(*function));
        }
        return {};
    };

    TRY(codegen_module(m_program.module(1))); // root

    TRY(codegen_epilogue());

    return {};
}

Util::OsErrorOr<void> CodeGenerator::codegen_prelude() {
    m_writer.writeff("// Generated by esl\n");
    m_writer.writeff("#include <fmt/format.h>\n");
    m_writer.writeff("#include <EssaUtil/UString.hpp>\n\n");
    TRY(m_writer.write(R"~~~(template<class... Args>
void print(fmt::format_string<Args...>&& fmtstr, Args&&... args) { fmt::print(std::move(fmtstr), std::forward<Args>(args)...); }

)~~~"));
    return {};
}

Util::OsErrorOr<void> CodeGenerator::codegen_epilogue() {
    TRY(m_writer.write("int main() { return (int)___esl_main(); }\n\n"));
    return {};
}

Util::OsErrorOr<void> CodeGenerator::codegen_function(Typechecker::CheckedFunction const& function) {
    TRY(codegen_type(m_program.get_type(function.return_type)));
    m_writer.writeff(" {}(", function.name == "main" ? "___esl_main" : function.name.encode());
    m_writer.writeff(")");
    if (function.body) {
        m_writer.writeff(" {{\n");
        TRY(codegen_block(*function.body));
        if (function.name == "main") {
            m_writer.writeff("\nreturn 0;\n");
        }
        m_writer.writeff("}}\n\n");
    }
    else {
        m_writer.writeff(";\n\n");
    }
    return {};
}

Util::OsErrorOr<void> CodeGenerator::codegen_type(Typechecker::Type const& type) {
    switch (type.type) {
    case Typechecker::Type::Primitive::Unknown:
        ESSA_UNREACHABLE;
    case Typechecker::Type::Primitive::Void:
        TRY(m_writer.write("void"));
        break;
    case Typechecker::Type::Primitive::U32:
        TRY(m_writer.write("uint32_t"));
        break;
    case Typechecker::Type::Primitive::String:
        TRY(m_writer.write("Util::UString"));
        break;
    }
    return {};
}

Util::OsErrorOr<void> CodeGenerator::codegen_block(Typechecker::CheckedBlock const& block) {
    TRY(m_writer.write("{\n"));
    for (auto const& stmt : block.statements) {
        TRY(std::visit(
            Util::Overloaded {
                [&](Typechecker::CheckedVariableDeclaration const& decl) -> Util::OsErrorOr<void> {
                    auto const& var = m_program.get_variable(decl.var_id);
                    TRY(codegen_type(m_program.get_type(var.type_id)));
                    m_writer.writeff(" {} {}", var.is_mut ? "" : "const", var.name.encode());
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
                [&](Typechecker::CheckedReturnStatement const& stmt) -> Util::OsErrorOr<void> {
                    TRY(m_writer.write("return "));
                    if (stmt.expression) {
                        TRY(codegen_expression(*stmt.expression));
                    }
                    TRY(m_writer.write(";\n"));
                    return {};
                },
            },
            stmt.statement));
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
                    if (func->name == "print" && s == 0 && m_program.get_type(call.arguments[s].type_id).name() == "string") {
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
            [&](Typechecker::CheckedExpression::UnsignedIntegerLiteral const& expr) -> Util::OsErrorOr<void> {
                m_writer.writeff("{}ull", expr.value);
                return {};
            },
            [&](Typechecker::CheckedExpression::StringLiteral const& expr) -> Util::OsErrorOr<void> {
                // TODO: Escape
                m_writer.writeff("Util::UString{{\"{}\"}}", expr.value.encode());
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
    TRY(codegen_expression(*expression.lhs));
    switch (expression.operator_) {
    case Parser::ParsedBinaryExpression::Operator::Add:
        TRY(m_writer.write("+"));
        break;
    case Parser::ParsedBinaryExpression::Operator::Subtract:
        TRY(m_writer.write("-"));
        break;
    case Parser::ParsedBinaryExpression::Operator::Multiply:
        TRY(m_writer.write("*"));
        break;
    case Parser::ParsedBinaryExpression::Operator::Divide:
        TRY(m_writer.write("/"));
        break;
    case Parser::ParsedBinaryExpression::Operator::Modulo:
        TRY(m_writer.write("%"));
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
        TRY(m_writer.write("/*="));
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
