#pragma once

#include "Typechecker.hpp"
#include <EssaUtil/Stream.hpp>

namespace ESL::Codegen {

class CodeGenerator {
public:
    CodeGenerator(Util::Writer& writer, Typechecker::CheckedProgram const& program)
        : m_writer(writer)
        , m_program(program) { }

    Util::OsErrorOr<void> codegen();

private:
    Util::OsErrorOr<void> codegen_prelude();
    Util::OsErrorOr<void> codegen_epilogue();
    Util::OsErrorOr<void> codegen_struct(Typechecker::CheckedStruct const&);
    Util::OsErrorOr<void> codegen_function_declaration(Typechecker::CheckedFunction const&);
    Util::OsErrorOr<void> codegen_function(Typechecker::CheckedFunction const&);
    Util::OsErrorOr<void> codegen_type(Typechecker::Type const&);
    Util::OsErrorOr<void> codegen_qualified_type(Typechecker::QualifiedType const&, bool reference);
    Util::OsErrorOr<void> codegen_statement(Typechecker::CheckedStatement const&);
    Util::OsErrorOr<void> codegen_block(Typechecker::CheckedBlock const&);
    Util::OsErrorOr<void> codegen_expression(Typechecker::CheckedExpression const&);
    Util::OsErrorOr<void> codegen_binary_expression(Typechecker::CheckedExpression::BinaryExpression const&);

    Util::Writer& m_writer;
    Typechecker::CheckedProgram const& m_program;
};

}
