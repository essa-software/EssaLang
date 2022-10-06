#include "Lexer.hpp"
#include "Parser.hpp"
#include "Typechecker.hpp"
#include <EssaUtil/Stream.hpp>

// TODO: TYPECHECKER
// TODO: CODEGEN

Util::OsErrorOr<void> run_esl(std::string const& file_name) {
    auto stream = TRY(Util::ReadableFileStream::open(file_name));

    ESL::Lexer lexer { stream };
    auto tokens = TRY(lexer.lex());

    for (auto const& token : tokens) {
        fmt::print("{} {}\n", (int)token.type(), token.value());
    }

    ESL::Parser::Parser parser { tokens };
    auto parsed_file = parser.parse_file();
    if (parsed_file.is_error()) {
        auto error = parsed_file.release_error();
        fmt::print("Parse error: {} at {}:{}\n", error.message, error.location.start.line + 1, error.location.start.column + 1);
        return {};
    }
    parsed_file.value().print();

    ESL::Typechecker::Typechecker typechecker { parsed_file.release_value() };
    auto const& program = typechecker.typecheck();
    for (auto const& error : typechecker.errors()) {
        fmt::print("Typechecker error: {} at {}:{}\n", error.message, error.range.start.line + 1, error.range.start.column + 1);
    }
    program.print();
    return {};
}

int main(int argc, char** argv) {
    if (argc < 2) {
        fmt::print("Usage: esl <file name>\n");
        return 1;
    }
    auto err = run_esl(argv[1]);
    if (err.is_error()) {
        auto error = err.release_error();
        fmt::print("Failed to open esl: {}: {}\n", error.function, strerror(error.error));
        return 1;
    }
    return 0;
}
