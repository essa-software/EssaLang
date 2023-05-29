#include "Codegen.hpp"
#include "Lexer.hpp"
#include "Parser.hpp"
#include "Typechecker.hpp"
#include <EssaUtil/DisplayError.hpp>
#include <EssaUtil/Stream.hpp>
#include <EssaUtil/Stream/File.hpp>
#include <EssaUtil/Stream/StandardStreams.hpp>
#include <EssaUtil/Stream/Writer.hpp>
#include <EssaUtil/UString.hpp>
#include <filesystem>
#include <string>

Util::OsErrorOr<bool> run_esl(std::string const& file_name) {
    auto stream = TRY(Util::ReadableFileStream::open(file_name));

    ESL::Lexer lexer { stream };
    auto tokens = TRY(lexer.lex());

    // for (auto const& token : tokens) {
    //     fmt::print("{} {} {}\n", (int)token.type(), token.value().encode(), token.start().offset);
    // }

    ESL::Parser::Parser parser { tokens };
    auto parsed_file = parser.parse_file();
    if (parsed_file.is_error()) {
        auto error = parsed_file.release_error();
        auto stream_for_errors = TRY(Util::ReadableFileStream::open(file_name));
        Util::display_error(stream_for_errors,
            Util::DisplayedError {
                .message = Util::UString { error.message },
                .start_offset = error.location.start.offset,
                .end_offset = error.location.end.offset,
            });
        return false;
    }
    // parsed_file.value().print();

    ESL::Typechecker::Typechecker typechecker { parsed_file.release_value() };
    auto const& program = typechecker.typecheck();
    for (auto const& error : typechecker.errors()) {
        auto stream_for_errors = TRY(Util::ReadableFileStream::open(file_name));
        Util::display_error(stream_for_errors,
            Util::DisplayedError {
                .message = Util::UString { error.message },
                .start_offset = error.range.start.offset,
                .end_offset = error.range.end.offset,
            });
    }
    if (!typechecker.errors().empty()) {
        return false;
    }
    // program.print();
    std::filesystem::create_directory("build");

    auto f_out = TRY(Util::WritableFileStream::open("build/main.cpp", Util::WritableFileStream::OpenOptions{}));

    Util::Writer writer { f_out };
    ESL::Codegen::CodeGenerator code_generator { writer, program };
    TRY(code_generator.codegen());
    auto cmake = TRY(Util::WritableFileStream::open("build/CMakeLists.txt", Util::WritableFileStream::OpenOptions{}));
    Util::Writer cmake_writer(cmake);
    TRY(cmake_writer.write(R"~~~(project(EssaLangTest)
cmake_minimum_required(VERSION 3.17)

find_package(EssaUtil REQUIRED)

add_executable(EssaLangTest
    main.cpp
)
target_link_libraries(EssaLangTest essautil)
essautil_setup_target(EssaLangTest)
set_property(TARGET EssaLangTest PROPERTY OUTPUT_NAME out)
)~~~"));

    system("cd build && cmake -GNinja . && ninja");
    std::filesystem::rename("build/out", "out");
    std::filesystem::remove_all("build");
    
    return true;
}

int main(int argc, char** argv) {
    if (argc < 2) {
        fmt::print("Usage: esl <file name>\n");
        return 3;
    }
    auto err = run_esl(argv[1]);
    if (err.is_error()) {
        auto error = err.release_error();
        fmt::print("Failed to open esl: {}: {}\n", error.function, strerror(error.error));
        return 2;
    }
    if (!err.release_value())
        return 1;
    return 0;
}
