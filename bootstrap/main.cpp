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
#include <fcntl.h>
#include <filesystem>
#include <spawn.h>
#include <string>
#include <sys/wait.h>

Util::OsErrorOr<void> run_process(std::vector<std::string> const& args, bool output) {
    pid_t pid;
    std::vector<char*> c_args;
    for (auto const& arg : args) {
        // Top posix moment
        c_args.push_back(const_cast<char*>(arg.c_str()));
    }
    c_args.push_back(nullptr);

    posix_spawn_file_actions_t action;
    posix_spawn_file_actions_init(&action);
    if (!output) {
        posix_spawn_file_actions_addopen(&action, STDOUT_FILENO, "/dev/null", O_WRONLY | O_APPEND, 0);
    }

    if (posix_spawnp(&pid, args[0].c_str(), &action, nullptr, c_args.data(), environ) != 0) {
        fmt::print("failed to run {}: {}\n", args[0], strerror(errno));
        return Util::OsError { .error = errno, .function = "run_process posix_spawn" };
    }
    int status = 0;
    if (waitpid(pid, &status, 0) < 0) {
        fmt::print("failed to wait for {}: {}\n", args[0], strerror(errno));
        return Util::OsError { .error = errno, .function = "run_process waitpid" };
    }
    return {};
}

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
    program.print();
    if (!typechecker.errors().empty()) {
        return false;
    }
    std::filesystem::remove_all("build");
    std::filesystem::create_directory("build");

    auto f_out = TRY(Util::WritableFileStream::open("build/main.cpp", Util::WritableFileStream::OpenOptions {}));

    Util::Writer writer { f_out };
    ESL::Codegen::CodeGenerator code_generator { writer, program };
    TRY(code_generator.codegen());
    auto cmake = TRY(Util::WritableFileStream::open("build/CMakeLists.txt", Util::WritableFileStream::OpenOptions {}));
    Util::Writer cmake_writer(cmake);

    TRY(cmake_writer.write(R"~~~(# Generated by esl. Do not edit!
cmake_minimum_required(VERSION 3.17)
project(EssaLangTest)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED TRUE)

find_package(fmt REQUIRED)

add_executable(EssaLangTest
    main.cpp
)

set(ESL_SOURCE_DIR ")~~~")); TRY(cmake_writer.write(ESL_SOURCE_DIR)); TRY(cmake_writer.write(R"~~~(")
set(ESL_BINARY_DIR ")~~~")); TRY(cmake_writer.write(ESL_BINARY_DIR)); TRY(cmake_writer.write(R"~~~(")
target_include_directories(EssaLangTest PRIVATE ${ESL_SOURCE_DIR})
target_link_libraries(EssaLangTest PRIVATE ${ESL_BINARY_DIR}/runtime/libesl-runtime.a fmt::fmt)

set_property(TARGET EssaLangTest PROPERTY OUTPUT_NAME out)
)~~~"));

    std::filesystem::current_path("build");
    TRY(run_process({ "cmake", "-GNinja", "." }, false));
    TRY(run_process({ "ninja" }, true));
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
