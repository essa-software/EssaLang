#include "Esl.hpp"

#include "Lexer.hpp"
#include "Parser.hpp"
#include <EssaUtil/GenericParser.hpp>
#include <EssaUtil/Stream.hpp>

#include <string>

namespace ESL {

Util::ErrorOr<Parser::ParsedFile, Util::OsError, Util::ParseError> parse_file(std::string const& path) {
    auto stream = TRY(Util::ReadableFileStream::open(path));

    ESL::Lexer lexer { stream };
    auto tokens = TRY(lexer.lex());

    // for (auto const& token : tokens) {
    //     fmt::print("{} {} {}\n", (int)token.type(), token.value().encode(), token.start().offset);
    // }

    ESL::Parser::Parser parser { tokens };
    return parser.parse_file();
}

}
