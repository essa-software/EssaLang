#pragma once

#include "Parser.hpp"
#include <EssaUtil/Error.hpp>

namespace ESL {

Util::ErrorOr<Parser::ParsedFile, Util::OsError, Util::ParseError> parse_file(std::string const& path);

}
