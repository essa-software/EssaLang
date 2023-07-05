#include "Builtins.hpp"

#include <iostream>

Util::UString input() {
    // TODO: Propagate errors
    std::string s;
    std::getline(std::cin, s);
    return Util::UString(s);
}
