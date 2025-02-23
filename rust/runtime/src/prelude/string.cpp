#include <prelude/string.h>

#include <cstring>

extern "C" {

esl_u32 str_size(esl_static_string s) {
    return strlen(s);
}
}
