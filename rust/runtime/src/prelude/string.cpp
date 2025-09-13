#include <prelude/string.h>

#include <cstdio>
#include <cstdlib>
#include <cstring>

extern "C" {

esl_string string_new(esl_static_string lit) {
    esl_string s;
    s._size = strlen(lit);
    s._data = malloc(s._size + 1);
    memcpy(s._data, lit, s._size + 1);
    return s;
}

void string_drop(esl_string s) {
    printf("dropped string: %s\n", (char*)s._data);
    free(s._data);
}

esl_usize str_size(esl_static_string s) {
    return strlen(s);
}
}
