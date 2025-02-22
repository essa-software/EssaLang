#include <prelude/fmt.h>

#include <prelude/panic.h>

#include <cstdio>

extern "C" {

void _esl_print(const char* fmtstr, size_t argc, esl_format_arg* argv) {
    // replace every "{}" in fmtstr with the next argument
    size_t idx = 0;
    while (*fmtstr) {
        if (*fmtstr == '{' && fmtstr[1] == '}') {
            if (idx >= argc) {
                _esl_panic("Not enough arguments passed for format string");
            }
            argv[idx].print(argv[idx].data);
            idx++;
            fmtstr += 2;
        }
        else {
            putchar(*fmtstr);
            fmtstr++;
        }
    }
}

void _esl_print_bool(void const* data) {
    printf("%s", *(esl_bool const*)data ? "true" : "false");
}

void _esl_print_static_string(void const* data) {
    printf("%s", (const char*)data);
}

void _esl_print_u32(void const* data) {
    printf("%u", *(esl_u32 const*)data);
}

void _esl_print_range(void const* data) {
    esl_range const* range = (esl_range const*)data;
    printf("%zu..%zu", range->_start, range->_end);
}
}
