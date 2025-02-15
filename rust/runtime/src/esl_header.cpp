#include <esl_header.h>

#include <cstdlib>

extern "C" {

void _esl_panic(const char* msg) {
    fprintf(stderr, "Panic: %s\n", msg);
    abort();
}

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

void _esl_print_bool(void* data) {
    printf("%s", *(esl_bool*)data ? "true" : "false");
}

void _esl_print_static_string(void* data) {
    printf("%s", (const char*)data);
}

void _esl_print_u32(void* data) {
    printf("%u", *(esl_u32*)data);
}
}
