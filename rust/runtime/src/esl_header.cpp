#include <esl_header.h>

#include <cstdlib>

extern "C" {

void _esl_panic(const char* msg) {
    fprintf(stderr, "Panic: %s\n", msg);
    abort();
}

esl_u32 _esl_opadd_u32_u32(esl_u32 a, esl_u32 b) {
    if (a > UINT32_MAX - b) {
        _esl_panic("Overflow in addition");
    }
    return a + b;
}

esl_u32 _esl_opsub_u32_u32(esl_u32 a, esl_u32 b) {
    if (a < b) {
        _esl_panic("Underflow in subtraction");
    }
    return a - b;
}

esl_u32 _esl_opmul_u32_u32(esl_u32 a, esl_u32 b) {
    if (a == 0 || b == 0) {
        return 0;
    }
    if (a > UINT32_MAX / b) {
        _esl_panic("Overflow in multiplication");
    }
    return a * b;
}

esl_u32 _esl_opdiv_u32_u32(esl_u32 a, esl_u32 b) {
    if (b == 0) {
        _esl_panic("Division by zero");
    }
    return a / b;
}

esl_u32 _esl_opmod_u32_u32(esl_u32 a, esl_u32 b) {
    if (b == 0) {
        _esl_panic("Division by zero");
    }
    return a % b;
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
