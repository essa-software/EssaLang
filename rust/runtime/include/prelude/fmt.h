#pragma once

#include <prelude/types.h>

_BEGIN_DECLS

typedef struct _esl_format_arg {
    void (*print)(void* data);
    void* data;
} esl_format_arg;

void _esl_print(esl_static_string fmtstr, size_t argc, esl_format_arg* argv);

void _esl_print_bool(void* data);
void _esl_print_static_string(void* data);
void _esl_print_u32(void* data);
void _esl_print_range(void* data);

_END_DECLS
