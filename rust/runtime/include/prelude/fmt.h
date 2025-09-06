#pragma once

#include <prelude/types.h>

_BEGIN_DECLS

typedef struct _esl_format_arg {
    void (*print)(void const* data);
    void const* data;
} esl_format_arg;

void _esl_print(esl_static_string fmtstr, size_t argc, esl_format_arg* argv);

void _esl_print_bool(void const* data);
void _esl_print_range(void const* data);
void _esl_print_static_string(void const* data);
void _esl_print_string(void const* data);
void _esl_print_u32(void const* data);

_END_DECLS
