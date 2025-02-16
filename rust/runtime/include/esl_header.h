#pragma once

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef __cplusplus
#    define _BEGIN_DECLS extern "C" {
#    define _END_DECLS }
#else
#    define _BEGIN_DECLS
#    define _END_DECLS
#endif

_BEGIN_DECLS

//// builtin types ////

typedef uint32_t esl_u32;
typedef size_t esl_usize;
typedef bool esl_bool;

typedef char const* esl_static_string;

typedef struct _esl_string {
    esl_usize _size;
    void* _data;
} esl_string;

typedef struct _esl_range {
    esl_usize _start;
    esl_usize _end;
} esl_range;

//// builtin functions ////

void _esl_panic(const char* msg);

/// print hack ///

typedef struct _esl_format_arg {
    void (*print)(void* data);
    void* data;
} esl_format_arg;

void _esl_print(const char* fmtstr, size_t argc, esl_format_arg* argv);

void _esl_print_bool(void* data);
void _esl_print_static_string(void* data);
void _esl_print_u32(void* data);

//// builtin operator overloads ////

#define _esl_opcmpeq_u32_u32(a, b) (a == b)
#define _esl_opcmpneq_u32_u32(a, b) (a != b)
#define _esl_opcmplt_u32_u32(a, b) (a < b)
#define _esl_opcmplte_u32_u32(a, b) (a <= b)
#define _esl_opcmpgt_u32_u32(a, b) (a > b)
#define _esl_opcmpgte_u32_u32(a, b) (a >= b)

_END_DECLS
