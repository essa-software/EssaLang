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

typedef struct _esl_format_args_data {
    void (*print)(void* data);
    void* data;
} esl_format_args_data;
typedef struct _esl_format_args {
    esl_format_args_data node;
    struct _esl_format_args* next;
} esl_format_args;

esl_format_args* _esl_format_args_push(esl_format_args* end, esl_format_args_data data);

// this takes ownership of the list
void _esl_print(const char* fmtstr, esl_format_args* args);

void _esl_print_u32(void* data);
void _esl_print_static_string(void* data);

_END_DECLS
