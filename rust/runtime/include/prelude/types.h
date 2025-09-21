#pragma once

#include <prelude/config.h>

#include <stddef.h>
#include <stdint.h>

_BEGIN_DECLS

typedef bool esl_bool;
typedef struct _esl_char {
    uint32_t cp;
} esl_char;
typedef uint8_t esl_u8;
typedef uint32_t esl_u32;
typedef size_t esl_usize;

typedef char const* esl_static_string;

typedef struct _esl_string {
    esl_usize _size;
    void* _data;
} esl_string;

typedef struct _esl_range {
    esl_usize _begin;
    esl_usize _end;
} esl_range;

_END_DECLS
