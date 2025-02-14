#pragma once

#include <stdint.h>
#include <stdio.h>

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
