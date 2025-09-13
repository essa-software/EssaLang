#pragma once

#include <prelude/config.h>
#include <prelude/types.h>

_BEGIN_DECLS

typedef struct _ExternTest {
    int test_value;
} ExternTest;

ExternTest extern_test_new(esl_u32 value);
esl_u32 extern_test_get_value(ExternTest v);

_END_DECLS
