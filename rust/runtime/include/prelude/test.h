#pragma once

#include <prelude/config.h>
#include <prelude/types.h>

_BEGIN_DECLS

typedef struct _ExternTest {
    int test_value;
} ExternTest;

ExternTest extern_test_new(esl_u32 value);
esl_u32 extern_test_get_value(ExternTest v);

ExternTest $s$ExternTest$new(esl_u32 value);
esl_u32 $s$ExternTest$get_value(ExternTest* this_);

typedef struct _ExternTestDrop {
    int _dummy;
} ExternTestDrop;

ExternTestDrop $s$ExternTestDrop$new();
void $s$ExternTestDrop$__drop__(ExternTestDrop* this_);

_END_DECLS
