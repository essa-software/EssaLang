#include <prelude/test.h>

#include <cstdio>

extern "C" {

ExternTest extern_test_new(esl_u32 v) {
    ExternTest t;
    t.test_value = v;
    return t;
}

esl_u32 extern_test_get_value(ExternTest v) {
    return v.test_value;
}
}
