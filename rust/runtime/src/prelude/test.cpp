#include <prelude/test.h>

extern "C" {

ExternTest extern_test_new(esl_u32 v) {
    ExternTest t;
    t.test_value = v;
    return t;
}

esl_u32 extern_test_get_value(ExternTest v) {
    return v.test_value;
}

ExternTest $s$ExternTest$new(esl_u32 value) {
    return extern_test_new(value);
}

esl_u32 $s$ExternTest$get_value(ExternTest* this_) {
    return this_->test_value;
}
}
