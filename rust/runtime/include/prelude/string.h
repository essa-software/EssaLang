#pragma once

#include <prelude/types.h>

_BEGIN_DECLS

// extern func string_new(lit: static_string): string;
esl_string string_new(esl_static_string lit);
// extern func string_drop(string: string);
void string_drop(esl_string s);
// extern func str_size(s: static_string) -> u32
esl_u32 str_size(esl_static_string s);

_END_DECLS
