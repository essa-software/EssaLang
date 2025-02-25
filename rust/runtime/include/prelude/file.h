#pragma once

#include <prelude/types.h>

_BEGIN_DECLS

esl_u32 file_open(esl_static_string path);
esl_u32 file_getchar(esl_u32 fd);
void file_close(esl_u32 fd);

_END_DECLS
