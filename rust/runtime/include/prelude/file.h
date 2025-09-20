#pragma once

#include <prelude/buffer.h>
#include <prelude/types.h>

_BEGIN_DECLS

esl_u32 file_open(esl_static_string path);
esl_u32 file_getchar(esl_u32 fd);
void file_close(esl_u32 fd);
Buffer file_read_to_buf(esl_u32 fd, esl_usize size);

_END_DECLS
