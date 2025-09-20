#pragma once

#include <prelude/types.h>

_BEGIN_DECLS

typedef struct Buffer_ {
    uint8_t* data;
    esl_usize size;
} Buffer;

Buffer $s$Buffer$zeros(esl_usize size);
// FIXME: u32->u8
esl_u32 $s$Buffer$get(Buffer* this_, esl_usize index);
// FIXME: u32->u8
void $s$Buffer$set(Buffer* this_, esl_usize index, esl_u32 value);
void $s$Buffer$__drop__(Buffer* this_);

_END_DECLS
