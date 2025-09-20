#include "prelude/panic.h"
#include <prelude/types.h>

#include <cstdlib>
#include <prelude/buffer.h>

_BEGIN_DECLS

Buffer $s$Buffer$zeros(esl_usize size) {
    Buffer buf;
    buf.size = size;
    buf.data = (uint8_t*)calloc(size, sizeof(uint8_t));
    return buf;
}

esl_usize $s$Buffer$size(Buffer* this_) {
    return this_->size;
}

// FIXME: u32->u8
esl_u32 $s$Buffer$get(Buffer* this_, esl_usize index) {
    if (index >= this_->size) {
        _esl_panic("index out of bounds");
    }
    return this_->data[index];
}

// FIXME: u32->u8
void $s$Buffer$set(Buffer* this_, esl_usize index, esl_u32 value) {
    if (index >= this_->size) {
        _esl_panic("index out of bounds");
    }
    this_->data[index] = (uint8_t)value;
}

void $s$Buffer$__drop__(Buffer* this_) {
    free(this_->data);
}

_END_DECLS
