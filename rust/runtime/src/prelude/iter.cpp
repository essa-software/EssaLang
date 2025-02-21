#include <prelude/iter.h>

#include <prelude/panic.h>

extern "C" {

esl_iterator__range _esl_iterator__range_new(esl_range const* range) {
    return (esl_iterator__range) { .current = range->_start, .end = range->_end };
}

esl_usize _esl_iterator__range_next(esl_iterator__range* self) {
    if (self->current >= self->end) {
        _esl_panic("No more elements in range");
    }
    return self->current++;
}

esl_bool _esl_iterator__range_has_next(esl_iterator__range const* self) {
    return self->current < self->end;
}
}
