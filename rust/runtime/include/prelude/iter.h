#pragma once

#include <prelude/types.h>

_BEGIN_DECLS

// All iterator structs follow this scheme:
//  iterator type: `struct esl_iterator__<iterable_type> {...}`
//  constructor from iterable: `iterator _esl_iterator__<iterable_type>_new(<iterable_type> const* iterable)`
//  iterator.next(): `<value_type> _esl_iterator__<iterable_type>_next(iterator* self)`
//  iterator.has_next(): `esl_bool _esl_iterator__<iterable_type>_has_next(iterator const* self)`

// Iterator for iterable_type=range (value_type=u32)
typedef struct _esl_iterator__range {
    esl_usize current;
    esl_usize end;
} esl_iterator__range;

esl_iterator__range _esl_iterator__range_new(esl_range const* range);
esl_usize _esl_iterator__range_next(esl_iterator__range* self);
esl_bool _esl_iterator__range_has_next(esl_iterator__range const* self);

_END_DECLS
