#pragma once

#include <prelude/panic.h>
#include <prelude/types.h>

_BEGIN_DECLS

typedef struct RefCell_ {
    esl_u32 readers;
    esl_u32 writers;
    esl_u8 data[];
} RefCell;

#define REFCELL_SIZEOF(T) (sizeof(RefCell) + sizeof(T))

static void esl_refcell_lock_r(RefCell* r) {
    if (r->writers > 0) {
        _esl_panic("RefCell: trying to share lock while exclusively locked");
    }
    r->readers++;
}

static void esl_refcell_unlock_r(RefCell* r) {
    if (r->readers == 0) {
        _esl_panic("RefCell: shared unlock not paired with shared lock");
    }
    r->readers--;
}

static void esl_refcell_lock_w(RefCell* r) {
    if (r->readers > 0 || r->writers > 0) {
        _esl_panic("RefCell: trying to exclusively lock while locked");
    }
    r->writers++;
}

static void esl_refcell_unlock_w(RefCell* r) {
    if (r->writers == 0) {
        _esl_panic("RefCell: exclusive unlock not paired with exclusive lock");
    }
    r->writers--;
}

static void* esl_refcell_data(RefCell* r) {
    return r->data;
}

typedef struct RcData_ {
    esl_u32 strong_count;
    // esl_u32 weak_count;
    esl_u8 data[];
} RcData;

typedef RcData* Rc;

Rc esl_rc_new(size_t data_size);
void esl_rc_free(Rc);

static void* esl_rc_data(Rc rc) {
    return rc->data;
}

static void* esl_rc_refcell_data(Rc rc) {
    return esl_refcell_data((RefCell*)esl_rc_data(rc));
}

_END_DECLS
