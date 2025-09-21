#include <cassert>
#include <cstdlib>
#include <prelude/rc.h>

_BEGIN_DECLS

Rc esl_rc_new(size_t data_size) {
    Rc rc = (Rc)malloc(sizeof(RcData) + data_size);
    assert(rc);
    rc->strong_count = 1;
    return rc;
}

void esl_rc_free(Rc rc) {
    assert(rc->strong_count == 0);
    free(rc);
}

_END_DECLS
