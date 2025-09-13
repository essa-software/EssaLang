#include <prelude/ops.h>

#include <prelude/panic.h>

extern "C" {

esl_u32 _esl_opadd_u32_u32(esl_u32 a, esl_u32 b) {
    if (a > UINT32_MAX - b) {
        _esl_panic("Overflow in `u32` addition");
    }
    return a + b;
}

esl_u32 _esl_opsub_u32_u32(esl_u32 a, esl_u32 b) {
    if (a < b) {
        _esl_panic("Underflow in `u32` subtraction");
    }
    return a - b;
}

esl_u32 _esl_opmul_u32_u32(esl_u32 a, esl_u32 b) {
    if (a == 0 || b == 0) {
        return 0;
    }
    if (a > UINT32_MAX / b) {
        _esl_panic("Overflow in `u32` multiplication");
    }
    return a * b;
}

esl_u32 _esl_opdiv_u32_u32(esl_u32 a, esl_u32 b) {
    if (b == 0) {
        _esl_panic("Division by zero");
    }
    return a / b;
}

esl_u32 _esl_opmod_u32_u32(esl_u32 a, esl_u32 b) {
    if (b == 0) {
        _esl_panic("Division by zero");
    }
    return a % b;
}

esl_usize _esl_opadd_usize_usize(esl_usize a, esl_usize b) {
    if (a > SIZE_MAX - b) {
        _esl_panic("Overflow in `usize` addition");
    }
    return a + b;
}

esl_usize _esl_opsub_usize_usize(esl_usize a, esl_usize b) {
    if (a < b) {
        _esl_panic("Underflow in `usize` subtraction");
    }
    return a - b;
}

esl_usize _esl_opmul_usize_usize(esl_usize a, esl_usize b) {
    if (a == 0 || b == 0) {
        return 0;
    }
    if (a > SIZE_MAX / b) {
        _esl_panic("Overflow in `usize` multiplication");
    }
    return a * b;
}

esl_usize _esl_opdiv_usize_usize(esl_usize a, esl_usize b) {
    if (b == 0) {
        _esl_panic("Division by zero");
    }

    return a / b;
}

esl_usize _esl_opmod_usize_usize(esl_usize a, esl_usize b) {
    if (b == 0) {
        _esl_panic("Division by zero");
    }
    return a % b;
}
}
