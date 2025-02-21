#include <prelude/panic.h>

#include <cstdio>
#include <cstdlib>

extern "C" {

void _esl_panic(const char* msg) {
    fprintf(stderr, "Panic: %s\n", msg);
    abort();
}
}
