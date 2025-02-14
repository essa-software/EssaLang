#include <std/io.h>

#include <stdio.h>

extern "C" {

void print(const char* fmtstr) {
    printf("%s", fmtstr);
}

}
