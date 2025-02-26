#include <prelude/file.h>

#include <fcntl.h>
#include <unistd.h>

extern "C" {

esl_u32 file_open(esl_static_string path) {
    return open(path, O_RDONLY);
}

esl_u32 file_getchar(esl_u32 fd) {
    uint8_t c;
    if (read(fd, &c, 1) == 1) {
        return c;
    }
    else {
        return (esl_u32)-1;
    }
}

void file_close(esl_u32 fd) {
    close(fd);
}
}
