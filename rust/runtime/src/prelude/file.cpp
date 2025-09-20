#include <prelude/file.h>

#include <cstdlib>
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

Buffer file_read_to_buf(esl_u32 fd, esl_usize size) {
    Buffer buf = $s$Buffer$zeros(size);
    ssize_t bytes_read = read(fd, buf.data, size);
    if (bytes_read < 0) {
        // Error (TODO: Error handling)
        $s$Buffer$__drop__(&buf);
        buf.size = 0;
        buf.data = nullptr;
        return buf;
    }
    if ((esl_usize)bytes_read < size) {
        // Resize buffer to actual bytes read
        uint8_t* new_data = (uint8_t*)realloc(buf.data, bytes_read);
        buf.data = new_data;
        buf.size = bytes_read;
    }
    return buf;
}
}
