#include "RingBuffer.h"

extern "C" {
size_t get_write_offset(RingBufferControl* handle) {
    if (!handle) return 0;
    return handle->write_offset;
}
void set_write_offset(RingBufferControl* handle, size_t val) {
    if (!handle) return;
    handle->write_offset = val;
}
size_t get_read_offset(RingBufferControl* handle) {
    if (!handle) return 0;
    return handle->read_offset;
}
void set_read_offset(RingBufferControl* handle, size_t val) {
    if (!handle) return;
    handle->read_offset = val;
}
}

