#include "RingBuffer.h"

extern "C" {
size_t get_write_offset(RingBufferControl* handle) {
    if (!handle) return 0;
    return handle->write_offset.load(std::memory_order_acquire);
}
void set_write_offset(RingBufferControl* handle, size_t val) {
    if (!handle) return;
    handle->write_offset.store(val, std::memory_order_release);
}
size_t get_read_offset(RingBufferControl* handle) {
    if (!handle) return 0;
    return handle->read_offset.load(std::memory_order_acquire);
}
void set_read_offset(RingBufferControl* handle, size_t val) {
    if (!handle) return;
    handle->read_offset.store(val, std::memory_order_release);
}
}
