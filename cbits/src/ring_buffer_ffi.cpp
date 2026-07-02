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
size_t rb_available_data(RingBufferControl* handle, size_t current_read_offset) {
    if (!handle) return 0;
    size_t write_off = handle->write_offset.load(std::memory_order_acquire);
    size_t size = handle->buffer_size;
    if (write_off >= current_read_offset) {
        return write_off - current_read_offset;
    } else {
        return size - current_read_offset + write_off;
    }
}

size_t rb_next_read_offset(RingBufferControl* handle, size_t current_read_offset, size_t consumed_bytes) {
    if (!handle) return current_read_offset;
    size_t size = handle->buffer_size;
    return (current_read_offset + consumed_bytes) % size;
}
}
