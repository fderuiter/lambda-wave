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
size_t calculate_available_read_bytes(size_t read_offset, size_t write_offset, size_t buffer_size) {
    if (write_offset >= read_offset) {
        return write_offset - read_offset;
    } else {
        return buffer_size - read_offset + write_offset;
    }
}

size_t calculate_next_read_offset(size_t read_offset, size_t consumed, size_t buffer_size) {
    return (read_offset + consumed) % buffer_size;
}
}
