#include "../include/RingBuffer.h"
#include <unistd.h>
#include <sys/mman.h>
#include <cstdlib>
#include <new>
#include <cstring>

extern "C" {

RingBufferControl* create_ring_buffer(size_t size) {
    void* control_mem = nullptr;
    if (posix_memalign(&control_mem, 64, sizeof(RingBufferControl)) != 0) {
        return nullptr;
    }

    void* buffer_mem = nullptr;
    size_t page_size = sysconf(_SC_PAGESIZE);

    // Ensure the buffer memory is aligned to page size for efficient mlock usage
    if (posix_memalign(&buffer_mem, page_size, size) != 0) {
        free(control_mem);
        return nullptr;
    }

    // Pin memory to prevent paging (critical for real-time performance)
    if (mlock(buffer_mem, size) != 0) {
        free(buffer_mem);
        free(control_mem);
        return nullptr;
    }

    // Initialize Control structure using placement new
    RingBufferControl* control = new (control_mem) RingBufferControl();
    control->write_offset.store(0, std::memory_order_relaxed);
    control->buffer_start = static_cast<char*>(buffer_mem);
    control->buffer_size = size;

    return control;
}

void free_ring_buffer(RingBufferControl* handle) {
    if (handle) {
        munlock(handle->buffer_start, handle->buffer_size);
        free(handle->buffer_start);
        handle->~RingBufferControl();
        free(handle);
    }
}

ssize_t read_from_uart(RingBufferControl* handle, int uart_fd) {
    if (!handle) return -1;

    size_t current_offset = handle->write_offset.load(std::memory_order_relaxed);
    char* buf_start = handle->buffer_start;
    size_t size = handle->buffer_size;

    // Determine max bytes we can read before wrapping around the buffer end
    size_t bytes_to_end = size - current_offset;

    // Attempt to read as much as possible up to the end of the linear buffer segment
    ssize_t bytes_read = read(uart_fd, buf_start + current_offset, bytes_to_end);

    if (bytes_read > 0) {
        size_t new_offset = current_offset + bytes_read;

        // If we reached the end of the buffer, wrap around to 0
        if (new_offset >= size) {
            new_offset = 0;
        }

        // Publish the new offset with release semantics
        handle->write_offset.store(new_offset, std::memory_order_release);
    }

    return bytes_read;
}

}
