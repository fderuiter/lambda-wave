#include "../include/RingBuffer.h"
#include <unistd.h>
#include <sys/mman.h>
#include <cstdlib>
#include <new>
#include <cstring>
#include <cerrno>

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
    // Relaxed ordering is sufficient for initialization as the pointer is not yet published
    control->write_offset.store(0, std::memory_order_relaxed);
    control->read_offset.store(0, std::memory_order_relaxed);
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

    // Load write_offset relaxed: we are the only writer to it.
    size_t current_offset = handle->write_offset.load(std::memory_order_relaxed);
    // Load read_offset acquire: ensures we see the latest value updated by the consumer
    size_t read_offset = handle->read_offset.load(std::memory_order_acquire);
    char* buf_start = handle->buffer_start;
    size_t size = handle->buffer_size;

    // Determine max bytes we can read before wrapping around the buffer end
    // AND without overwriting unread data (respecting read_offset).
    size_t available_contiguous;

    if (read_offset > current_offset) {
        // Space is between write cursor and read cursor (minus 1 to distinguish full vs empty)
        available_contiguous = read_offset - current_offset - 1;
    } else {
        // read_offset <= current_offset
        // We can write until the end of the buffer...
        size_t space_to_end = size - current_offset;

        // ...unless read_offset is 0, in which case we must stop one byte short of the end
        // to avoid wrapping to 0 and colliding with read_offset (making it look empty).
        if (read_offset == 0) {
            available_contiguous = space_to_end - 1;
        } else {
            available_contiguous = space_to_end;
        }
    }

    if (available_contiguous == 0) {
        // Buffer is full (or at least the contiguous block is full/blocked).
        return -4; // READ_BUFFER_FULL
    }

    // Attempt to read as much as possible up to the safe limit
    ssize_t bytes_read;
    do {
        bytes_read = read(uart_fd, buf_start + current_offset, available_contiguous);
    } while (bytes_read == -1 && errno == EINTR);

    if (bytes_read == -1) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return -3; // READ_BUSY / EAGAIN
        }
        return -1; // READ_ERROR
    }

    if (bytes_read == 0) {
        return -2; // READ_EOF (UART Disconnected)
    }

    if (bytes_read > 0) {
        size_t new_offset = current_offset + bytes_read;

        // If we reached the end of the buffer, wrap around to 0
        if (new_offset >= size) {
            new_offset = 0;
        }

        // Publish the new offset with release semantics
        // This ensures the data written to the buffer is visible to the consumer
        // before they see the updated write_offset.
        handle->write_offset.store(new_offset, std::memory_order_release);
    }

    return bytes_read;
}

size_t get_write_offset(RingBufferControl* handle) {
    if (!handle) return 0;
    // Acquire semantics to ensure we see the data committed before this offset update
    return handle->write_offset.load(std::memory_order_acquire);
}

void set_read_offset(RingBufferControl* handle, size_t offset) {
    if (!handle) return;
    handle->read_offset.store(offset, std::memory_order_release);
}

}
