#include "../include/RingBuffer.h"
#include <unistd.h>
#include <cstdlib>
#include <new>
#include <cstring>
#include <cerrno>

extern "C" {

RingBufferControl* create_ring_buffer(size_t size) {
    size_t total_size = sizeof(RingBufferControl) + size;
    
    void* mem = std::malloc(total_size);
    if (!mem) return nullptr;

    RingBufferControl* control = new (mem) RingBufferControl();
    control->write_offset = 0;
    control->read_offset = 0;
    
    control->buffer_offset = sizeof(RingBufferControl);
    control->buffer_size = size;

    return control;
}

RingBufferControl* attach_ring_buffer(size_t size) {
    // Single process architecture, attach just creates a new one or should not be used
    return create_ring_buffer(size);
}

void get_buffer_pointers(RingBufferControl* control, char** buf_start, size_t* size) {
    if (control) {
        *buf_start = reinterpret_cast<char*>(control) + control->buffer_offset;
        *size = control->buffer_size;
    }
}

void free_ring_buffer(RingBufferControl* handle) {
    if (handle) {
        handle->~RingBufferControl();
        std::free(handle);
    }
}

void detach_ring_buffer(RingBufferControl* handle) {
    free_ring_buffer(handle);
}

ssize_t read_from_uart(RingBufferControl* handle, int uart_fd) {
    if (!handle) return -1;

    size_t current_offset = handle->write_offset;
    size_t read_offset = handle->read_offset;
    
    // Use dynamic pointer computation
    char* buf_start = reinterpret_cast<char*>(handle) + handle->buffer_offset;
    size_t size = handle->buffer_size;

    size_t available_contiguous;

    if (read_offset > current_offset) {
        available_contiguous = read_offset - current_offset - 1;
    } else {
        size_t space_to_end = size - current_offset;
        if (read_offset == 0) {
            available_contiguous = space_to_end - 1;
        } else {
            available_contiguous = space_to_end;
        }
    }

    if (available_contiguous == 0) {
        return 0;
    }

    ssize_t bytes_read;
    do {
        bytes_read = read(uart_fd, buf_start + current_offset, available_contiguous);
    } while (bytes_read == -1 && errno == EINTR);

    if (bytes_read == -1) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return -3;
        }
        return -1;
    }

    if (bytes_read == 0) {
        return -2;
    }

    if (bytes_read > 0) {
        size_t new_offset = current_offset + bytes_read;
        if (new_offset >= size) {
            new_offset = 0;
        }
        handle->write_offset = new_offset;
    }

    return bytes_read;
}

}

// Requirement FR-DAQ-001

// Requirement FR-DAQ-004
// Hazard H-SOUP-003: FFI Memory Leaks

