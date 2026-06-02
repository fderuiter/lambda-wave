#include "../include/RingBuffer.h"
#include <unistd.h>
#include <sys/mman.h>
#include <cstdlib>
#include <new>
#include <cstring>
#include <cerrno>
#include <fcntl.h>

extern "C" {

RingBufferControl* create_ring_buffer(size_t size) {
    size_t total_size = sizeof(RingBufferControl) + size;
    
    shm_unlink("/sgrt_ring_buffer");
    int fd = shm_open("/sgrt_ring_buffer", O_CREAT | O_RDWR, 0666);
    if (fd == -1) return nullptr;
    if (ftruncate(fd, total_size) == -1) { close(fd); return nullptr; }

    void* mem = mmap(nullptr, total_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    close(fd);
    if (mem == MAP_FAILED) return nullptr;
    if (mlock(mem, total_size) != 0) { munmap(mem, total_size); return nullptr; }

    RingBufferControl* control = new (mem) RingBufferControl();
    for (size_t i = 0; i < NUM_BLOCKS; i++) {
        control->block_states[i].store(static_cast<uint32_t>(BlockState::AVAILABLE), std::memory_order_relaxed);
        control->block_bytes_written[i] = 0;
    }
    
    control->buffer_start = static_cast<char*>(mem) + sizeof(RingBufferControl);
    control->buffer_size = size;
    control->current_write_block = 0;
    control->current_write_offset = 0;

    return control;
}

RingBufferControl* attach_ring_buffer(size_t size) {
    size_t total_size = sizeof(RingBufferControl) + size;
    int fd = shm_open("/sgrt_ring_buffer", O_RDWR, 0666);
    if (fd == -1) return nullptr;
    void* mem = mmap(nullptr, total_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    close(fd);
    if (mem == MAP_FAILED) return nullptr;
    if (mlock(mem, total_size) != 0) { munmap(mem, total_size); return nullptr; }
    return static_cast<RingBufferControl*>(mem);
}

void get_buffer_pointers(RingBufferControl* control, char** buf_start, size_t* size) {
    if (control) {
        *buf_start = reinterpret_cast<char*>(control) + sizeof(RingBufferControl);
        *size = control->buffer_size;
    }
}

void free_ring_buffer(RingBufferControl* handle) {
    if (handle) {
        size_t total_size = sizeof(RingBufferControl) + handle->buffer_size;
        munlock(handle, total_size);
        handle->~RingBufferControl();
        munmap(handle, total_size);
        shm_unlink("/sgrt_ring_buffer");
    }
}

void detach_ring_buffer(RingBufferControl* handle) {
    if (handle) {
        size_t total_size = sizeof(RingBufferControl) + handle->buffer_size;
        munlock(handle, total_size);
        munmap(handle, total_size);
    }
}

ssize_t checkout_block(RingBufferControl* handle) {
    if (!handle) return -1;
    for (size_t i = 0; i < NUM_BLOCKS; i++) {
        uint32_t expected = static_cast<uint32_t>(BlockState::READY);
        if (handle->block_states[i].compare_exchange_strong(
                expected, static_cast<uint32_t>(BlockState::CHECKED_OUT), std::memory_order_acquire)) {
            return static_cast<ssize_t>(i);
        }
    }
    return -1;
}

void release_block(RingBufferControl* handle, size_t block_index) {
    if (!handle || block_index >= NUM_BLOCKS) return;
    handle->block_states[block_index].store(static_cast<uint32_t>(BlockState::AVAILABLE), std::memory_order_release);
}

size_t get_block_bytes_written(RingBufferControl* handle, size_t block_index) {
    if (!handle || block_index >= NUM_BLOCKS) return 0;
    return handle->block_bytes_written[block_index];
}

ssize_t read_from_uart(RingBufferControl* handle, int uart_fd) {
    if (!handle) return -1;

    size_t current_block = handle->current_write_block;
    uint32_t state = handle->block_states[current_block].load(std::memory_order_acquire);
    
    if (state != static_cast<uint32_t>(BlockState::WRITING)) {
        bool found = false;
        size_t next_block = current_block;
        for (size_t i = 0; i < NUM_BLOCKS; i++) {
            uint32_t expected = static_cast<uint32_t>(BlockState::AVAILABLE);
            if (handle->block_states[next_block].compare_exchange_strong(
                    expected, static_cast<uint32_t>(BlockState::WRITING), std::memory_order_acq_rel)) {
                found = true;
                current_block = next_block;
                handle->current_write_block = current_block;
                handle->current_write_offset = 0;
                break;
            }
            next_block = (next_block + 1) % NUM_BLOCKS;
        }
        if (!found) return 0; // Buffer full
    }

    size_t block_size = handle->buffer_size / NUM_BLOCKS;
    size_t offset = handle->current_write_offset;
    size_t available_space = block_size - offset;

    if (available_space == 0) {
        handle->block_bytes_written[current_block] = block_size;
        handle->block_states[current_block].store(static_cast<uint32_t>(BlockState::READY), std::memory_order_release);
        
        bool found = false;
        size_t next_block = (current_block + 1) % NUM_BLOCKS;
        for (size_t i = 0; i < NUM_BLOCKS; i++) {
            uint32_t expected = static_cast<uint32_t>(BlockState::AVAILABLE);
            if (handle->block_states[next_block].compare_exchange_strong(
                    expected, static_cast<uint32_t>(BlockState::WRITING), std::memory_order_acq_rel)) {
                found = true;
                current_block = next_block;
                handle->current_write_block = current_block;
                handle->current_write_offset = 0;
                break;
            }
            next_block = (next_block + 1) % NUM_BLOCKS;
        }
        if (!found) return 0;
        
        offset = handle->current_write_offset;
        available_space = block_size - offset;
    }

    char* buf_start = reinterpret_cast<char*>(handle) + sizeof(RingBufferControl);
    char* write_ptr = buf_start + (current_block * block_size) + offset;

    ssize_t bytes_read;
    do {
        bytes_read = read(uart_fd, write_ptr, available_space);
    } while (bytes_read == -1 && errno == EINTR);

    if (bytes_read == -1) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            // Eager flush on EAGAIN to reduce latency
            if (handle->current_write_offset > 0) {
                handle->block_states[current_block].store(static_cast<uint32_t>(BlockState::READY), std::memory_order_release);
            }
            return -3;
        }
        return -1;
    }
    if (bytes_read == 0) {
        // EOF, flush
        if (handle->current_write_offset > 0) {
            handle->block_states[current_block].store(static_cast<uint32_t>(BlockState::READY), std::memory_order_release);
        }
        return -2;
    }

    handle->current_write_offset += bytes_read;
    handle->block_bytes_written[current_block] = handle->current_write_offset;

    // If block filled exactly, we could flush it here, but it will be handled by available_space == 0 next time.
    return bytes_read;
}

}
