#include "ring_buffer.h"
#include <stdatomic.h>
#include <string.h>

#define BUFFER_SIZE (10 * 1024 * 1024) // 10MB

static uint8_t buffer[BUFFER_SIZE];
static atomic_size_t write_idx = 0;
static atomic_size_t read_idx = 0;

void init_ring_buffer() {
    atomic_store(&write_idx, 0);
    atomic_store(&read_idx, 0);
}

void write_byte(uint8_t byte) {
    size_t current_write = atomic_load(&write_idx);
    size_t next_write = (current_write + 1) % BUFFER_SIZE;

    // Simple overwrite if full (or drop, depending on requirements. Here we overwrite)
    buffer[current_write] = byte;
    atomic_store(&write_idx, next_write);
}

size_t read_chunk(uint8_t* dest, size_t max_len) {
    size_t current_read = atomic_load(&read_idx);
    size_t current_write = atomic_load(&write_idx);

    size_t count = 0;
    while (current_read != current_write && count < max_len) {
        dest[count] = buffer[current_read];
        current_read = (current_read + 1) % BUFFER_SIZE;
        count++;
    }

    atomic_store(&read_idx, current_read);
    return count;
}
