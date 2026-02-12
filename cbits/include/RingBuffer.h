#ifndef RING_BUFFER_CONTROL_H
#define RING_BUFFER_CONTROL_H

#include <atomic>
#include <cstddef>
#include <cstdint>

struct RingBufferControl {
    alignas(64) std::atomic<size_t> write_offset;
    std::atomic<size_t> read_offset; // Added for consumer flow control
    char* buffer_start;
    size_t buffer_size;
};

// Static assertions to verify layout assumptions
// We allow 32-bit or 64-bit platforms, but alignment and padding must ensure consistent 64-byte blocks.
static_assert(alignof(RingBufferControl) == 64, "RingBufferControl alignment expected to be 64");
static_assert(sizeof(RingBufferControl) == 64, "RingBufferControl size expected to be 64");

#endif
