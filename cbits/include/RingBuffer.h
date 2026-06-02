#ifndef RING_BUFFER_CONTROL_H
#define RING_BUFFER_CONTROL_H

#include <atomic>
#include <cstddef>
#include <cstdint>

#define NUM_BLOCKS 8

struct RingBufferControl {
  alignas(64) std::atomic<size_t> write_offset;
  std::atomic<size_t> read_offset; // Added for consumer flow control
  char *buffer_start;
  size_t buffer_size;
  std::atomic<uint32_t> blocks[NUM_BLOCKS]; // 0 = AVAILABLE, 1 = CHECKED_OUT
};

// Static assertions to verify layout assumptions
// We allow 32-bit or 64-bit platforms, but alignment and padding must ensure
// consistent 64-byte blocks.
static_assert(alignof(RingBufferControl) == 64,
              "RingBufferControl alignment expected to be 64");
static_assert(sizeof(RingBufferControl) == 64,
              "RingBufferControl size expected to be 64");

#endif
