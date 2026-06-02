#ifndef RING_BUFFER_CONTROL_H
#define RING_BUFFER_CONTROL_H

#include <atomic>
#include <cstddef>
#include <cstdint>

constexpr size_t NUM_BLOCKS = 4;

enum class BlockState : uint32_t {
    AVAILABLE = 0,
    WRITING = 1,
    READY = 2,
    CHECKED_OUT = 3
};

struct RingBufferControl {
  alignas(64) std::atomic<uint32_t> block_states[NUM_BLOCKS];
  uint32_t block_bytes_written[NUM_BLOCKS];
  char *buffer_start;
  size_t buffer_size;
  size_t current_write_block;
  size_t current_write_offset;
};

// Static assertions to verify layout assumptions
// We allow 32-bit or 64-bit platforms, but alignment and padding must ensure
// consistent 64-byte blocks.
static_assert(alignof(RingBufferControl) == 64,
              "RingBufferControl alignment expected to be 64");
static_assert(sizeof(RingBufferControl) == 64,
              "RingBufferControl size expected to be 64");

#endif
