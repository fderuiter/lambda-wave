#ifndef RING_BUFFER_CONTROL_H
#define RING_BUFFER_CONTROL_H

#include <atomic>
#include <cstddef>
#include <cstdint>

#define BUFFER_GAP 1

struct RingBufferControl {
  alignas(64) std::atomic<size_t> write_offset;
  std::atomic<size_t> read_offset;
  size_t buffer_offset;
  size_t buffer_size;
};

static_assert(alignof(RingBufferControl) == 64,
              "RingBufferControl alignment expected to be 64");
static_assert(sizeof(RingBufferControl) == 64,
              "RingBufferControl size expected to be 64");

extern "C" {
size_t get_write_offset(RingBufferControl* handle);
void set_write_offset(RingBufferControl* handle, size_t val);
size_t get_read_offset(RingBufferControl* handle);
void set_read_offset(RingBufferControl* handle, size_t val);
size_t calculate_available_read_bytes(size_t read_offset, size_t write_offset, size_t buffer_size);
size_t calculate_next_read_offset(size_t read_offset, size_t consumed, size_t buffer_size);
}
#endif
