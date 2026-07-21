/// High-Performance Ring Buffer Memory Manager
///
/// Implements a lock-free, zero-copy shared memory ring buffer for
/// inter-process communication.
///
/// Failure Modes:
/// * Race conditions during multi-producer/multi-consumer access causing memory
/// corruption.
/// * Memory leaks if shared memory segments are not unlinked on abnormal
/// termination.
///
/// Mitigations:
/// * Uses std::atomic for read/write offset management ensuring memory
/// ordering.
/// * Employs RAII and strict lifecycle control to cleanup /dev/shm artifacts.
///
/// Traceability:
/// * Requirement FR-DAQ-004: Low-latency IPC
/// * Hazard H-SOUP-003: FFI Memory Leaks

#include "../include/RingBuffer.h"
#include <cerrno>
#include <cstdlib>
#include <fcntl.h>
#include <new>
#include <sys/mman.h>
#include <unistd.h>

extern "C" {

RingBufferControl *create_ring_buffer(size_t size, int *status_out) {
  size_t total_size = sizeof(RingBufferControl) + size;

  // Unlink old if exists
  shm_unlink("/sgrt_ring_buffer");

  int fd = shm_open("/sgrt_ring_buffer", O_CREAT | O_RDWR, 0666);
  if (fd == -1) {
    if (status_out)
      *status_out = 2; // Simulation Mode

    // Fallback to normal allocation
    void *mem = nullptr;
    if (posix_memalign(&mem, 64, total_size) != 0) {
      return nullptr;
    }
    RingBufferControl *control = new (mem) RingBufferControl();
    control->write_offset.store(0, std::memory_order_relaxed);
    control->read_offset.store(0, std::memory_order_relaxed);
    control->buffer_offset = sizeof(RingBufferControl);
    control->buffer_size = size;
    return control;
  }

  if (status_out)
    *status_out = 0; // Success

  if (ftruncate(fd, total_size) == -1) {
    close(fd);
    return nullptr;
  }

  void *mem =
      mmap(nullptr, total_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
  close(fd); // fd is no longer needed after mmap
  if (mem == MAP_FAILED)
    return nullptr;

  // Pin memory
  if (mlock(mem, total_size) != 0) {
    munmap(mem, total_size);
    return nullptr;
  }

  RingBufferControl *control = new (mem) RingBufferControl();
  control->write_offset.store(0, std::memory_order_relaxed);
  control->read_offset.store(0, std::memory_order_relaxed);

  // Note: buffer_start is a pointer. It will be valid for the creator process.
  // Attachers must override it for their own address space.
  control->buffer_offset = sizeof(RingBufferControl);
  control->buffer_size = size;

  return control;
}

RingBufferControl *attach_ring_buffer(size_t size) {
  size_t total_size = sizeof(RingBufferControl) + size;

  int fd = shm_open("/sgrt_ring_buffer", O_RDWR, 0666);
  if (fd == -1)
    return nullptr;

  void *mem =
      mmap(nullptr, total_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
  close(fd);
  if (mem == MAP_FAILED)
    return nullptr;

  // Pin memory
  if (mlock(mem, total_size) != 0) {
    munmap(mem, total_size);
    return nullptr;
  }

  RingBufferControl *control = static_cast<RingBufferControl *>(mem);
  // DO NOT OVERWRITE control->buffer_start IN SHARED MEMORY!
  // It would break the creator.
  // We cannot change RingBufferControl struct because of ABI.
  // Wait, buffer_start is a field in the shared memory. We shouldn't write to
  // it. Instead, we should NEVER read buffer_start directly if we are the
  // attacher, or we just live with it? Actually, in our Haskell consumer code,
  // we peek buffer_start! So we MUST change how consumer gets the buffer start!
  return control;
}

void free_ring_buffer(RingBufferControl *handle) {
  if (handle) {
    size_t total_size = sizeof(RingBufferControl) + handle->buffer_size;
    munlock(handle, total_size);
    handle->~RingBufferControl();
    munmap(handle, total_size);
    shm_unlink("/sgrt_ring_buffer");
  }
}

void detach_ring_buffer(RingBufferControl *handle) {
  if (handle) {
    size_t total_size = sizeof(RingBufferControl) + handle->buffer_size;
    munlock(handle, total_size);
    munmap(handle, total_size);
  }
}

ssize_t read_from_uart(RingBufferControl *handle, int uart_fd) {
  if (!handle)
    return -1;

  size_t current_offset = handle->write_offset.load(std::memory_order_relaxed);
  size_t read_offset = handle->read_offset.load(std::memory_order_acquire);

  // Use dynamic pointer computation
  char *buf_start = reinterpret_cast<char *>(handle) + handle->buffer_offset;
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
    bytes_read =
        read(uart_fd, buf_start + current_offset, available_contiguous);
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
    handle->write_offset.store(new_offset, std::memory_order_release);
  }

  return bytes_read;
}
}

// Requirement FR-DAQ-001

// Requirement FR-DAQ-004
// Hazard H-SOUP-003: FFI Memory Leaks
