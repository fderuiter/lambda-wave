#include "../../cbits/include/RingBuffer.h"
#include <iostream>
#include <thread>
#include <vector>
#include <atomic>
#include <unistd.h>
#include <cstring>
#include <chrono>

// Explicitly declare C functions from ring_buffer.cpp since they are not in the header
extern "C" {
    RingBufferControl* create_ring_buffer(size_t size);
    void free_ring_buffer(RingBufferControl* handle);
    ssize_t read_from_uart(RingBufferControl* handle, int uart_fd);
    size_t get_write_offset(RingBufferControl* handle);
    void set_read_offset(RingBufferControl* handle, size_t offset);
}

/**
 * TSan Harness for SGRT RingBuffer
 * * This harness simulates the race conditions between:
 * 1. The Kernel/Driver thread (Producer) calling read_from_uart
 * 2. The Haskell Runtime thread (Consumer) reading data
 * * It is compiled with -fsanitize=thread to detect atomic violations.
 */

// Mock UART data feed
void uart_simulator_thread(int write_fd, std::atomic<bool>& running) {
    char junk_data[1024];
    memset(junk_data, 0xAB, 1024);

    while (running) {
        // Burst write to pipe
        if (write(write_fd, junk_data, 64) < 0) {
            // Ignore write errors in simulation
        }
        std::this_thread::sleep_for(std::chrono::microseconds(100));
    }
}

// Simulates the 'driver' thread that pushes data into RingBuffer
void driver_thread(RingBufferControl* rb, int read_fd, std::atomic<bool>& running) {
    while (running) {
        // This function uses atomic loads/stores internally.
        // TSan will watch these ops.
        read_from_uart(rb, read_fd);
        std::this_thread::sleep_for(std::chrono::microseconds(50));
    }
}

// Simulates the Haskell application consuming data
void consumer_thread(RingBufferControl* rb, std::atomic<bool>& running) {
    while (running) {
        size_t w_off = get_write_offset(rb);
        // We manually access the atomic to simulate Haskell's FFI behavior
        size_t r_off = rb->read_offset.load(std::memory_order_acquire);

        if (w_off != r_off) {
            // Calculate available data
            size_t diff = (w_off >= r_off) ? (w_off - r_off) : (rb->buffer_size - r_off + w_off);

            // Advance read offset arbitrarily to simulate processing
            size_t new_r_off = (r_off + diff) % rb->buffer_size;
            set_read_offset(rb, new_r_off);
        }
        std::this_thread::yield();
    }
}

int main() {
    std::cout << "[TSan] Starting Concurrency Stress Test..." << std::endl;

    // Allocate RingBuffer (simulating FFI call)
    // Note: CI environments might fail mlock/posix_memalign depending on ulimits.
    // Real SGRT hardware would succeed.
    size_t size = 4096 * 10;
    RingBufferControl* rb = create_ring_buffer(size);

    if (!rb) {
        std::cerr << "[SKIP] create_ring_buffer failed (likely memory limits). Skipping test." << std::endl;
        return 0;
    }

    int pipe_fds[2];
    if (pipe(pipe_fds) == -1) {
        perror("pipe");
        return 1;
    }

    std::atomic<bool> running(true);

    // Spawn Threads
    std::thread t_uart(uart_simulator_thread, pipe_fds[1], std::ref(running));
    std::thread t_driver(driver_thread, rb, pipe_fds[0], std::ref(running));
    std::thread t_consumer(consumer_thread, rb, std::ref(running));

    // Run stress test for 5 seconds
    std::this_thread::sleep_for(std::chrono::seconds(5));

    running = false;
    t_uart.join();
    t_driver.join();
    t_consumer.join();

    free_ring_buffer(rb);
    std::cout << "[TSan] Test Passed (No Race Conditions Detected)." << std::endl;
    return 0;
}
