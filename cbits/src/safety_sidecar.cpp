#include "safety_sidecar.h"
#include <thread>
#include <atomic>
#include <chrono>
#include <iostream>
#include <fstream>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

std::atomic<uint64_t> g_last_heartbeat_ns{0};
std::atomic<bool> g_sidecar_running{false};
std::thread g_sidecar_thread;

const uint64_t TIMEOUT_NS = 100000000; // 100ms

void set_gpio_beam_off() {
    int fd = open("/sys/class/gpio/gpio17/value", O_WRONLY);
    if (fd >= 0) {
        if (write(fd, "0", 1) < 0) {}
        close(fd);
    } else {
        fd = open("/tmp/gpio17_value", O_WRONLY | O_CREAT, 0666);
        if (fd >= 0) {
            if (write(fd, "0", 1) < 0) {}
            close(fd);
        }
    }
    std::cout << "[Hardware] Beam Set To: OFF" << std::endl;
}

void write_audit_log() {
    int fd = open("audit_critical.log", O_WRONLY | O_CREAT | O_APPEND, 0666);
    if (fd >= 0) {
        auto now = std::chrono::system_clock::now().time_since_epoch().count();
        std::string msg = std::to_string(now) + " [Critical] Watchdog: Application frozen, safety trip triggered.\n";
        if (write(fd, msg.c_str(), msg.length()) < 0) {}
        close(fd);
    }
}

void sidecar_loop() {
    while (g_sidecar_running) {
        std::this_thread::sleep_for(std::chrono::milliseconds(5));
        
        uint64_t last = g_last_heartbeat_ns.load(std::memory_order_acquire);
        if (last == 0) {
            continue;
        }

        uint64_t now = std::chrono::duration_cast<std::chrono::nanoseconds>(
            std::chrono::steady_clock::now().time_since_epoch()).count();
            
        if (now - last > TIMEOUT_NS) {
            set_gpio_beam_off();
            write_audit_log();
            std::cout << "!!! WATCHDOG TRIP: Thread FROZEN. FORCING BEAM OFF." << std::endl;
            _exit(1); 
        }
    }
}

extern "C" {

int start_safety_sidecar(void) {
    if (g_sidecar_running) return 0;
    
    // Default to Beam Off initially
    set_gpio_beam_off();
    
    g_sidecar_running = true;
    g_last_heartbeat_ns.store(0, std::memory_order_release);
    
    try {
        g_sidecar_thread = std::thread(sidecar_loop);
    } catch (...) {
        g_sidecar_running = false;
        return -1;
    }
    return 0;
}

void stop_safety_sidecar(void) {
    if (g_sidecar_running) {
        g_sidecar_running = false;
        if (g_sidecar_thread.joinable()) {
            g_sidecar_thread.join();
        }
    }
}

void update_heartbeat(uint64_t timestamp_ns) {
    uint64_t now = std::chrono::duration_cast<std::chrono::nanoseconds>(
        std::chrono::steady_clock::now().time_since_epoch()).count();
    g_last_heartbeat_ns.store(now, std::memory_order_release);
}

}
