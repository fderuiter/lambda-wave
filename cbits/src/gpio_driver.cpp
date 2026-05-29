#include "gpio.h"
#include <atomic>
#include <csignal>
#include <cstdlib>

static std::atomic<int> g_pins[256];
static int g_watchdog_pin = -1;

extern "C" {

static void handle_fatal_signal(int sig) {
    if (g_watchdog_pin >= 0) {
        g_pins[g_watchdog_pin].store(0); // Hardware interlock safe state
    }
    std::exit(128 + sig);
}

int gpio_init() {
    for (int i = 0; i < 256; i++) {
        g_pins[i].store(0);
    }
    std::signal(SIGTERM, handle_fatal_signal);
    std::signal(SIGABRT, handle_fatal_signal);
    return 0;
}

int gpio_write(int pin, int val) {
    if (pin >= 0 && pin < 256) {
        g_pins[pin].store(val);
        return 0;
    }
    return -1;
}

int gpio_read(int pin) {
    if (pin >= 0 && pin < 256) {
        return g_pins[pin].load();
    }
    return -1;
}

int gpio_setup_watchdog(int pin) {
    if (pin >= 0 && pin < 256) {
        g_watchdog_pin = pin;
        g_pins[pin].store(1); // Default HIGH
        return 0;
    }
    return -1;
}

}
