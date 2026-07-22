/// Hardware GPIO Driver
///
/// Manages the low-level physical pin mapping and watchdog interlocks for the
/// SGRT hardware.
///
/// Failure Modes:
/// * Unexpected physical pin state transitions leading to hardware damage.
/// * Watchdog failure due to software lockup or memory map corruption.
///
/// Mitigations:
/// * Atomic pin state tracking and hardware interlocks for the watchdog.
/// * Automatic safe-state transition on fatal signals (SIGTERM, SIGSEGV).
///
/// Traceability:
/// * Requirement FR-DAQ-002: Hardware safety interlocks
/// * Hazard H-HW-001: Uncontrolled pin state

#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>

#include <atomic>
#include <csignal>
#include <cstdlib>

#include "gpio.h"

static std::atomic<int> g_pins[256];
static int g_watchdog_pin = -1;

static volatile uint32_t *gpio_map = nullptr;

#define BCM2708_PERI_BASE 0x3F000000
#define GPIO_BASE (BCM2708_PERI_BASE + 0x200000)
#define BLOCK_SIZE (4 * 1024)

extern "C" {

static void clear_watchdog_safe_state() {
  if (g_watchdog_pin >= 0) {
    if (gpio_map != nullptr && g_watchdog_pin < 54) {
      int pin = g_watchdog_pin;
      *(gpio_map + 10 + pin / 32) = (1 << (pin % 32));  // GPCLRn is offset 10
    }
    g_pins[g_watchdog_pin].store(0);  // Hardware interlock safe state
  }
}

static void handle_fatal_signal(int sig) {
  clear_watchdog_safe_state();
  std::exit(128 + sig);
}

int gpio_init() {
  for (int i = 0; i < 256; i++) {
    g_pins[i].store(0);
  }

  int mem_fd = open("/dev/gpiomem", O_RDWR | O_SYNC);
  if (mem_fd < 0) {
    mem_fd = open("/dev/mem", O_RDWR | O_SYNC);
    if (mem_fd >= 0) {
      gpio_map =
          (volatile uint32_t *)mmap(NULL, BLOCK_SIZE, PROT_READ | PROT_WRITE,
                                    MAP_SHARED, mem_fd, GPIO_BASE);
    }
  } else {
    gpio_map = (volatile uint32_t *)mmap(
        NULL, BLOCK_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, mem_fd, 0);
  }

  if (mem_fd >= 0) {
    close(mem_fd);  // mmap keeps it open
  }

  if (gpio_map == MAP_FAILED || gpio_map == nullptr) {
    gpio_map = nullptr;
    return 2;  // Simulation Mode
  }

  std::atexit(clear_watchdog_safe_state);
  std::signal(SIGTERM, handle_fatal_signal);
  std::signal(SIGABRT, handle_fatal_signal);
  std::signal(SIGINT, handle_fatal_signal);
  std::signal(SIGSEGV, handle_fatal_signal);
  return 0;
}

int gpio_write(int pin, int val) {
  if (pin >= 0 && pin < 256) {
    g_pins[pin].store(val);
    if (gpio_map != nullptr && pin < 54) {
      // GPSETn: 7, GPCLRn: 10
      if (val) {
        *(gpio_map + 7 + pin / 32) = (1 << (pin % 32));
      } else {
        *(gpio_map + 10 + pin / 32) = (1 << (pin % 32));
      }
    }
    return 0;
  }
  return -1;
}

int gpio_read(int pin) {
  if (pin >= 0 && pin < 256) {
    if (gpio_map != nullptr && pin < 54) {
      // GPLEVn: 13
      uint32_t val = *(gpio_map + 13 + pin / 32);
      return (val & (1 << (pin % 32))) ? 1 : 0;
    }
    return g_pins[pin].load();
  }
  return -1;
}

int gpio_setup_watchdog(int pin) {
  if (pin >= 0 && pin < 256) {
    g_watchdog_pin = pin;

    if (gpio_map != nullptr && pin < 54) {
      // Setup pin as output (GPFSELn)
      int fsel = pin / 10;
      int shift = (pin % 10) * 3;
      uint32_t val = *(gpio_map + fsel);
      val &= ~(7 << shift);  // clear
      val |= (1 << shift);   // output
      *(gpio_map + fsel) = val;
    }

    gpio_write(pin, 1);
    return 0;
  }
  return -1;
}
}
