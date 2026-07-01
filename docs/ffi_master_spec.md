# FFI Master Specification

This document serves as the master specification for all hardware-software interfaces in the system. It defines memory layouts, functional contracts, error code mappings, and their corresponding audit events.

## FFI Memory Layouts

### RingBufferControl
The ring buffer is implemented via POSIX shared memory mapped into process space (`/sgrt_ring_buffer`).
It uses a zero-copy lock-free design using `std::atomic` variables for indices.

**Memory Structure**:
- `std::atomic<size_t> write_offset` (8 bytes)
- `std::atomic<size_t> read_offset` (8 bytes)
- `size_t buffer_offset` (8 bytes)
- `size_t buffer_size` (8 bytes)
- `char[] data_region` (starts at `buffer_offset`, spans `buffer_size` bytes)

*Constraint*: The consumer (Haskell) must dynamically resolve the buffer start address using `buffer_offset` relative to the handle to support multi-process address spaces securely.

### GPIO Memory Mapping
GPIO operations map `/dev/gpiomem` (or fallback to `/dev/mem` for legacy compatibility) to a `volatile uint32_t*` in process space to interact with Broadcom BCM2708 peripherals directly, bypassing kernel latency on fast paths.
- `BLOCK_SIZE` is fixed at 4KB.
- Pins are state-mirrored using `std::atomic<int> g_pins[256]` for software-side observability.

## Functional Contracts & Audit Integrity
Hardware errors must trigger explicit audit events. The bridge layer translates integer return codes to strongly typed `HardwareResult`s, which in turn are logged by the `auditHardwareEvent` function with specific severity levels.

```yaml
---
ffi_functions:
  read_from_uart:
    module: cbits/src/ring_buffer.cpp
    return_codes:
      ">0":
        description: Success (bytes read)
        hardware_result: PartialData
        audit_event: Info
      "0":
        description: No contiguous space
        hardware_result: TransientError
        audit_event: Warning
      "-1":
        description: Invalid buffer handle / POSIX read error
        hardware_result: SystemError
        audit_event: Critical
      "-2":
        description: End-of-file
        hardware_result: EOF
        audit_event: Info
      "-3":
        description: Non-blocking error (EAGAIN/EWOULDBLOCK)
        hardware_result: TransientError
        audit_event: Warning

  configure_serial_port:
    module: cbits/src/serial_config.cpp
    return_codes:
      "0":
        description: Success
        hardware_result: Success
        audit_event: Info
      "-1":
        description: Failed POSIX tcgetattr/tcsetattr
        hardware_result: SystemError
        audit_event: Critical
      "-2":
        description: Unsupported/invalid baud rate
        hardware_result: DriverError
        audit_event: Critical

  gpio_init:
    module: cbits/src/gpio_driver.cpp
    return_codes:
      "0":
        description: Success
        hardware_result: Success
        audit_event: Info

  gpio_write:
    module: cbits/src/gpio_driver.cpp
    return_codes:
      "0":
        description: Success
        hardware_result: Success
        audit_event: Info
      "-1":
        description: Out-of-bounds pin request
        hardware_result: SystemError
        audit_event: Critical

  gpio_read:
    module: cbits/src/gpio_driver.cpp
    return_codes:
      "0":
        description: Success (Low)
        hardware_result: Success
        audit_event: Info
      "1":
        description: Success (High)
        hardware_result: Success
        audit_event: Info
      "-1":
        description: Out-of-bounds pin request
        hardware_result: SystemError
        audit_event: Critical

  gpio_setup_watchdog:
    module: cbits/src/gpio_driver.cpp
    return_codes:
      "0":
        description: Success
        hardware_result: Success
        audit_event: Info
      "-1":
        description: Out-of-bounds pin request
        hardware_result: SystemError
        audit_event: Critical

  get_write_offset:
    module: cbits/src/ring_buffer_ffi.cpp
    return_codes:
      "0":
        description: Returns 0 if RingBufferControl is null
        hardware_result: Success
        audit_event: Info

  set_write_offset:
    module: cbits/src/ring_buffer_ffi.cpp
    return_codes:
      "0":
        description: Aborts early if RingBufferControl is null (void return)
        hardware_result: Success
        audit_event: Info

  get_read_offset:
    module: cbits/src/ring_buffer_ffi.cpp
    return_codes:
      "0":
        description: Returns 0 if RingBufferControl is null
        hardware_result: Success
        audit_event: Info

  set_read_offset:
    module: cbits/src/ring_buffer_ffi.cpp
    return_codes:
      "0":
        description: Aborts early if RingBufferControl is null (void return)
        hardware_result: Success
        audit_event: Info
...
```
