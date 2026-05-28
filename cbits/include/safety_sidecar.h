#pragma once
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Returns 0 on success.
int start_safety_sidecar(void);

// Stops the sidecar.
void stop_safety_sidecar(void);

// Updates the heartbeat atomic.
void update_heartbeat(uint64_t timestamp_ns);

#ifdef __cplusplus
}
#endif
