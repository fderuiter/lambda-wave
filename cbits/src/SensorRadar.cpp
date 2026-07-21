#include "SensorRadar.h"

extern "C" {
void *c_create_radar() { return nullptr; }
void c_destroy_radar(void *ptr) { (void)ptr; }
void *c_attach_radar(void *existing_ptr) { return existing_ptr; }
}
