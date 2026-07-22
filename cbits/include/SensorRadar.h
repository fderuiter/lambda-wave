#ifndef SENSOR_RADAR_H
#define SENSOR_RADAR_H

#ifdef __cplusplus
extern "C" {
#endif

void* c_create_radar();
void c_destroy_radar(void* ptr);
void* c_attach_radar(void* existing_ptr);

#ifdef __cplusplus
}
#endif

#endif
