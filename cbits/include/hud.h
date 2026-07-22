// Requirement FR-UI-001, FR-UI-002, FR-UI-003
#pragma once
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef const char *(*TranslateCallback)(const char *lang, const char *key);
void register_translate_callback(TranslateCallback callback);

typedef struct {
  double x, y, z;
} Point3DC;

typedef struct {
  int beam_state;  // 0=Off, 1=On, 2=Hold
  Point3DC *points;
  size_t num_points;
  double resp_z;  // from kalman state
  bool audio_alert_enabled;
  int calibration_status;
  float beam_color_r;
  float beam_color_g;
  float beam_color_b;
  float trace_scale_min;
  float trace_scale_max;
  float point_color_r;
  float point_color_g;
  float point_color_b;
} HudStateC;

void set_cpp_hud_state(const HudStateC *state);
void start_cpp_hud_loop(void);

#ifdef __cplusplus
}
#endif
