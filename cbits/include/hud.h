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
  int beam_state; // 0=Off, 1=On, 2=Hold
  Point3DC *points;
  size_t num_points;
  double resp_z; // from kalman state
  bool audio_alert_enabled;
  const char *active_language;
  const char *localized_beam_state;
  int calibration_status;
} HudStateC;

void set_cpp_hud_state(const HudStateC *state);
void get_cpp_hud_language(char *out_lang, size_t max_len);
void start_cpp_hud_loop(void);

#ifdef __cplusplus
}
#endif
