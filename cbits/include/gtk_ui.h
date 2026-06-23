#ifndef GTK_UI_H
#define GTK_UI_H

#ifdef __cplusplus
extern "C" {
#endif

// BeamState mapping: 0 = BeamOff, 1 = BeamOn, 2 = BeamHold
void init_gtk_ui(int argc, char** argv);
void update_gtk_ui(int beam_state, const float* points, int num_points, int seq_num);
void process_gtk_events();

#ifdef __cplusplus
}
#endif

#endif
