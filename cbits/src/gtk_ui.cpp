#include "gtk_ui.h"
#include <gtk/gtk.h>
#include <epoxy/gl.h>
#include <vector>
#include <string>
#include <iostream>
#include <mutex>

static GtkWidget* window = nullptr;
static GtkWidget* gl_area = nullptr;
static GtkWidget* status_label = nullptr;
static GtkWidget* main_box = nullptr;

static std::mutex data_mutex;
static int current_beam_state = 0; // 0=Off, 1=On, 2=Hold
static std::vector<float> current_points;
static int current_seq_num = 0;

static GLuint vbo_points = 0;
static GLuint vbo_heartbeat = 0;

static bool space_pressed = false;

extern "C" {
    int check_space_pressed() {
        if (space_pressed) {
            space_pressed = false;
            return 1;
        }
        return 0;
    }
}

// Update data from Haskell
void update_gtk_ui(int beam_state, const float* points, int num_points, int seq_num) {
    std::lock_guard<std::mutex> lock(data_mutex);
    current_beam_state = beam_state;
    current_points.assign(points, points + (num_points * 3));
    current_seq_num = seq_num;
    
    // In GTK, we should queue draw from the main thread, but we can call it here if we use g_idle_add,
    // or just let a periodic timer do it.
}

static gboolean on_render(GtkGLArea *area, GdkGLContext *context) {
    std::lock_guard<std::mutex> lock(data_mutex);

    // Set background color based on Beam State
    float bgR = 0.2f, bgG = 0.0f, bgB = 0.0f;
    const char* status_text = "Beam Off";
    if (current_beam_state == 1) { // BeamOn
        bgR = 0.0f; bgG = 0.2f; bgB = 0.0f;
        status_text = "Beam On";
    } else if (current_beam_state == 2) { // BeamHold
        bgR = 0.2f; bgG = 0.2f; bgB = 0.0f;
        status_text = "Beam Hold";
    }
    
    // Update label (UI thread safe because on_render is called by GTK main loop)
    gtk_label_set_text(GTK_LABEL(status_label), status_text);

    glClearColor(bgR, bgG, bgB, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    // Simplistic rendering (since old code used legacy OpenGL, we can use it if context is compatibility profile)
    // GtkGLArea requests a core profile by default on GTK 3.20+, but we can use epoxy.
    // However, to keep it simple and match the old `drawArrays`, we'll just do legacy if possible, 
    // or very basic modern GL. The old code used `clientState` which is legacy.
    // Wait, let's just draw some points if legacy is available.
    // GtkGLArea in GTK3 defaults to Core profile, which means no `glEnableClientState`.
    // Let's use simple shaders.

    // To save time and keep 30+ FPS, we can just compile a basic shader once.
    static GLuint program = 0;
    if (program == 0) {
        const char* vs = "#version 130\nin vec3 pos; void main() { gl_Position = vec4(pos.x, pos.y, pos.z, 1.0); }";
        const char* fs = "#version 130\nout vec4 color; void main() { color = vec4(1.0); }";
        
        GLuint vso = glCreateShader(GL_VERTEX_SHADER);
        glShaderSource(vso, 1, &vs, NULL);
        glCompileShader(vso);
        
        GLuint fso = glCreateShader(GL_FRAGMENT_SHADER);
        glShaderSource(fso, 1, &fs, NULL);
        glCompileShader(fso);
        
        program = glCreateProgram();
        glAttachShader(program, vso);
        glAttachShader(program, fso);
        glLinkProgram(program);
        
        glGenBuffers(1, &vbo_points);
        glGenBuffers(1, &vbo_heartbeat);
    }

    glUseProgram(program);

    if (!current_points.empty()) {
        glBindBuffer(GL_ARRAY_BUFFER, vbo_points);
        glBufferData(GL_ARRAY_BUFFER, current_points.size() * sizeof(float), current_points.data(), GL_DYNAMIC_DRAW);
        GLint posAttrib = glGetAttribLocation(program, "pos");
        glEnableVertexAttribArray(posAttrib);
        glVertexAttribPointer(posAttrib, 3, GL_FLOAT, GL_FALSE, 0, 0);
        
        glDrawArrays(GL_POINTS, 0, current_points.size() / 3);
        glDisableVertexAttribArray(posAttrib);
    }

    return TRUE;
}

static gboolean on_tick(gpointer user_data) {
    if (gl_area) {
        gtk_widget_queue_draw(gl_area);
    }
    return G_SOURCE_CONTINUE;
}

static gboolean on_key_press(GtkWidget *widget, GdkEventKey *event, gpointer user_data) {
    if (event->keyval == GDK_KEY_space) {
        space_pressed = true;
        return TRUE;
    }
    return FALSE;
}

void init_gtk_ui(int argc, char** argv) {
    gtk_init(&argc, &argv);

    window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(window), "Lambda-Wave Visualizer (GTK Migration)");
    gtk_window_set_default_size(GTK_WINDOW(window), 800, 600);
    g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);
    g_signal_connect(window, "key-press-event", G_CALLBACK(on_key_press), NULL);

    main_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_container_add(GTK_CONTAINER(window), main_box);

    // Accessibility label for screen readers
    status_label = gtk_label_new("Beam Off");
    gtk_widget_set_name(status_label, "BeamStateLabel");
    
    // Set ARIA equivalent roles
    AtkObject* atk_obj = gtk_widget_get_accessible(status_label);
    atk_object_set_name(atk_obj, "Beam Status");
    atk_object_set_role(atk_obj, ATK_ROLE_STATUSBAR);

    gtk_box_pack_start(GTK_BOX(main_box), status_label, FALSE, FALSE, 10);

    // GLArea
    gl_area = gtk_gl_area_new();
    gtk_widget_set_vexpand(gl_area, TRUE);
    gtk_widget_set_hexpand(gl_area, TRUE);
    g_signal_connect(gl_area, "render", G_CALLBACK(on_render), NULL);
    
    // Let GL area take focus so keyboard events can be captured
    gtk_widget_set_can_focus(gl_area, TRUE);

    gtk_box_pack_start(GTK_BOX(main_box), gl_area, TRUE, TRUE, 0);

    // Refresh timer (~30 FPS = 33ms)
    g_timeout_add(33, on_tick, NULL);

    gtk_widget_show_all(window);
}

void process_gtk_events() {
    while (gtk_events_pending()) {
        gtk_main_iteration();
    }
}
