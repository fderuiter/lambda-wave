#include "hud.h"
#include "../imgui/backends/imgui_impl_glfw.h"
#include "../imgui/backends/imgui_impl_opengl3.h"
#include "../imgui/imgui.h"
#include <a11y_bridge.h>
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <deque>
#include <iostream>
#include <mutex>
#include <string>
#include <unordered_map>
#include <vector>

static std::mutex g_state_mutex;
static std::vector<Point3DC> g_points;
static double g_resp_z = 0.0;
static std::string g_active_language = "en";
static std::string g_localized_beam_state = "BEAM OFF";
static int g_calibration_status = 0;
static float g_beam_color_r = 1.0f;
static float g_beam_color_g = 0.0f;
static float g_beam_color_b = 0.0f;
static float g_trace_scale_min = -20.0f;
static float g_trace_scale_max = 20.0f;
static float g_point_color_r = 0.0f;
static float g_point_color_g = 1.0f;
static float g_point_color_b = 1.0f;

static std::deque<float> g_resp_history;
static const size_t MAX_HISTORY = 300;

static TranslateCallback g_translate_callback = nullptr;
static std::unordered_map<std::string, std::string> g_translation_cache;

static std::deque<std::string> g_a11y_announcements;
static int g_last_focused_item = -1;

static void CheckFocus(int id, const char *name, const char *role) {
  if (ImGui::IsItemFocused()) {
    if (g_last_focused_item != id) {
      g_last_focused_item = id;
      A11y::SetFocus(name, role);
    }
  }
}

static const char *get_localized_string(const char *key_cstr,
                                        const char *default_val_cstr) {
  std::string key(key_cstr);
  auto it = g_translation_cache.find(key);
  if (it != g_translation_cache.end()) {
    return it->second.c_str();
  }
  if (g_translate_callback != nullptr) {
    const char *result =
        g_translate_callback(g_active_language.c_str(), key_cstr);
    if (result != nullptr) {
      std::string res_str(result);
      free((void *)result);
      g_translation_cache[key] = res_str;
      return g_translation_cache[key].c_str();
    }
  }
  g_translation_cache[key] = default_val_cstr;
  return g_translation_cache[key].c_str();
}

extern "C" {
void register_translate_callback(TranslateCallback callback) {
  std::lock_guard<std::mutex> lock(g_state_mutex);
  g_translate_callback = callback;
}

void set_cpp_hud_state(const HudStateC *state) {
  std::lock_guard<std::mutex> lock(g_state_mutex);
  g_points.assign(state->points, state->points + state->num_points);

  static size_t last_announced_points = 0;
  if (std::abs((long long)state->num_points -
               (long long)last_announced_points) >= 500) {
    std::string ann = std::string(get_localized_string("point_cloud_count",
                                                       "Point Cloud Count: ")) +
                      " " + std::to_string(state->num_points);
    g_a11y_announcements.push_back(ann);
    last_announced_points = state->num_points;
  }

  g_resp_z = state->resp_z;
  
  if (state->active_language && g_active_language != state->active_language) {
    g_active_language = state->active_language;
    g_translation_cache.clear();
  }

  bool beam_changed = false;
  std::string new_beam_state;
  if (state->localized_beam_state &&
      g_localized_beam_state != state->localized_beam_state) {
    g_localized_beam_state = state->localized_beam_state;
    beam_changed = true;
    new_beam_state = g_localized_beam_state;
  }

  bool cal_changed = false;
  int new_cal_status = 0;
  if (g_calibration_status != state->calibration_status) {
    g_calibration_status = state->calibration_status;
    cal_changed = true;
    new_cal_status = g_calibration_status;
  }

  if (beam_changed) {
    std::string ann = std::string(get_localized_string(
                          "beam_status_changed", "Beam Status Changed: ")) +
                      " " + new_beam_state;
    g_a11y_announcements.push_back(ann);
  }
  if (cal_changed) {
    std::string ann =
        new_cal_status == 1
            ? get_localized_string("calibration_valid", "Calibration Valid")
            : get_localized_string("calibration_invalid",
                                   "Calibration Invalid");
    g_a11y_announcements.push_back(ann);
  }

  g_beam_color_r = state->beam_color_r;
  g_beam_color_g = state->beam_color_g;
  g_beam_color_b = state->beam_color_b;
  g_trace_scale_min = state->trace_scale_min;
  g_trace_scale_max = state->trace_scale_max;
  g_point_color_r = state->point_color_r;
  g_point_color_g = state->point_color_g;
  g_point_color_b = state->point_color_b;

  g_resp_history.push_back(static_cast<float>(g_resp_z));
  if (g_resp_history.size() > MAX_HISTORY) {
    g_resp_history.pop_front();
  }
}
} // close extern "C"

extern "C" void get_cpp_hud_language(char *out_lang, size_t max_len) {
  std::lock_guard<std::mutex> lock(g_state_mutex);
  strncpy(out_lang, g_active_language.c_str(), max_len - 1);
  out_lang[max_len - 1] = '\0';
}

static void glfw_error_callback(int error, const char *description) {
  std::cerr << "GLFW Error " << error << ": " << description << '\n';
}

extern "C" void start_cpp_hud_loop(void) {
  // Requirement FR-UI-001
  // Requirement FR-UI-002
  // Requirement FR-UI-003
  glfwSetErrorCallback(glfw_error_callback);
  if (!glfwInit())
    return;

  const char *glsl_version = "#version 130";
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 0);

  GLFWwindow *window = glfwCreateWindow(1280, 720, "SGRT HUD", NULL, NULL);
  if (window == NULL)
    return;
  glfwMakeContextCurrent(window);
  glfwSwapInterval(1); // Enable vsync

  if (glewInit() != GLEW_OK) {
    std::cerr << "Failed to initialize OpenGL loader!\n";
    return;
  }

  IMGUI_CHECKVERSION();
  ImGui::CreateContext();
  ImGuiIO &io = ImGui::GetIO();
  (void)io;
  io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;
  ImGui::StyleColorsDark();

  ImGui_ImplGlfw_InitForOpenGL(window, true);
  ImGui_ImplOpenGL3_Init(glsl_version);

  A11y::Init();

  bool logged_in = false;
  char username[128] = "";
  char password[128] = "";

  while (!glfwWindowShouldClose(window)) {
    glfwPollEvents();

    ImGui_ImplOpenGL3_NewFrame();
    ImGui_ImplGlfw_NewFrame();
    ImGui::NewFrame();

    if (!logged_in) {
      std::lock_guard<std::mutex> lock(g_state_mutex);
      ImGui::Begin(get_localized_string("login_title", "Authentication"), NULL,
                   ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoResize |
                       ImGuiWindowFlags_AlwaysAutoResize);

      if (ImGui::Button("EN")) {
        g_active_language = "en";
        g_translation_cache.clear();
      }
      CheckFocus(1, "English", "button");
      ImGui::SameLine();
      if (ImGui::Button("ES")) {
        g_active_language = "es";
        g_translation_cache.clear();
      }
      CheckFocus(2, "Spanish", "button");
      ImGui::SameLine();
      if (ImGui::Button("FR")) {
        g_active_language = "fr";
        g_translation_cache.clear();
      }
      CheckFocus(3, "French", "button");
      ImGui::Separator();

      const char *user_label =
          get_localized_string("username_label", "Username");
      ImGui::InputText(user_label, username, IM_ARRAYSIZE(username));
      CheckFocus(4, user_label, "textbox");
      const char *pass_label =
          get_localized_string("password_label", "Password");
      bool submit_password = ImGui::InputText(
          pass_label, password, IM_ARRAYSIZE(password),
          ImGuiInputTextFlags_Password | ImGuiInputTextFlags_EnterReturnsTrue);
      CheckFocus(5, pass_label, "textbox");
      const char *login_label = get_localized_string("login_button", "Login");
      if (ImGui::Button(login_label) || submit_password) {
        if ((strcmp(username, "admin") == 0 ||
             strcmp(username, "operator") == 0) &&
            strcmp(password, "password") == 0) {
          logged_in = true;
          g_a11y_announcements.push_back(
              get_localized_string("login_success", "Login Successful"));
        }
      }
      CheckFocus(6, login_label, "button");
      ImGui::End();
    } else {
      std::lock_guard<std::mutex> lock(g_state_mutex);

      // Dashboard Window
      ImGui::Begin("SGRT Monitoring HUD");
      ImGui::Text("Active Language: %s", g_active_language.c_str());
      if (ImGui::Button("EN")) {
        g_active_language = "en";
        g_translation_cache.clear();
        g_a11y_announcements.push_back(get_localized_string(
            "lang_changed", "Language changed to English"));
      }
      CheckFocus(7, "English", "button");
      ImGui::SameLine();
      if (ImGui::Button("ES")) {
        g_active_language = "es";
        g_translation_cache.clear();
        g_a11y_announcements.push_back(
            get_localized_string("lang_changed", "Idioma cambiado a Español"));
      }
      CheckFocus(8, "Spanish", "button");
      ImGui::SameLine();
      if (ImGui::Button("FR")) {
        g_active_language = "fr";
        g_translation_cache.clear();
        g_a11y_announcements.push_back(
            get_localized_string("lang_changed", "Langue changée en Français"));
      }
      CheckFocus(9, "French", "button");

      ImGui::Text("%s%s",
                  get_localized_string("calibration_status_prefix",
                                       "Calibration Status: "),
                  g_calibration_status == 1
                      ? get_localized_string("calibration_valid", "Valid")
                      : get_localized_string("calibration_invalid", "Invalid"));

      ImVec4 beamColor =
          ImVec4(g_beam_color_r, g_beam_color_g, g_beam_color_b, 1.0f);

      ImGui::TextColored(beamColor, "STATUS: %s",
                         g_localized_beam_state.c_str());

      // Respiratory trace
      ImGui::PlotLines(
          get_localized_string("resp_trace_title", "Respiratory Trace"),
          [](void *data, int idx) {
            return (*static_cast<std::deque<float> *>(data))[idx];
          },
          &g_resp_history, g_resp_history.size(), 0, NULL, g_trace_scale_min,
          g_trace_scale_max, ImVec2(0, 100));

      // Point Cloud Info
      ImGui::Text("Active Points: %zu", g_points.size());

      while (!g_a11y_announcements.empty()) {
        A11y::Announce(g_a11y_announcements.front());
        g_a11y_announcements.pop_front();
      }

      ImGui::End();

      // Render 3D Point cloud natively in OpenGL below ImGui
      int display_w, display_h;
      glfwGetFramebufferSize(window, &display_w, &display_h);
      glViewport(0, 0, display_w, display_h);
      glClearColor(0.1f, 0.1f, 0.1f, 1.0f);
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

      glMatrixMode(GL_PROJECTION);
      glLoadIdentity();
      float ratio = display_w / (float)display_h;
      // simple perspective
      float fov = 45.0f;
      float near_plane = 0.1f, far_plane = 100.0f;
      float top = std::tan(fov * 3.14159f / 360.0f) * near_plane;
      float bottom = -top;
      float right = top * ratio;
      float left = -right;
      glFrustum(left, right, bottom, top, near_plane, far_plane);

      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity();
      // LookAt roughly: pos=(0, 2, -2), target=(0,0,2), up=(0,1,0)
      glTranslatef(0.0f, -1.0f, -5.0f); // basic positioning

      glPointSize(2.0f);
      glBegin(GL_POINTS);
      glColor3f(g_point_color_r, g_point_color_g, g_point_color_b);
      for (const auto &pt : g_points) {
        glVertex3f(pt.x, pt.y, pt.z);
      }
      glEnd();
    }

    ImGui::Render();
    if (logged_in) {
      // we already cleared, just render imgui on top
    } else {
      std::lock_guard<std::mutex> lock(g_state_mutex);
      while (!g_a11y_announcements.empty()) {
        A11y::Announce(g_a11y_announcements.front());
        g_a11y_announcements.pop_front();
      }
      int display_w, display_h;
      glfwGetFramebufferSize(window, &display_w, &display_h);
      glViewport(0, 0, display_w, display_h);
      glClearColor(0.2f, 0.2f, 0.2f, 1.0f);
      glClear(GL_COLOR_BUFFER_BIT);
    }
    ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());
    glfwSwapBuffers(window);
  }

  ImGui_ImplOpenGL3_Shutdown();
  ImGui_ImplGlfw_Shutdown();
  ImGui::DestroyContext();
  A11y::Shutdown();
  glfwDestroyWindow(window);
  glfwTerminate();
}
