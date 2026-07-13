#include "hud.h"
#include "../imgui/backends/imgui_impl_glfw.h"
#include "../imgui/backends/imgui_impl_opengl3.h"
#include "../imgui/imgui.h"
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
static int g_beam_state = 0;
static std::vector<Point3DC> g_points;
static double g_resp_z = 0.0;
static bool g_audio_alert_enabled = false;
static std::string g_active_language = "en";
static std::string g_localized_beam_state = "BEAM OFF";
static int g_calibration_status = 0;

static std::deque<float> g_resp_history;
static const size_t MAX_HISTORY = 300;

static TranslateCallback g_translate_callback = nullptr;
static std::unordered_map<std::string, std::string> g_translation_cache;

static const char *get_localized_string(const std::string &key,
                                        const std::string &default_val) {
  auto it = g_translation_cache.find(key);
  if (it != g_translation_cache.end()) {
    return it->second.c_str();
  }
  if (g_translate_callback != nullptr) {
    const char *result =
        g_translate_callback(g_active_language.c_str(), key.c_str());
    if (result != nullptr) {
      std::string res_str(result);
      free((void *)result);
      g_translation_cache[key] = res_str;
      return g_translation_cache[key].c_str();
    }
  }
  g_translation_cache[key] = default_val;
  return g_translation_cache[key].c_str();
}

extern "C" {
void register_translate_callback(TranslateCallback callback) {
  std::lock_guard<std::mutex> lock(g_state_mutex);
  g_translate_callback = callback;
}

void set_cpp_hud_state(const HudStateC *state) {
  std::lock_guard<std::mutex> lock(g_state_mutex);
  g_beam_state = state->beam_state;
  g_points.assign(state->points, state->points + state->num_points);
  g_resp_z = state->resp_z;
  g_audio_alert_enabled = state->audio_alert_enabled;
  // We don't overwrite g_active_language from state anymore, it's managed by UI
  if (state->localized_beam_state)
    g_localized_beam_state = state->localized_beam_state;
  g_calibration_status = state->calibration_status;

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
  std::cerr << "GLFW Error " << error << ": " << description << std::endl;
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
    std::cerr << "Failed to initialize OpenGL loader!" << std::endl;
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
      ImGui::SameLine();
      if (ImGui::Button("ES")) {
        g_active_language = "es";
        g_translation_cache.clear();
      }
      ImGui::SameLine();
      if (ImGui::Button("FR")) {
        g_active_language = "fr";
        g_translation_cache.clear();
      }
      ImGui::Separator();

      ImGui::InputText(get_localized_string("username_label", "Username"),
                       username, IM_ARRAYSIZE(username));
      bool submit_password = ImGui::InputText(
          get_localized_string("password_label", "Password"), password,
          IM_ARRAYSIZE(password),
          ImGuiInputTextFlags_Password | ImGuiInputTextFlags_EnterReturnsTrue);
      if (ImGui::Button(get_localized_string("login_button", "Login")) ||
          submit_password) {
        if ((strcmp(username, "admin") == 0 ||
             strcmp(username, "operator") == 0) &&
            strcmp(password, "password") == 0) {
          logged_in = true;
        }
      }
      ImGui::End();
    } else {
      std::lock_guard<std::mutex> lock(g_state_mutex);

      // Dashboard Window
      ImGui::Begin("SGRT Monitoring HUD");
      ImGui::Text("Active Language: %s", g_active_language.c_str());
      if (ImGui::Button("EN")) {
        g_active_language = "en";
        g_translation_cache.clear();
      }
      ImGui::SameLine();
      if (ImGui::Button("ES")) {
        g_active_language = "es";
        g_translation_cache.clear();
      }
      ImGui::SameLine();
      if (ImGui::Button("FR")) {
        g_active_language = "fr";
        g_translation_cache.clear();
      }

      ImGui::Text("%s%s",
                  get_localized_string("calibration_status_prefix",
                                       "Calibration Status: "),
                  g_calibration_status == 1
                      ? get_localized_string("calibration_valid", "Valid")
                      : get_localized_string("calibration_invalid", "Invalid"));

      ImVec4 beamColor;
      if (g_beam_state == 1)
        beamColor = ImVec4(0, 1, 0, 1); // BeamOn
      else if (g_beam_state == 2)
        beamColor = ImVec4(1, 1, 0, 1); // BeamHold
      else
        beamColor = ImVec4(1, 0, 0, 1); // BeamOff

      ImGui::TextColored(beamColor, "STATUS: %s",
                         g_localized_beam_state.c_str());

      // Respiratory trace
      std::vector<float> trace(g_resp_history.begin(), g_resp_history.end());
      ImGui::PlotLines(
          get_localized_string("resp_trace_title", "Respiratory Trace"),
          trace.data(), trace.size(), 0, NULL, -20.0f, 20.0f, ImVec2(0, 100));

      // Point Cloud Info
      ImGui::Text("Active Points: %zu", g_points.size());

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
      float top = tan(fov * 3.14159f / 360.0f) * near_plane;
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
      glColor3f(0.0f, 1.0f, 1.0f);
      for (const auto &pt : g_points) {
        glVertex3f(pt.x, pt.y, pt.z);
      }
      glEnd();
    }

    ImGui::Render();
    if (logged_in) {
      // we already cleared, just render imgui on top
    } else {
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
  glfwDestroyWindow(window);
  glfwTerminate();
}
