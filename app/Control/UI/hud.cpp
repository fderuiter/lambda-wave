#include "RingBuffer.h"
#include "backends/imgui_impl_glut.h"
#include "backends/imgui_impl_opengl2.h"
#include "imgui.h"
#include <GL/glut.h>
#include <atomic>
#include <fcntl.h>
#include <iostream>
#include <thread>
#include <unistd.h>
#include <vector>

std::atomic<bool> g_loggedIn(false);
std::atomic<int> g_language(0); // 0 = EN, 1 = ES, 2 = FR
std::atomic<float> g_respiratoryTrace(0.0f);
std::vector<float> g_traceHistory;

int telemetry_fd = -1;
bool isRunning = true;

void telemetry_loop() {
  telemetry_fd = open("/tmp/sgrt_telemetry.fifo", O_RDONLY | O_NONBLOCK);
  while (isRunning) {
    if (telemetry_fd >= 0) {
      char buffer[256];
      ssize_t bytes = read(telemetry_fd, buffer, sizeof(buffer));
      if (bytes > 0) {
        // Parse telemetry directly from POSIX FIFO
        g_respiratoryTrace = (float)(rand() % 100) / 100.0f; // Simulated parse
      }
    }
    std::this_thread::sleep_for(std::chrono::milliseconds(33)); // 30 FPS
  }
  if (telemetry_fd >= 0)
    close(telemetry_fd);
}

void display() {
  ImGui_ImplOpenGL2_NewFrame();
  ImGui_ImplGLUT_NewFrame();
  ImGui::NewFrame();
  ImGuiIO &io = ImGui::GetIO();

  if (!g_loggedIn) {
    ImGui::Begin("System Access");
    ImGui::Text("Enter credentials to access SGRT Monitoring");
    static char user[64] = "";
    static char pass[64] = "";
    ImGui::InputText("Username", user, 64);
    ImGui::InputText("Password", pass, 64, ImGuiInputTextFlags_Password);
    if (ImGui::Button("Login")) {
      g_loggedIn = true; // Simplified auth
    }
    ImGui::End();
  } else {
    ImGui::Begin("SGRT Native C++ HUD");

    // Multi-language support
    const char *langs[] = {"English", "Espanol", "Francais"};
    int lang = g_language;
    if (ImGui::Combo("Language", &lang, langs, 3)) {
      g_language = lang;
    }

    ImGui::Text(g_language == 0
                    ? "Real-time Telemetry"
                    : (g_language == 1 ? "Telemetria en tiempo real"
                                       : "Telemetrie en temps reel"));

    g_traceHistory.push_back(g_respiratoryTrace);
    if (g_traceHistory.size() > 100)
      g_traceHistory.erase(g_traceHistory.begin());
    ImGui::PlotLines("Respiratory Trace", g_traceHistory.data(),
                     g_traceHistory.size());

    ImGui::Text("Point Cloud Renderer (OpenGL)");
    // The actual 3D point cloud would be rendered via pure OpenGL outside
    // ImGui, or rendered to a framebuffer and displayed as an ImGui image.
    ImGui::End();
  }

  ImGui::Render();
  glViewport(0, 0, (GLsizei)io.DisplaySize.x, (GLsizei)io.DisplaySize.y);
  glClearColor(0.1f, 0.1f, 0.1f, 1.0f);
  glClear(GL_COLOR_BUFFER_BIT);

  // Draw 3D point cloud directly from RingBuffer here using OpenGL

  ImGui_ImplOpenGL2_RenderDrawData(ImGui::GetDrawData());
  glutSwapBuffers();
  glutPostRedisplay();
}

extern "C" void run_cpp_hud() {
  int argc = 1;
  char *argv[1] = {(char *)"sgrt-hud"};
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_MULTISAMPLE);
  glutInitWindowSize(1280, 720);
  glutCreateWindow("SGRT Native C++ HUD");

  IMGUI_CHECKVERSION();
  ImGui::CreateContext();
  ImGui::StyleColorsDark();

  ImGui_ImplGLUT_Init();
  ImGui_ImplGLUT_InstallFuncs();
  ImGui_ImplOpenGL2_Init();

  glutDisplayFunc(display);

  std::thread t1(telemetry_loop);

  glutMainLoop();

  isRunning = false;
  t1.join();

  ImGui_ImplOpenGL2_Shutdown();
  ImGui_ImplGLUT_Shutdown();
  ImGui::DestroyContext();
}
