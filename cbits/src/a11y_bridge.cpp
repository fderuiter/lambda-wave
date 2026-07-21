#include <a11y_bridge.h>

#include <iostream>

#ifdef _WIN32
#include <windows.h>
// In a full implementation, this would use UIAutomationCore.h and related COM
// interfaces.
namespace A11y {
void Init() {
  // CoInitializeEx(NULL, COINIT_MULTITHREADED);
}
void Shutdown() {
  // CoUninitialize();
}
void Announce(const std::string &message) {
  // UiaRaiseAutomationEvent(...) or active screen reader TTS announcement
  std::cout << "[A11y-Win32] Announce: " << message << std::endl;
}
void SetFocus(const std::string &element_name, const std::string &role) {
  // UiaRaiseAutomationEvent(..., UIA_AutomationFocusChangedEventId)
  std::cout << "[A11y-Win32] Focus: " << element_name << " (" << role << ")"
            << std::endl;
}
}  // namespace A11y

#elif defined(__APPLE__)
// In a full implementation, this would use NSAccessibility APIs.
namespace A11y {
void Init() {}
void Shutdown() {}
void Announce(const std::string &message) {
  // NSAccessibilityPostNotification(NSApp,
  // NSAccessibilityAnnouncementRequestedNotification, userInfo);
  std::cout << "[A11y-macOS] Announce: " << message << std::endl;
}
void SetFocus(const std::string &element_name, const std::string &role) {
  // NSAccessibilityPostNotification(element,
  // NSAccessibilityFocusedUIElementChangedNotification, userInfo);
  std::cout << "[A11y-macOS] Focus: " << element_name << " (" << role << ")"
            << std::endl;
}
}  // namespace A11y

#else
// Linux / Fallback
namespace A11y {
void Init() {}
void Shutdown() {}
void Announce(const std::string &message) {
  std::cout << "[A11y-Linux] Announce: " << message << std::endl;
}
void SetFocus(const std::string &element_name, const std::string &role) {
  std::cout << "[A11y-Linux] Focus: " << element_name << " (" << role << ")"
            << std::endl;
}
}  // namespace A11y
#endif
