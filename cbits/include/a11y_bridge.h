#pragma once

#include <string>

namespace A11y {

void Init();
void Shutdown();
void Announce(const std::string &message);
void SetFocus(const std::string &element_name, const std::string &role);

} // namespace A11y
