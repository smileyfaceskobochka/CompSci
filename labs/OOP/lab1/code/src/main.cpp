#include "core/TeamFactory.h"
#include "ui/UI.h"

int main() {
  auto teams = createTeams();
  UI ui(std::move(teams));
  ui.run();
  return 0;
}
