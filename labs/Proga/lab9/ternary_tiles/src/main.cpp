#include "Game/Renderer.hpp"
#include <iostream>

int main() {
  constexpr int screenWidth = 1280;
  constexpr int screenHeight = 720;

  TernaryTiles::Renderer renderer(screenWidth, screenHeight, "Ternary Tiles");
  if (!renderer.isInitialized()) {
    std::cerr << "Failed to initialize renderer\n";
    return 1;
  }

  renderer.run();
  return 0;
}
