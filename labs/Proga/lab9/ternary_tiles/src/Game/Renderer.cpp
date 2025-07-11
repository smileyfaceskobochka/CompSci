#include "Game/Renderer.hpp"
#include <algorithm>
#include <cmath>
#include <functional>
#include <map>
#include <sstream>
#include <tuple>
#include <unordered_set>
#include <vector>

namespace TernaryTiles {

// Renderer handles window, drawing, and input
Renderer::Renderer(int width, int height, const char *title)
    : m_font(GetFontDefault()), m_fontSize(48), m_initialized(false),
      m_hasWon(false) {
  if (width < 320)
    width = 320;
  if (height < 240)
    height = 240;
  InitWindow(width, height, title);
  SetWindowMinSize(320, 240);
  SetWindowState(FLAG_WINDOW_RESIZABLE);
  SetTargetFPS(60);
  m_animSpeed = 14.0f;
  m_initialized = IsWindowReady();
}

float Renderer::getScale() const {
  // Compute scale so board fits in window
  const int size = m_board.grid().size();
  int boardPixels = TILE_SIZE * size + PADDING * (size + 1);
  float scaleX = (GetScreenWidth() - 2 * PADDING) / (float)boardPixels;
  float scaleY = (GetScreenHeight() - 2 * PADDING - 120) / (float)boardPixels;
  return std::min(1.0f, std::min(scaleX, scaleY));
}

Renderer::~Renderer() { CloseWindow(); }

// Main game loop
void Renderer::run() {
  float lastTime = GetTime();
  while (!WindowShouldClose()) {
    float now = GetTime();
    float dt = now - lastTime;
    lastTime = now;
    if (m_state == RendererState::Menu) {
      handleMenuInput();
      BeginDrawing();
      ClearBackground(RAYWHITE);
      drawMenu();
      EndDrawing();
      continue;
    } else if (m_state == RendererState::Help) {
      handleHelpInput();
      BeginDrawing();
      ClearBackground(RAYWHITE);
      drawHelp();
      EndDrawing();
      continue;
    }
    processInput();
    updateAnimations(dt);
    // Track win state
    if (m_board.isWin() && !m_hasWon) {
      m_hasWon = true;
    }
    BeginDrawing();
    ClearBackground(RAYWHITE);
    drawBoard();
    drawAnimatingTiles();
    // Draw score
    std::stringstream scoreText;
    scoreText << "Score: " << m_board.score();
    DrawText(scoreText.str().c_str(), PADDING, PADDING, 30, TEXT_COLOR);
    // Draw game over message if the game is over
    if (m_board.isGameOver()) {
      DrawRectangle(0, 0, GetScreenWidth(), GetScreenHeight(),
                    Color{0, 0, 0, 180});
      drawCenteredText("Game Over!", 0, -30, 40, WHITE);
      drawCenteredText("Press R to restart", 0, 20, 30, WHITE);
    }
    // Draw win banner if the player has won (but do not block input)
    if (m_hasWon) {
      int bannerHeight = 60;
      DrawRectangle(0, 0, GetScreenWidth(), bannerHeight,
                    Color{0, 128, 0, 220});
      drawCenteredText("You Win! Keep playing for a high score!", 0, 10, 32,
                       WHITE);
    }
    EndDrawing();
  }
}

// Draws the main menu
void Renderer::drawMenu() {
  const int w = GetScreenWidth();
  const int h = GetScreenHeight();
  drawCenteredText("BALANCED TERNARY 2048", h / 6, 48, TEXT_COLOR);
  drawCenteredText("Select Board Size:", h / 3, 32, TEXT_COLOR);
  int sizes[] = {3, 4, 5};
  int btnW = 100, btnH = 60, gap = 40;
  int totalW = 3 * btnW + 2 * gap;
  int startX = (w - totalW) / 2;
  int y = h / 3 + 50;
  for (int i = 0; i < 3; ++i) {
    int x = startX + i * (btnW + gap);
    Color c = (m_selectedBoardSize == sizes[i]) ? ORANGE : LIGHTGRAY;
    DrawRectangle(x, y, btnW, btnH, c);
    drawCenteredText(
        (std::to_string(sizes[i]) + "x" + std::to_string(sizes[i])).c_str(),
        x + btnW / 2, y + btnH / 2 - 16, 28, DARKGRAY);
  }
  // Start button
  int startBtnY = y + btnH + 40;
  DrawRectangle(w / 2 - 80, startBtnY, 160, 50, GREEN);
  drawCenteredText("Start Game", w / 2, startBtnY + 12, 28, WHITE);
  // Help button
  int helpBtnY = startBtnY + 70;
  DrawRectangle(w / 2 - 80, helpBtnY, 160, 40, BLUE);
  drawCenteredText("Help / Rules", w / 2, helpBtnY + 8, 24, WHITE);
}

// Draws the help/rules screen
void Renderer::drawHelp() {
  const int w = GetScreenWidth();
  const int h = GetScreenHeight();
  DrawRectangle(60, 60, w - 120, h - 120, ColorAlpha(LIGHTGRAY, 0.95f));
  drawCenteredText("How to Play", w / 2, 90, 36, TEXT_COLOR);
  int y = 140;
  int fontSize = 22;
  std::vector<std::string> lines = {
      "Combine tiles of the same absolute value:",
      "  - Same sign: merge to next power of 3 (e.g. 1+1=3, -1+-1=-3)",
      "  - Opposite sign: step down (e.g. 3+-3=1, 1+-1=0)",
      "  - Only +1 and -1 tiles spawn.",
      "Use arrow keys to move. Press R to restart.",
      "Game ends when no moves are possible."};
  for (const auto &line : lines) {
    drawCenteredText(line.c_str(), y, fontSize, TEXT_COLOR);
    y += fontSize + 8;
  }
  int btnW = 120, btnH = 40;
  int btnX = w / 2 - btnW / 2, btnY = h - 100;
  DrawRectangle(btnX, btnY, btnW, btnH, DARKGRAY);
  drawCenteredText("Back", btnY + 8, 24, WHITE);
}

// Handles menu input (mouse)
void Renderer::handleMenuInput() {
  int w = GetScreenWidth();
  int h = GetScreenHeight();
  int sizes[] = {3, 4, 5};
  int btnW = 100, btnH = 60, gap = 40;
  int totalW = 3 * btnW + 2 * gap;
  int startX = (w - totalW) / 2;
  int y = h / 3 + 50;
  Vector2 mouse = GetMousePosition();
  if (IsMouseButtonPressed(MOUSE_LEFT_BUTTON)) {
    for (int i = 0; i < 3; ++i) {
      int x = startX + i * (btnW + gap);
      Rectangle rect = {(float)x, (float)y, (float)btnW, (float)btnH};
      if (CheckCollisionPointRec(mouse, rect)) {
        m_selectedBoardSize = sizes[i];
      }
    }
    // Start button
    int startBtnY = y + btnH + 40;
    Rectangle startRect = {(float)(w / 2 - 80), (float)startBtnY, 160, 50};
    if (CheckCollisionPointRec(mouse, startRect)) {
      startGameWithBoardSize(m_selectedBoardSize);
    }
    // Help button
    int helpBtnY = startBtnY + 70;
    Rectangle helpRect = {(float)(w / 2 - 80), (float)helpBtnY, 160, 40};
    if (CheckCollisionPointRec(mouse, helpRect)) {
      m_state = RendererState::Help;
    }
  }
}

// Handles help screen input (mouse)
void Renderer::handleHelpInput() {
  int w = GetScreenWidth();
  int h = GetScreenHeight();
  Vector2 mouse = GetMousePosition();
  int btnW = 120, btnH = 40;
  int btnX = w / 2 - btnW / 2, btnY = h - 100;
  Rectangle backRect = {(float)btnX, (float)btnY, (float)btnW, (float)btnH};
  if (IsMouseButtonPressed(MOUSE_LEFT_BUTTON) &&
      CheckCollisionPointRec(mouse, backRect)) {
    m_state = RendererState::Menu;
  }
}

// Start a new game with selected board size
void Renderer::startGameWithBoardSize(int size) {
  int winValue = 0;
  switch (size) {
  case 3:
    winValue = 27;
    break;
  case 4:
    winValue = 81;
    break;
  case 5:
    winValue = 243;
    break;
  default:
    winValue = 81;
    break;
  }
  m_board = GameBoard(size, winValue);
  m_hasWon = false;
  m_state = RendererState::Playing;
}

// --- Animation helpers ---
// Animation state for a tile
struct TileAnim {
  enum class Type { Move, Spawn } type;
  float fromX, fromY, toX, toY;
  float progress; // 0.0 to 1.0
  float duration;
  int value;
  bool active;
  TileAnim(Type t, float fx, float fy, float tx, float ty, float dur, int val)
      : type(t), fromX(fx), fromY(fy), toX(tx), toY(ty), progress(0.0f),
        duration(dur), value(val), active(true) {}
};

// Update all active animations
void Renderer::updateAnimations(float dt) {
  for (auto it = m_tileAnims.begin(); it != m_tileAnims.end();) {
    TileAnim &anim = it->second;
    if (!anim.active) {
      ++it;
      continue;
    }
    anim.progress += dt / anim.duration;
    if (anim.progress >= 1.0f) {
      anim.progress = 1.0f;
      anim.active = false;
      it = m_tileAnims.erase(it);
    } else {
      ++it;
    }
  }
}

// Draw all animating tiles
void Renderer::drawAnimatingTiles() {
  for (const auto &[key, anim] : m_tileAnims) {
    if (!anim.active)
      continue;
    float x = anim.fromX + (anim.toX - anim.fromX) * anim.progress;
    float y = anim.fromY + (anim.toY - anim.fromY) * anim.progress;
    float scale = 1.0f;
    if (anim.type == TileAnim::Type::Spawn) {
      scale = 0.2f + 0.8f * anim.progress;
    }
    drawTile(std::get<0>(key), std::get<1>(key), anim.value, x, y, scale);
  }
}

// Start a move animation for a tile
void Renderer::startTileMoveAnim(int row, int col, int value, float from_x,
                                 float from_y, float to_x, float to_y) {
  m_tileAnims[{row, col, value}] =
      TileAnim(TileAnim::Type::Move, from_x, from_y, to_x, to_y, 0.15f, value);
}

// Start a spawn animation for a tile
void Renderer::startTileSpawnAnim(int row, int col, int value, float x,
                                  float y) {
  m_tileAnims[{row, col, value}] =
      TileAnim(TileAnim::Type::Spawn, x, y, x, y, 0.18f, value);
}

// Get color for a tile value
Color Renderer::getTileColor(int value) const {
  if (value == 0)
    return Color{205, 193, 180, 255};
  int absValue = std::abs(value);
  if (value > 0) {
    switch (absValue) {
    case 1:
      return Color{242, 177, 121, 255};
    case 3:
      return Color{236, 141, 83, 255};
    case 9:
      return Color{247, 124, 95, 255};
    case 27:
      return Color{245, 93, 62, 255};
    case 81:
      return Color{233, 89, 80, 255};
    default:
      return Color{200, 0, 0, 255};
    }
  } else {
    switch (absValue) {
    case 1:
      return Color{100, 149, 237, 255};
    case 3:
      return Color{65, 105, 225, 255};
    case 9:
      return Color{30, 144, 255, 255};
    case 27:
      return Color{0, 0, 205, 255};
    case 81:
      return Color{0, 0, 139, 255};
    default:
      return Color{0, 0, 100, 255};
    }
  }
}

// Draw the board, static tiles, and overlays
void Renderer::drawBoard() {
  const int size = m_board.grid().size();
  float scale = getScale();
  int boardPixels = TILE_SIZE * size + PADDING * (size + 1);
  int scaledBoard = static_cast<int>(boardPixels * scale);
  int startX = (GetScreenWidth() - scaledBoard) / 2;
  int startY = 120;
  // Center title horizontally
  const char *title = "Ternary Tiles";
  int titleFontSize = 36;
  int titleY = 30;
  int titleWidth = MeasureText(title, titleFontSize);
  int titleX = (GetScreenWidth() - titleWidth) / 2;
  drawText(title, titleX + 2, titleY + 2, titleFontSize,
           ColorAlpha(BLACK, 0.3f));
  drawText(title, titleX, titleY, titleFontSize, TEXT_COLOR);
  // Draw score with a nice background
  std::string scoreText = "SCORE: " + std::to_string(m_board.score());
  int scoreFontSize = 24;
  int scoreY = 80;
  int scorePadding = 20;
  int scoreTextWidth = MeasureText(scoreText.c_str(), scoreFontSize);
  int scoreBgX = (GetScreenWidth() - (scoreTextWidth + scorePadding * 2)) / 2;
  int scoreBgY = scoreY - scorePadding / 2;
  DrawRectangleRounded({static_cast<float>(scoreBgX),
                        static_cast<float>(scoreBgY),
                        static_cast<float>(scoreTextWidth + scorePadding * 2),
                        static_cast<float>(scoreFontSize + scorePadding)},
                       0.5f, 10, ColorAlpha(BOARD_COLOR, 0.7f));
  drawCenteredText(scoreText.c_str(), scoreY, scoreFontSize, TILE_TEXT_COLOR);

  // Draw board background
  DrawRectangleRounded({static_cast<float>(startX), static_cast<float>(startY),
                        static_cast<float>(scaledBoard),
                        static_cast<float>(scaledBoard)},
                       0.05f, 10, BOARD_COLOR);
  // Draw empty cells first (background)
  for (int row = 0; row < size; ++row) {
    for (int col = 0; col < size; ++col) {
      int x = startX + static_cast<int>(PADDING * scale) +
              col * static_cast<int>((TILE_SIZE + PADDING) * scale);
      int y = startY + static_cast<int>(PADDING * scale) +
              row * static_cast<int>((TILE_SIZE + PADDING) * scale);
      DrawRectangleRounded({static_cast<float>(x), static_cast<float>(y),
                            static_cast<float>(TILE_SIZE * scale),
                            static_cast<float>(TILE_SIZE * scale)},
                           0.1f, 10, getTileColor(0));
    }
  }
  // Draw static tiles (skip animating ones)
  struct TupleHash {
    std::size_t operator()(const std::tuple<int, int, int> &t) const {
      std::size_t h1 = std::hash<int>{}(std::get<0>(t));
      std::size_t h2 = std::hash<int>{}(std::get<1>(t));
      std::size_t h3 = std::hash<int>{}(std::get<2>(t));
      return h1 ^ (h2 << 1) ^ (h3 << 2);
    }
  };
  std::unordered_set<std::tuple<int, int, int>, TupleHash> animatingKeys;
  for (const auto &pair : m_tileAnims) {
    if (pair.second.active)
      animatingKeys.insert(pair.first);
  }
  for (int row = 0; row < size; ++row) {
    for (int col = 0; col < size; ++col) {
      int value = m_board.grid()[row][col].value();
      if (value == 0)
        continue;
      auto key = std::make_tuple(row, col, value);
      if (animatingKeys.find(key) == animatingKeys.end()) {
        drawTile(row, col, value);
      }
    }
  }
  // Draw game over message if the game is over
  if (m_board.isGameOver()) {
    DrawRectangle(0, 0, GetScreenWidth(), GetScreenHeight(),
                  Color{0, 0, 0, 128});
    const char *gameOverText = "GAME OVER";
    int gameOverFontSize = 48;
    int gameOverY = GetScreenHeight() / 2 - 60;
    drawCenteredText(gameOverText, gameOverY, gameOverFontSize, WHITE);
    const char *restartText = "Press R to restart";
    int restartFontSize = 24;
    int restartY = gameOverY + 80;
    drawCenteredText(restartText, restartY, restartFontSize, LIGHTGRAY);
  }
}

// Draw a tile at a custom position/scale (for animation)
void Renderer::drawTile([[maybe_unused]] int row, [[maybe_unused]] int col,
                        int value, float animX, float animY, float tileScale) {
  float scale = getScale();
  float tileSize = TILE_SIZE * scale * tileScale;
  float offset = (TILE_SIZE * scale - tileSize) / 2;
  DrawRectangleRounded({animX + offset, animY + offset, tileSize, tileSize},
                       0.1f, 10, getTileColor(value));
  if (value == 0)
    return;
  std::string displayText = TernaryTiles::Tile(value).toString();
  int fontSize = static_cast<int>(TILE_SIZE / 2 * scale * tileScale);
  int tileCenterX = static_cast<int>(animX + offset + tileSize / 2);
  int tileCenterY = static_cast<int>(animY + offset + tileSize / 2);
  int textWidth = MeasureText(displayText.c_str(), fontSize);
  int textHeight = fontSize;
  int textX = tileCenterX - textWidth / 2;
  int textY = tileCenterY - textHeight / 2;
  DrawText(displayText.c_str(), textX + 2, textY + 2, fontSize,
           ColorAlpha(BLACK, 0.25f));
  DrawText(displayText.c_str(), textX, textY, fontSize,
           (value < 0) ? TILE_TEXT_COLOR : TEXT_COLOR);
}

// Draw a static tile at its board position
void Renderer::drawTile(int row, int col, int value) {
  const int size = m_board.grid().size();
  float scale = getScale();
  int boardPixels = TILE_SIZE * size + PADDING * (size + 1);
  int scaledBoard = static_cast<int>(boardPixels * scale);
  int startX = (GetScreenWidth() - scaledBoard) / 2;
  int startY = 120;
  float animX = startX + PADDING * scale + col * (TILE_SIZE + PADDING) * scale;
  float animY = startY + PADDING * scale + row * (TILE_SIZE + PADDING) * scale;
  drawTile(row, col, value, animX, animY, 1.0f);
}

// Draw text at a position
void Renderer::drawText(const char *text, int posX, int posY, int fontSize,
                        Color color) const {
  DrawText(text, posX, posY, fontSize, color);
}

// Draw text centered horizontally
void Renderer::drawCenteredText(const char *text, int y, int fontSize,
                                Color color) const {
  int textWidth = MeasureText(text, fontSize);
  int x = (GetScreenWidth() - textWidth) / 2;
  drawText(text, x, y, fontSize, color);
}

// Draw text centered at (x, y)
void Renderer::drawCenteredText(const char *text, int x, int y, int fontSize,
                                Color color) const {
  int textWidth = MeasureText(text, fontSize);
  drawText(text, x - textWidth / 2, y, fontSize, color);
}

// Handle keyboard/game input
void Renderer::processInput() {
  if (m_board.isGameOver()) {
    if (IsKeyPressed(KEY_R)) {
      m_board.initialize();
    }
    return;
  }
  int moveDir = -1;
  if (IsKeyPressed(KEY_UP))
    moveDir = 0;
  else if (IsKeyPressed(KEY_RIGHT))
    moveDir = 1;
  else if (IsKeyPressed(KEY_DOWN))
    moveDir = 2;
  else if (IsKeyPressed(KEY_LEFT))
    moveDir = 3;
  if (moveDir != -1) {
    // Get move result for animation
    auto result = m_board.moveWithResult(moveDir);
    if (result.moved) {
      // Animate moved tiles
      float scale = getScale();
      int size = m_board.grid().size();
      int boardPixels = TILE_SIZE * size + PADDING * (size + 1);
      int scaledBoard = static_cast<int>(boardPixels * scale);
      int startX = (GetScreenWidth() - scaledBoard) / 2;
      int startY = 120;
      for (const auto &move : result.moves) {
        float fromX = startX + PADDING * scale +
                      move.fromCol * (TILE_SIZE + PADDING) * scale;
        float fromY = startY + PADDING * scale +
                      move.fromRow * (TILE_SIZE + PADDING) * scale;
        float toX = startX + PADDING * scale +
                    move.toCol * (TILE_SIZE + PADDING) * scale;
        float toY = startY + PADDING * scale +
                    move.toRow * (TILE_SIZE + PADDING) * scale;
        startTileMoveAnim(move.toRow, move.toCol, move.value, fromX, fromY, toX,
                          toY);
      }
      for (const auto &spawn : result.spawns) {
        float x = startX + PADDING * scale +
                  spawn.col * (TILE_SIZE + PADDING) * scale;
        float y = startY + PADDING * scale +
                  spawn.row * (TILE_SIZE + PADDING) * scale;
        startTileSpawnAnim(spawn.row, spawn.col, spawn.value, x, y);
      }
    }
  } else if (IsKeyPressed(KEY_R)) {
    m_board.initialize();
  }
}

} // namespace TernaryTiles
