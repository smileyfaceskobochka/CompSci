#pragma once

#include "Game/GameBoard.hpp"
#include <map>
#include <raylib.h>

namespace TernaryTiles {

enum class RendererState { Menu, Playing, Help };

class Renderer {
public:
  Renderer(int width, int height, const char *title);
  ~Renderer();

  // Disable copying
  Renderer(const Renderer &) = delete;
  Renderer &operator=(const Renderer &) = delete;

  // Main rendering loop
  void run();

  // Check if the renderer was initialized successfully
  [[nodiscard]] bool isInitialized() const { return m_initialized; }

private:
  static constexpr int TILE_SIZE = 100;
  static constexpr int PADDING = 15;

  GameBoard m_board;

  Font m_font{};
  int m_fontSize{48};
  bool m_initialized{false};

  // Colors
  const Color BACKGROUND_COLOR{238, 228, 218, 255}; // Light beige
  const Color BOARD_COLOR{187, 173, 160, 255};      // Gray
  const Color TEXT_COLOR{119, 110, 101, 255};       // Dark gray
  const Color TILE_TEXT_COLOR{249, 246, 242, 255};  // White for tile text

  // Get color for a tile based on its value
  [[nodiscard]] Color getTileColor(int value) const;

  // Draw the game board
  void drawBoard();

  // Draw a single tile
  void drawTile(int row, int col, int value);
  void drawTile(int row, int col, int value, float animX, float animY,
                float tileScale);

  // Draw text centered in a rectangle
  void drawCenteredText(const char *text, int x, int y, int fontSize,
                        Color color) const;

  // Draw text using the custom font if available
  void drawText(const char *text, int posX, int posY, int fontSize,
                Color color) const;

  // Draw text centered using the custom font if available
  void drawCenteredText(const char *text, int y, int fontSize,
                        Color color) const;

  // Handle input
  void processInput();

  // Animation state and helpers
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
    TileAnim()
        : type(Type::Move), fromX(0), fromY(0), toX(0), toY(0), progress(0),
          duration(0), value(0), active(false) {}
  };
  std::map<std::tuple<int, int, int>, TileAnim> m_tileAnims;
  float m_animSpeed = 8.0f;
  void updateAnimations(float dt);
  void startTileMoveAnim(int row, int col, int value, float from_x,
                         float from_y, float to_x, float to_y);
  void startTileSpawnAnim(int row, int col, int value, float x, float y);
  void drawAnimatingTiles();

  RendererState m_state = RendererState::Menu;
  int m_selectedBoardSize = 4; // Default 4x4
  bool m_hasWon = false;
  int m_pendingMoveDir = -1;
  bool m_waitingForAnim = false;

  void drawMenu();
  void drawHelp();
  void handleMenuInput();
  void handleHelpInput();
  void startGameWithBoardSize(int size);

  float getScale() const;
};

} // namespace TernaryTiles
