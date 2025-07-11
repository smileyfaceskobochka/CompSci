#pragma once

#include "Game/Tile.hpp"
#include <array>
#include <random>
#include <utility> // for std::pair
#include <vector>

namespace TernaryTiles {

class GameBoard {
public:
  static constexpr std::size_t DEFAULT_SIZE = 4;
  using Grid = std::vector<std::vector<Tile>>;
  using Row = std::vector<Tile>;

  GameBoard();
  explicit GameBoard(std::size_t size, int winValue = 81);

  // Initialize the board with two random tiles
  void initialize();

  // Move tiles in a direction (0: up, 1: right, 2: down, 3: left)
  // Returns true if the board state changed
  bool move(int direction);

  // Check if the game is over (no more moves possible)
  [[nodiscard]] bool isGameOver() const;

  // Get the current score (sum of absolute values of all tiles)
  [[nodiscard]] int score() const {
    int total = 0;
    for (const auto &row : m_grid) {
      for (const auto &tile : row) {
        if (!tile.isEmpty()) {
          total += tile.absValue();
        }
      }
    }
    return total;
  }

  // Get a read-only view of the grid
  [[nodiscard]] const Grid &grid() const { return m_grid; }

  // For testing and debugging
  void setTile(int row, int col, int value) {
    if (row >= 0 && row < static_cast<int>(DEFAULT_SIZE) && col >= 0 &&
        col < static_cast<int>(DEFAULT_SIZE)) {
      m_grid[row][col] = Tile(value);
    }
  }

  void setWinValue(int winValue) { m_winValue = winValue; }
  void setSize(std::size_t size);
  [[nodiscard]] std::size_t size() const { return m_size; }
  [[nodiscard]] bool isWin() const;

  struct MoveInfo {
    int fromRow, fromCol;
    int toRow, toCol;
    int value;
  };
  struct SpawnInfo {
    int row, col;
    int value;
  };
  struct MoveResult {
    bool moved = false;
    std::vector<MoveInfo> moves;
    std::vector<SpawnInfo> spawns;
  };

  // New: Move with result info for animation
  MoveResult moveWithResult(int direction);

private:
  std::size_t m_size;
  int m_winValue;
  Grid m_grid;
  std::mt19937 m_rng{std::random_device{}()};
  std::uniform_int_distribution<std::size_t> m_pos_dist{0, DEFAULT_SIZE - 1};
  std::uniform_int_distribution<int> m_value_dist{0, 1}; // 0 or 1 -> -1 or 1

  // Helper methods
  bool moveLeft();
  bool moveRight();
  bool moveUp();
  bool moveDown();
  bool moveRowLeft(Row &row);
  bool moveRowRight(Row &row);
  void addRandomTile();
  bool canMove() const;
  [[nodiscard]] bool isFull() const;

  // Get a list of empty cell positions
  std::vector<std::pair<size_t, size_t>> getEmptyCells() const;

  // Helper for moveWithResult
  void addRandomTileWithResult(MoveResult &result);
};

} // namespace TernaryTiles
