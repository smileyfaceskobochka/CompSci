#include "Game/GameBoard.hpp"
#include <algorithm>
#include <iostream>
#include <random>

namespace TernaryTiles {

GameBoard::GameBoard() { initialize(); }

GameBoard::GameBoard(std::size_t size, int winValue)
    : m_size(size), m_winValue(winValue),
      m_grid(size, std::vector<Tile>(size)) {
  initialize();
}

void GameBoard::setSize(std::size_t size) {
  m_size = size;
  m_grid.assign(size, std::vector<Tile>(size));
  initialize();
}

void GameBoard::initialize() {
  for (auto &row : m_grid) {
    for (auto &tile : row) {
      tile = Tile{};
    }
  }
  addRandomTile();
  addRandomTile();
}

std::vector<std::pair<size_t, size_t>> GameBoard::getEmptyCells() const {
  std::vector<std::pair<size_t, size_t>> emptyCells;
  for (size_t i = 0; i < m_size; ++i) {
    for (size_t j = 0; j < m_size; ++j) {
      if (m_grid[i][j].isEmpty()) {
        emptyCells.emplace_back(i, j);
      }
    }
  }
  return emptyCells;
}

bool GameBoard::move(int direction) {
  bool moved = false;
  switch (direction) {
  case 0:
    moved = moveUp();
    break;
  case 1:
    moved = moveRight();
    break;
  case 2:
    moved = moveDown();
    break;
  case 3:
    moved = moveLeft();
    break;
  }
  if (moved) {
    addRandomTile();
  }
  return moved;
}

// Move with animation result for UI
GameBoard::MoveResult GameBoard::moveWithResult(int direction) {
  MoveResult result;
  auto oldGrid = m_grid;
  bool moved = false;
  switch (direction) {
  case 0:
    moved = moveUp();
    break;
  case 1:
    moved = moveRight();
    break;
  case 2:
    moved = moveDown();
    break;
  case 3:
    moved = moveLeft();
    break;
  }
  result.moved = moved;
  if (moved) {
    // Track moved and merged tiles for animation
    std::vector<std::vector<bool>> matchedOld(m_size,
                                              std::vector<bool>(m_size, false));
    for (size_t toRow = 0; toRow < m_size; ++toRow) {
      for (size_t toCol = 0; toCol < m_size; ++toCol) {
        const Tile &newTile = m_grid[toRow][toCol];
        if (!newTile.isEmpty() && newTile != oldGrid[toRow][toCol]) {
          bool found = false;
          for (size_t fromRow = 0; fromRow < m_size && !found; ++fromRow) {
            for (size_t fromCol = 0; fromCol < m_size && !found; ++fromCol) {
              if (!matchedOld[fromRow][fromCol] &&
                  !oldGrid[fromRow][fromCol].isEmpty() &&
                  oldGrid[fromRow][fromCol].value() == newTile.value()) {
                result.moves.push_back({(int)fromRow, (int)fromCol, (int)toRow,
                                        (int)toCol, newTile.value()});
                matchedOld[fromRow][fromCol] = true;
                found = true;
              }
            }
          }
          if (!found) {
            result.moves.push_back({(int)toRow, (int)toCol, (int)toRow,
                                    (int)toCol, newTile.value()});
          }
        }
      }
    }
    addRandomTileWithResult(result);
  }
  return result;
}

bool GameBoard::moveLeft() {
  bool moved = false;
  for (auto &row : m_grid) {
    if (moveRowLeft(row)) {
      moved = true;
    }
  }
  return moved;
}

bool GameBoard::moveRight() {
  bool moved = false;
  for (auto &row : m_grid) {
    if (moveRowRight(row)) {
      moved = true;
    }
  }
  return moved;
}

bool GameBoard::moveUp() {
  bool moved = false;
  for (size_t col = 0; col < m_size; ++col) {
    Row column(m_size);
    for (size_t row = 0; row < m_size; ++row) {
      column[row] = m_grid[row][col];
    }
    if (moveRowLeft(column)) {
      moved = true;
      for (size_t row = 0; row < m_size; ++row) {
        m_grid[row][col] = column[row];
      }
    }
  }
  return moved;
}

bool GameBoard::moveDown() {
  bool moved = false;
  for (size_t col = 0; col < m_size; ++col) {
    Row column(m_size);
    for (size_t row = 0; row < m_size; ++row) {
      column[row] = m_grid[m_size - 1 - row][col];
    }
    if (moveRowLeft(column)) {
      moved = true;
      for (size_t row = 0; row < m_size; ++row) {
        m_grid[m_size - 1 - row][col] = column[row];
      }
    }
  }
  return moved;
}

// Slide and merge left
bool GameBoard::moveRowLeft(Row &row) {
  size_t write_pos = 0;
  bool moved = false;
  for (size_t i = 0; i < m_size; ++i) {
    if (!row[i].isEmpty()) {
      if (i != write_pos) {
        row[write_pos] = row[i];
        row[i].clear();
        moved = true;
      }
      ++write_pos;
    }
  }
  for (size_t i = 0; i < m_size - 1;) {
    if (!row[i].isEmpty() && row[i].canMergeWith(row[i + 1])) {
      Tile merged = row[i].mergeWith(row[i + 1]);
      row[i] = merged;
      for (size_t j = i + 1; j < m_size - 1; ++j) {
        row[j] = row[j + 1];
      }
      row[m_size - 1].clear();
      moved = true;
      continue;
    }
    ++i;
  }
  return moved;
}

// Slide and merge right
bool GameBoard::moveRowRight(Row &row) {
  int write_pos = m_size - 1;
  bool moved = false;
  for (int i = m_size - 1; i >= 0; --i) {
    if (!row[i].isEmpty()) {
      if (i != write_pos) {
        row[write_pos] = row[i];
        row[i].clear();
        moved = true;
      }
      --write_pos;
    }
  }
  for (int i = m_size - 1; i > 0;) {
    if (!row[i].isEmpty() && row[i].canMergeWith(row[i - 1])) {
      Tile merged = row[i].mergeWith(row[i - 1]);
      row[i] = merged;
      for (int j = i - 1; j > 0; --j) {
        row[j] = row[j - 1];
      }
      row[0].clear();
      moved = true;
      continue;
    }
    --i;
  }
  return moved;
}

// Add a random +1 or -1 tile to an empty cell
void GameBoard::addRandomTile() {
  auto emptyCells = getEmptyCells();
  if (emptyCells.empty())
    return;
  std::uniform_int_distribution<size_t> cellDist(0, emptyCells.size() - 1);
  const auto &[row, col] = emptyCells[cellDist(m_rng)];
  int value = (m_value_dist(m_rng) == 0) ? -1 : 1;
  m_grid[row][col] = Tile{value};
}

// Add a random tile and record it for animation
void GameBoard::addRandomTileWithResult(MoveResult &result) {
  auto emptyCells = getEmptyCells();
  if (emptyCells.empty())
    return;
  std::uniform_int_distribution<size_t> cellDist(0, emptyCells.size() - 1);
  const auto &[row, col] = emptyCells[cellDist(m_rng)];
  int value = (m_value_dist(m_rng) == 0) ? -1 : 1;
  m_grid[row][col] = Tile{value};
  result.spawns.push_back({(int)row, (int)col, value});
}

// Check if any moves are possible
bool GameBoard::canMove() const {
  for (const auto &row : m_grid) {
    for (const auto &tile : row) {
      if (tile.isEmpty()) {
        return true;
      }
    }
  }
  for (size_t i = 0; i < m_size; ++i) {
    for (size_t j = 0; j < m_size - 1; ++j) {
      if (m_grid[i][j].canMergeWith(m_grid[i][j + 1])) {
        return true;
      }
    }
  }
  for (size_t i = 0; i < m_size - 1; ++i) {
    for (size_t j = 0; j < m_size; ++j) {
      if (m_grid[i][j].canMergeWith(m_grid[i + 1][j])) {
        return true;
      }
    }
  }
  return false;
}

bool GameBoard::isFull() const {
  for (const auto &row : m_grid) {
    for (const auto &tile : row) {
      if (tile.isEmpty()) {
        return false;
      }
    }
  }
  return true;
}

// No more moves possible
bool GameBoard::isGameOver() const { return !canMove(); }

// Win if any tile reaches or exceeds win value
bool GameBoard::isWin() const {
  for (const auto &row : m_grid) {
    for (const auto &tile : row) {
      if (tile.absValue() >= m_winValue) {
        return true;
      }
    }
  }
  return false;
}

} // namespace TernaryTiles
