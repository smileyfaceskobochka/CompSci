#pragma once

#include <cmath>
#include <cstdint>
#include <string>
#include <vector>

namespace TernaryTiles {

class Tile {
public:
  // Default constructor creates a tile with value 0 (empty)
  constexpr Tile() = default;

  // Create a tile with a specific value (must be a power of 3 or its negative,
  // or 0) If an invalid value is provided, creates an empty tile (0)
  constexpr explicit Tile(int value) : m_value{0} {
    if (value != 0) {
      int abs_val = std::abs(value);
      if (isPowerOfThree(abs_val)) {
        m_value = value;
      }
      // If not a power of 3, leave as 0 (empty)
    }
  }

  // Get the tile's value
  [[nodiscard]] constexpr int value() const { return m_value; }

  // Check if the tile is empty (value == 0)
  [[nodiscard]] constexpr bool isEmpty() const { return m_value == 0; }

  // Check if this tile can merge with another tile
  // Tiles can only merge if they have the same absolute value
  [[nodiscard]] constexpr bool canMergeWith(const Tile &other) const {
    if (isEmpty() || other.isEmpty())
      return false;

    // Only allow merges between tiles with the same absolute value
    return std::abs(m_value) == std::abs(other.m_value);
  }

  // Merge this tile with another tile (assumes canMergeWith is true)
  // Returns the new tile after merging according to the rules
  [[nodiscard]] Tile mergeWith(const Tile &other) const {
    if (!canMergeWith(other)) {
      return *this; // Shouldn't happen if used correctly
    }

    int abs_this = std::abs(m_value);
    int abs_other = std::abs(other.m_value);

    // Handle same value merges (1+1=3, -1+-1=-3, 3+3=9, etc.)
    if (m_value == other.m_value) {
      return Tile(m_value * 3);
    }

    // Handle opposite value merges (1+-1=0, 3+-3=1, 9+-9=3, etc.)
    if (m_value == -other.m_value) {
      if (abs_this == 1) {
        return Tile(0); // 1 + -1 = 0
      } else {
        return Tile(m_value > 0
                        ? (m_value / 3)
                        : (m_value / 3)); // 3 + -3 = 1, -3 + 3 = -1, etc.
      }
    }

    // Handle step-down merges (3 + 1 = 1, -3 + -1 = -1, etc.)
    if (abs_this * 3 == abs_other) {
      return Tile(m_value > 0 ? (abs_this) : (-abs_this));
    } else if (abs_this == abs_other * 3) {
      return Tile(other.m_value > 0 ? (abs_other) : (-abs_other));
    }

    // Should never reach here if canMergeWith is correct
    return *this;
  }

  // Reset the tile to empty
  constexpr void clear() { m_value = 0; }

  // Get the absolute value of the tile
  [[nodiscard]] constexpr int absValue() const { return std::abs(m_value); }

  // Check if two tiles are equal
  [[nodiscard]] constexpr bool operator==(const Tile &other) const {
    return m_value == other.m_value;
  }

  // Check if two tiles are not equal
  [[nodiscard]] constexpr bool operator!=(const Tile &other) const {
    return !(*this == other);
  }

  // Get a string representation of the tile
  [[nodiscard]] std::string toString() const {
    if (isEmpty())
      return "0";
    if (m_value < 0)
      return std::to_string(-m_value) + "n";
    return std::to_string(m_value);
  }

private:
  int m_value{0}; // Can be 0, ±1, ±3, ±9, ±27, etc.

  // Helper function to check if a number is a power of 3
  static constexpr bool isPowerOfThree(int n) {
    if (n <= 0)
      return false;
    while (n % 3 == 0) {
      n /= 3;
    }
    return n == 1;
  }
};

} // namespace TernaryTiles
