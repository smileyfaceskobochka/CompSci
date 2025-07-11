#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace TernaryTiles {

using TernaryDigit = int8_t;


std::vector<TernaryDigit> decimalToBalancedTernary(int n);

/**
 * @brief Converts a balanced ternary number to decimal
 * @param digits Vector of ternary digits (least significant digit first)
 * @return The decimal equivalent
 */
int balancedTernaryToDecimal(const std::vector<TernaryDigit> &digits);

/**
 * @brief Adds two balanced ternary digits with carry
 * @param a First digit
 * @param b Second digit
 * @param carry Input/Output carry (can be -1, 0, or 1)
 * @return The sum digit
 */
TernaryDigit addTernaryDigits(TernaryDigit a, TernaryDigit b,
                              TernaryDigit &carry);

/**
 * @brief Adds two balanced ternary numbers
 * @param a First number (least significant digit first)
 * @param b Second number (least significant digit first)
 * @return The sum in balanced ternary (least significant digit first)
 */
std::vector<TernaryDigit>
addBalancedTernary(const std::vector<TernaryDigit> &a,
                   const std::vector<TernaryDigit> &b);

/**
 * @brief Converts a balanced ternary number to string representation
 * @param digits The number to convert (least significant digit first)
 * @return String representation (most significant digit first)
 */
std::string balancedTernaryToString(const std::vector<TernaryDigit> &digits);

/**
 * @brief Converts a balanced ternary number to its decimal string
 * representation
 * @param digits The number to convert (least significant digit first)
 * @return Decimal string representation
 */
std::string
balancedTernaryToDecimalString(const std::vector<TernaryDigit> &digits);

} // namespace TernaryTiles
