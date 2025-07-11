#include "Game/BalancedTernary.hpp"
#include <algorithm>
#include <cassert>
#include <cmath>
#include <sstream>

namespace TernaryTiles {

std::vector<TernaryDigit> decimalToBalancedTernary(int n) {
  std::vector<TernaryDigit> result;

  if (n == 0) {
    result.push_back(0);
    return result;
  }

  while (n != 0) {
    int remainder = n % 3;
    n = n / 3;

    if (remainder == 2) {
      remainder = -1;
      n++;
    } else if (remainder == -2) {
      remainder = 1;
      n--;
    } else if (remainder == -1) {
      remainder = -1;
    }

    result.push_back(static_cast<TernaryDigit>(remainder));
  }

  return result;
}

int balancedTernaryToDecimal(const std::vector<TernaryDigit> &digits) {
  int result = 0;
  int power = 1;

  for (TernaryDigit d : digits) {
    // Use assert instead of exception since exceptions are disabled
    assert(d >= -1 && d <= 1 && "Invalid balanced ternary digit");
    result += d * power;
    power *= 3;
  }

  return result;
}

TernaryDigit addTernaryDigits(TernaryDigit a, TernaryDigit b,
                              TernaryDigit &carry) {
  // Normalize inputs to -1, 0, or 1
  a = (a > 0) - (a < 0);
  b = (b > 0) - (b < 0);

  int sum = a + b + carry;

  // Determine the result digit and new carry
  TernaryDigit result;
  if (sum > 1) {
    result = sum - 3;
    carry = 1;
  } else if (sum < -1) {
    result = sum + 3;
    carry = -1;
  } else {
    result = sum;
    carry = 0;
  }

  return result;
}

std::vector<TernaryDigit>
addBalancedTernary(const std::vector<TernaryDigit> &a,
                   const std::vector<TernaryDigit> &b) {
  std::vector<TernaryDigit> result;
  TernaryDigit carry = 0;
  size_t max_len = std::max(a.size(), b.size());

  for (size_t i = 0; i < max_len || carry != 0; ++i) {
    TernaryDigit digit_a = (i < a.size()) ? a[i] : 0;
    TernaryDigit digit_b = (i < b.size()) ? b[i] : 0;

    TernaryDigit sum = addTernaryDigits(digit_a, digit_b, carry);
    result.push_back(sum);
  }

  // Remove leading zeros
  while (result.size() > 1 && result.back() == 0) {
    result.pop_back();
  }

  return result;
}

std::string balancedTernaryToString(const std::vector<TernaryDigit> &digits) {
  if (digits.empty()) {
    return "0";
  }

  std::string result;
  // Iterate from most significant to least significant digit
  for (auto it = digits.rbegin(); it != digits.rend(); ++it) {
    if (*it == -1) {
      result += 'n';
    } else {
      result += ('0' + *it);
    }
  }

  return result;
}

std::string
balancedTernaryToDecimalString(const std::vector<TernaryDigit> &digits) {
  int decimal = balancedTernaryToDecimal(digits);
  return std::to_string(decimal);
}

} // namespace TernaryTiles
