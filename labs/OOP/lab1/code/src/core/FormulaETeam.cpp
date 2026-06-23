#include "FormulaETeam.h"
#include <algorithm>
#include <iomanip>
#include <iostream>
#include <sstream>

FormulaETeam::FormulaETeam()
    : FormulaETeam("Unknown", "Unknown", "Unknown", "Unknown") {}

FormulaETeam::FormulaETeam(const std::string &name,
                           const std::string &principal, const std::string &hq,
                           const std::string &partner)
    : FormulaETeam(name, principal, hq, partner, 50, 38.0, 0, 0, 0) {}

FormulaETeam::FormulaETeam(const std::string &name,
                           const std::string &principal, const std::string &hq,
                           const std::string &partner, int sustainScore,
                           double batteryKwh, int points, int wins, int pods)
    : FormulaTeam(name, principal, hq, points, wins, pods),
      sustainabilityScore(sustainScore), batteryCapacityKwh(batteryKwh),
      energyPartner(partner) {}

void FormulaETeam::updateSustainabilityScore(int delta) {
  sustainabilityScore = std::clamp(sustainabilityScore + delta, 0, 100);
}

std::string FormulaETeam::getBatteryEfficiencyRating() const {
  if (batteryCapacityKwh >= 40.0)
    return "Next-Gen (40+ kWh)";
  if (batteryCapacityKwh >= 38.0)
    return "Gen3 Standard (38 kWh)";
  return "Legacy";
}

void FormulaETeam::displayInfo() const {
  std::cout << "\033[1;32m[Formula E]\033[0m " << teamName << "\n"
            << "  Principal    : " << principalName << "\n"
            << "  HQ           : " << headquarters << "\n"
            << "  Energy Partner: " << energyPartner << "\n"
            << "  Battery      : " << std::fixed << std::setprecision(1)
            << batteryCapacityKwh << " kWh"
            << " [" << getBatteryEfficiencyRating() << "]\n"
            << "  Sustainability: " << sustainabilityScore << "/100\n";
  FormulaTeam::displayStats();
}

std::string FormulaETeam::getSeriesName() const { return "Formula E"; }

// Formula E scoring matches F1 for top 10
int FormulaETeam::calculatePoints(int position) const {
  const int FE_POINTS[] = {25, 18, 15, 12, 10, 8, 6, 4, 2, 1};
  if (position >= 1 && position <= 10)
    return FE_POINTS[position - 1];
  return 0;
}

void FormulaETeam::displayStats() const {
  FormulaTeam::displayStats();
  std::cout << "  Sustainability: " << sustainabilityScore << "/100\n"
            << "  Battery Rating: " << getBatteryEfficiencyRating() << "\n";
}

std::string FormulaETeam::getSummary() const {
  std::ostringstream oss;
  oss << FormulaTeam::getSummary() << " | Eco: " << sustainabilityScore
      << "/100"
      << " | Battery: " << batteryCapacityKwh << " kWh";
  return oss.str();
}
