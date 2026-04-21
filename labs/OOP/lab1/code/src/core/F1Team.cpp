#include "F1Team.h"
#include <iomanip>
#include <iostream>
#include <sstream>



F1Team::F1Team()
    : FormulaTeam(), powerUnit("Unknown"), budgetCapMln(135.0),
      constructorPos(0) {}

F1Team::F1Team(const std::string &name, const std::string &principal,
               const std::string &hq, const std::string &pu)
    : FormulaTeam(name, principal, hq), powerUnit(pu), budgetCapMln(135.0),
      constructorPos(0) {}

F1Team::F1Team(const std::string &name, const std::string &principal,
               const std::string &hq, const std::string &pu, double budget,
               int ctorPos, int points, int wins, int pods)
    : FormulaTeam(name, principal, hq, points, wins, pods), powerUnit(pu),
      budgetCapMln(budget), constructorPos(ctorPos) {}



double F1Team::calculateBudgetAllocation(double percentage) const {
  return budgetCapMln * (percentage / 100.0);
}

std::string F1Team::getPowerUnitStatus() const {
  if (raceWins > 10)
    return powerUnit + " [Dominant]";
  if (raceWins > 4)
    return powerUnit + " [Competitive]";
  return powerUnit + " [Development]";
}



void F1Team::displayInfo() const {
  std::cout << "\033[1;31m[Formula 1]\033[0m " << teamName << "\n"
            << "  Principal  : " << principalName << "\n"
            << "  HQ         : " << headquarters << "\n"
            << "  Power Unit : " << getPowerUnitStatus() << "\n"
            << "  Budget Cap : $" << std::fixed << std::setprecision(1)
            << budgetCapMln << "M\n"
            << "  Ctor. Pos  : "
            << (constructorPos ? std::to_string(constructorPos) : "TBD")
            << "\n";
  displayStats();
}

std::string F1Team::getSeriesName() const { return "Formula 1"; }

// F1 points system: 25-18-15-12-10-8-6-4-2-1 for top 10
int F1Team::calculatePoints(int position) const {
  const int F1_POINTS[] = {25, 18, 15, 12, 10, 8, 6, 4, 2, 1};
  if (position >= 1 && position <= 10)
    return F1_POINTS[position - 1];
  return 0;
}

void F1Team::displayStats() const {
  FormulaTeam::displayStats();
  std::cout << "  Constructor Position: "
            << (constructorPos ? std::to_string(constructorPos) : "N/A")
            << "\n";
}

std::string F1Team::getSummary() const {
  std::ostringstream oss;
  oss << FormulaTeam::getSummary() << " | PU: " << powerUnit << " | Budget: $"
      << budgetCapMln << "M";
  return oss.str();
}
