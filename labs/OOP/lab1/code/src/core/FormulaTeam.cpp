#include "FormulaTeam.h"
#include <iomanip>
#include <sstream>

FormulaTeam::FormulaTeam()
    : FormulaTeam("Unknown", "Unknown", "Unknown") {}

FormulaTeam::FormulaTeam(const std::string &name, const std::string &principal,
                         const std::string &hq)
    : FormulaTeam(name, principal, hq, 0, 0, 0) {}

FormulaTeam::FormulaTeam(const std::string &name, const std::string &principal,
                         const std::string &hq, int points, int wins, int pods)
    : teamName(name), principalName(principal), headquarters(hq),
      championshipPoints(points), raceWins(wins), podiums(pods) {}

void FormulaTeam::addPoints(int pts) {
  if (pts > 0)
    championshipPoints += pts;
}

void FormulaTeam::addWin() {
  ++raceWins;
  ++podiums; // a win is also a podium
}

void FormulaTeam::addPodium() { ++podiums; }

void FormulaTeam::resetSeason() {
  championshipPoints = 0;
  raceWins = 0;
  podiums = 0;
}

double FormulaTeam::getWinRate(int totalRaces) const {
  if (totalRaces <= 0)
    return 0.0;
  return static_cast<double>(raceWins) / totalRaces * 100.0;
}

void FormulaTeam::addDriver(const FormulaDriver &driver) {
  drivers.push_back(driver);
}

const std::vector<FormulaDriver> &FormulaTeam::getDrivers() const {
  return drivers;
}

std::vector<FormulaDriver> &FormulaTeam::getDriversRef() { return drivers; }

void FormulaTeam::displayStats() const {
  std::cout << "  Points : " << championshipPoints << "\n"
            << "  Wins   : " << raceWins << "\n"
            << "  Podiums: " << podiums << "\n";
}

std::string FormulaTeam::getSummary() const {
  std::ostringstream oss;
  oss << "[" << getSeriesName() << "] " << teamName
      << " | Pts: " << championshipPoints << " | Wins: " << raceWins;
  return oss.str();
}

FormulaTeam &FormulaTeam::operator+=(int pts) {
  addPoints(pts);
  return *this;
}

FormulaTeam &FormulaTeam::operator++() {
  addWin();
  return *this;
}

bool FormulaTeam::operator==(const FormulaTeam &other) const {
  return teamName == other.teamName;
}

bool FormulaTeam::operator>(const FormulaTeam &other) const {
  return championshipPoints > other.championshipPoints;
}

bool FormulaTeam::operator<(const FormulaTeam &other) const {
  return championshipPoints < other.championshipPoints;
}

std::ostream &operator<<(std::ostream &os, const FormulaTeam &team) {
  os << "┌─────────────────────────────────────────┐\n"
     << "│  " << std::left << std::setw(40) << team.teamName << "│\n"
     << "│  Principal : " << std::setw(28) << team.principalName << "│\n"
     << "│  HQ        : " << std::setw(28) << team.headquarters << "│\n"
     << "│  Series    : " << std::setw(28) << team.getSeriesName() << "│\n"
     << "│  Points    : " << std::setw(28) << team.championshipPoints << "│\n"
     << "└─────────────────────────────────────────┘";
  return os;
}
