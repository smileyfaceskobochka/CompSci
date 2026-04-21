#include "F2Team.h"
#include <iostream>
#include <sstream>



F2Team::F2Team()
    : FormulaTeam(), chassisModel("Dallara F2 2024"), f1Graduates(0), isFeederSeries(true) {}

F2Team::F2Team(const std::string& name, const std::string& principal,
               const std::string& hq,   const std::string& chassis)
    : FormulaTeam(name, principal, hq),
      chassisModel(chassis), f1Graduates(0), isFeederSeries(true) {}

F2Team::F2Team(const std::string& name, const std::string& principal,
               const std::string& hq,   const std::string& chassis,
               int graduates,           bool feeder,
               int points,              int wins,  int pods)
    : FormulaTeam(name, principal, hq, points, wins, pods),
      chassisModel(chassis), f1Graduates(graduates), isFeederSeries(feeder) {}



void F2Team::addGraduate() {
    ++f1Graduates;
}

std::string F2Team::getFeederStatus() const {
    if (!isFeederSeries)       return "Independent";
    if (f1Graduates >= 5)      return "Elite Feeder (5+ F1 graduates)";
    if (f1Graduates >= 2)      return "Active Feeder";
    return "Developing Feeder";
}



void F2Team::displayInfo() const {
    std::cout << "\033[1;33m[Formula 2]\033[0m " << teamName << "\n"
              << "  Principal    : " << principalName     << "\n"
              << "  HQ           : " << headquarters       << "\n"
              << "  Chassis      : " << chassisModel        << "\n"
              << "  F1 Graduates : " << f1Graduates         << "\n"
              << "  Feeder Status: " << getFeederStatus()   << "\n";
    FormulaTeam::displayStats();
}

std::string F2Team::getSeriesName() const {
    return "Formula 2";
}

// F2 sprint / feature race combined simplified scoring
int F2Team::calculatePoints(int position) const {
    const int F2_POINTS[] = {15, 12, 10, 8, 6, 5, 4, 3, 2, 1};
    if (position >= 1 && position <= 10)
        return F2_POINTS[position - 1];
    return 0;
}

void F2Team::displayStats() const {
    FormulaTeam::displayStats();
    std::cout << "  Feeder Status : " << getFeederStatus() << "\n";
}

std::string F2Team::getSummary() const {
    std::ostringstream oss;
    oss << FormulaTeam::getSummary()
        << " | Chassis: " << chassisModel
        << " | F1 Grads: " << f1Graduates;
    return oss.str();
}
