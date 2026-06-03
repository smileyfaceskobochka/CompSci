#pragma once
#include <iostream>
#include <string>
#include <vector>
#include "FormulaDriver.h"

// Abstract base class for all formula racing teams
class FormulaTeam {
protected:
  std::string teamName;
  std::string principalName;
  std::string headquarters;
  int championshipPoints;
  int raceWins;
  int podiums;
  std::vector<FormulaDriver> drivers;

public:

  FormulaTeam();
  FormulaTeam(const std::string &name, const std::string &principal,
              const std::string &hq);
  FormulaTeam(const std::string &name, const std::string &principal,
              const std::string &hq, int points, int wins, int podiums);
  virtual ~FormulaTeam() = default;


  std::string getTeamName() const { return teamName; }
  std::string getPrincipalName() const { return principalName; }
  std::string getHeadquarters() const { return headquarters; }
  int getChampionshipPoints() const { return championshipPoints; }
  int getRaceWins() const { return raceWins; }
  int getPodiums() const { return podiums; }


  void setTeamName(const std::string &name) { teamName = name; }
  void setPrincipalName(const std::string &principal) {
    principalName = principal;
  }
  void setHeadquarters(const std::string &hq) { headquarters = hq; }
  void setChampionshipPoints(int pts) { championshipPoints = pts; }
  void setRaceWins(int wins) { raceWins = wins; }
  void setPodiums(int p) { podiums = p; }


  void addPoints(int pts);
  void addWin();
  void addPodium();
  void resetSeason();
  double getWinRate(int totalRaces) const;

  // Driver Management
  void addDriver(const FormulaDriver &driver);
  const std::vector<FormulaDriver> &getDrivers() const;
  std::vector<FormulaDriver> &getDriversRef();

  // Pure virtual methods
  virtual void displayInfo() const = 0;
  virtual std::string getSeriesName() const = 0;
  virtual int calculatePoints(int position) const = 0;

  // Virtual methods
  virtual void displayStats() const;
  virtual std::string getSummary() const;


  FormulaTeam &operator+=(int pts);
  FormulaTeam &operator++(); // prefix ++ (adds a win)
  bool operator==(const FormulaTeam &other) const;
  bool operator>(const FormulaTeam &other) const;
  bool operator<(const FormulaTeam &other) const;

  friend std::ostream &operator<<(std::ostream &os, const FormulaTeam &team);
};