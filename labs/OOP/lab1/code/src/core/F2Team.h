#pragma once
#include "FormulaTeam.h"

class F2Team : public FormulaTeam {
private:
  std::string chassisModel; // Spec chassis model (all teams use same chassis)
  int f1Graduates;          // Number of drivers who graduated to F1
  bool isFeederSeries;      // Whether actively used as F1 feeder

public:

  F2Team();
  F2Team(const std::string &name, const std::string &principal,
         const std::string &hq, const std::string &chassis);
  F2Team(const std::string &name, const std::string &principal,
         const std::string &hq, const std::string &chassis, int graduates,
         bool feeder, int points, int wins, int podiums);


  std::string getChassisModel() const { return chassisModel; }
  int getF1Graduates() const { return f1Graduates; }
  bool getIsFeederSeries() const { return isFeederSeries; }

  void setChassisModel(const std::string &c) { chassisModel = c; }
  void setF1Graduates(int g) { f1Graduates = g; }
  void setIsFeederSeries(bool f) { isFeederSeries = f; }


  void addGraduate();
  std::string getFeederStatus() const;


  void displayInfo() const override;
  std::string getSeriesName() const override;
  int calculatePoints(int position) const override;
  void displayStats() const override;
  std::string getSummary() const override;
};
