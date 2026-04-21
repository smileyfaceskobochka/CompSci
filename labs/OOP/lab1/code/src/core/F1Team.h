#pragma once
#include "FormulaTeam.h"

class F1Team : public FormulaTeam {
private:
  std::string powerUnit; // Engine supplier (e.g. "Mercedes", "Honda RBPT")
  double budgetCapMln;   // Budget cap in million USD
  int constructorPos;    // Current constructor championship position

public:

  F1Team();
  F1Team(const std::string &name, const std::string &principal,
         const std::string &hq, const std::string &powerUnit);
  F1Team(const std::string &name, const std::string &principal,
         const std::string &hq, const std::string &powerUnit, double budget,
         int ctorPos, int points, int wins, int podiums);


  std::string getPowerUnit() const { return powerUnit; }
  double getBudgetCapMln() const { return budgetCapMln; }
  int getConstructorPos() const { return constructorPos; }

  void setPowerUnit(const std::string &pu) { powerUnit = pu; }
  void setBudgetCapMln(double b) { budgetCapMln = b; }
  void setConstructorPos(int pos) { constructorPos = pos; }


  double calculateBudgetAllocation(double percentage) const;
  std::string getPowerUnitStatus() const;


  void displayInfo() const override;
  std::string getSeriesName() const override;
  int calculatePoints(int position) const override;
  void displayStats() const override;
  std::string getSummary() const override;
};