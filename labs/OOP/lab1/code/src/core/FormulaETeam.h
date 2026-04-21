#pragma once
#include "FormulaTeam.h"

class FormulaETeam : public FormulaTeam {
private:
  int sustainabilityScore;   // 0–100 ESG / green score
  double batteryCapacityKwh; // Usable battery capacity in kWh
  std::string energyPartner; // Energy / tech sponsor

public:

  FormulaETeam();
  FormulaETeam(const std::string &name, const std::string &principal,
               const std::string &hq, const std::string &energyPartner);
  FormulaETeam(const std::string &name, const std::string &principal,
               const std::string &hq, const std::string &energyPartner,
               int sustainScore, double batteryKwh, int points, int wins,
               int podiums);


  int getSustainabilityScore() const { return sustainabilityScore; }
  double getBatteryCapacityKwh() const { return batteryCapacityKwh; }
  std::string getEnergyPartner() const { return energyPartner; }

  void setSustainabilityScore(int s) { sustainabilityScore = s; }
  void setBatteryCapacityKwh(double b) { batteryCapacityKwh = b; }
  void setEnergyPartner(const std::string &e) { energyPartner = e; }


  void updateSustainabilityScore(int delta);
  std::string getBatteryEfficiencyRating() const;


  void displayInfo() const override;
  std::string getSeriesName() const override;
  int calculatePoints(int position) const override;
  void displayStats() const override;
  std::string getSummary() const override;
};
