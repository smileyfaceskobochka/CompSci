using System;

namespace RacingSystem.Core
{
  public class FormulaETeam : FormulaTeam
  {
    public int SustainabilityScore { get; set; } = 50;
    public double BatteryCapacityKwh { get; set; } = 38.0;
    public string EnergyPartner { get; set; } = "Unknown";

    public FormulaETeam() : base() { }

    public FormulaETeam(string name, string principal, string hq, string energyPartner)
        : base(name, principal, hq)
    {
      EnergyPartner = energyPartner;
    }

    public FormulaETeam(string name, string principal, string hq, string energyPartner, int sustainScore, double batteryKwh, int points, int wins, int podiums)
        : base(name, principal, hq, points, wins, podiums)
    {
      EnergyPartner = energyPartner;
      SustainabilityScore = sustainScore;
      BatteryCapacityKwh = batteryKwh;
    }

    public override string GetSeriesName() => "Formula E";

    public override int CalculatePoints(int position)
    {
      int[] points = { 25, 18, 15, 12, 10, 8, 6, 4, 2, 1 };
      return (position >= 1 && position <= 10) ? points[position - 1] : 0;
    }

    public string GetBatteryEfficiencyRating()
    {
      if (BatteryCapacityKwh >= 40.0) return "Next-Gen (40+ kWh)";
      if (BatteryCapacityKwh >= 38.0) return "Gen3 Standard (38 kWh)";
      return "Legacy";
    }

    [System.Text.Json.Serialization.JsonPropertyName("battery_rating_text")]
    public string BatteryRatingText => GetBatteryEfficiencyRating();

    public override string GetSummary()
    {
      return $"{base.GetSummary()} | Eco: {SustainabilityScore}/100 | Battery: {BatteryCapacityKwh} kWh";
    }
  }
}
