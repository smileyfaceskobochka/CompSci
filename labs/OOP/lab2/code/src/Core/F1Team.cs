using System;

namespace RacingSystem.Core
{
    public class F1Team : FormulaTeam
    {
        public string PowerUnit { get; set; } = "Unknown";
        public double BudgetCapMln { get; set; } = 135.0;
        public int ConstructorPos { get; set; }

        public F1Team() : base() { }

        public F1Team(string name, string principal, string hq, string powerUnit)
            : base(name, principal, hq)
        {
            PowerUnit = powerUnit;
        }

        public F1Team(string name, string principal, string hq, string powerUnit, double budget, int ctorPos, int points, int wins, int podiums)
            : base(name, principal, hq, points, wins, podiums)
        {
            PowerUnit = powerUnit;
            BudgetCapMln = budget;
            ConstructorPos = ctorPos;
        }

        public override string GetSeriesName() => "Formula 1";

        public override int CalculatePoints(int position)
        {
            int[] points = { 25, 18, 15, 12, 10, 8, 6, 4, 2, 1 };
            return (position >= 1 && position <= 10) ? points[position - 1] : 0;
        }

        public double CalculateBudgetAllocation(double percentage) => BudgetCapMln * (percentage / 100.0);

        public string GetPowerUnitStatus()
        {
            if (RaceWins > 10) return $"{PowerUnit} [Dominant]";
            if (RaceWins > 4) return $"{PowerUnit} [Competitive]";
            return $"{PowerUnit} [Development]";
        }

        [System.Text.Json.Serialization.JsonPropertyName("engine_status")]
        public string EngineStatus => GetPowerUnitStatus();

        public override string GetSummary()
        {
            return $"{base.GetSummary()} | PU: {PowerUnit} | Budget: ${BudgetCapMln}M";
        }
    }
}
