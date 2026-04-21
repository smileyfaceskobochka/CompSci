using System;

namespace RacingSystem.Core
{
    public class F2Team : FormulaTeam
    {
        public string ChassisModel { get; set; } = "Dallara F2 2024";
        public int F1Graduates { get; set; }
        public bool IsFeederSeries { get; set; } = true;

        public F2Team() : base() { }

        public F2Team(string name, string principal, string hq, string chassis)
            : base(name, principal, hq)
        {
            ChassisModel = chassis;
        }

        public F2Team(string name, string principal, string hq, string chassis, int graduates, bool feeder, int points, int wins, int podiums)
            : base(name, principal, hq, points, wins, podiums)
        {
            ChassisModel = chassis;
            F1Graduates = graduates;
            IsFeederSeries = feeder;
        }

        public override string GetSeriesName() => "Formula 2";

        public override int CalculatePoints(int position)
        {
            int[] points = { 15, 12, 10, 8, 6, 5, 4, 3, 2, 1 };
            return (position >= 1 && position <= 10) ? points[position - 1] : 0;
        }

        public string GetFeederStatus()
        {
            if (!IsFeederSeries) return "Independent";
            if (F1Graduates >= 5) return "Elite Feeder (5+ F1 graduates)";
            if (F1Graduates >= 2) return "Active Feeder";
            return "Developing Feeder";
        }

        public override string GetSummary()
        {
            return $"{base.GetSummary()} | Chassis: {ChassisModel} | F1 Grads: {F1Graduates}";
        }
    }
}
