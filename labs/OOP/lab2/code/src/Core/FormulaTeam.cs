using System;
using System.Collections.Generic;

namespace RacingSystem.Core
{
    public abstract class FormulaTeam
    {
        // Enforce encapsulation with Properties
        public int Id { get; set; }
        public string TeamName { get; set; } = "Unknown";
        public string PrincipalName { get; set; } = "Unknown";
        public string Headquarters { get; set; } = "Unknown";
        public int ChampionshipPoints { get; set; }
        public int RaceWins { get; set; }
        public int Podiums { get; set; }
        public List<FormulaDriver> Drivers { get; set; } = new List<FormulaDriver>();

        protected FormulaTeam() { }

        protected FormulaTeam(string name, string principal, string hq)
        {
            TeamName = name;
            PrincipalName = principal;
            Headquarters = hq;
        }

        protected FormulaTeam(string name, string principal, string hq, int points, int wins, int podiums)
            : this(name, principal, hq)
        {
            ChampionshipPoints = points;
            RaceWins = wins;
            Podiums = podiums;
        }

        // Logic methods from Lab 1
        public void AddPoints(int pts)
        {
            if (pts > 0) ChampionshipPoints += pts;
        }

        public void AddWin()
        {
            RaceWins++;
            Podiums++;
        }

        public void AddPodium() => Podiums++;

        public double GetWinRate(int totalRaces)
        {
            if (totalRaces <= 0) return 0.0;
            return (double)RaceWins / totalRaces * 100.0;
        }

        // Abstract methods (requirement from Lab 1)
        // Logic methods
        public abstract string GetSeriesName();
        public abstract int CalculatePoints(int position);

        public virtual string GetSummary()
        {
            return $"[{GetSeriesName()}] {TeamName} | Pts: {ChampionshipPoints} | Wins: {RaceWins}";
        }

        // Properties for UI binding
        public string SeriesName => GetSeriesName();
        public string Summary => GetSummary();

        // Operator overloading equivalents (C# supports operator overloading)
        public static bool operator >(FormulaTeam a, FormulaTeam b) => a.ChampionshipPoints > b.ChampionshipPoints;
        public static bool operator <(FormulaTeam a, FormulaTeam b) => a.ChampionshipPoints < b.ChampionshipPoints;

        public override string ToString() => GetSummary();
    }
}
