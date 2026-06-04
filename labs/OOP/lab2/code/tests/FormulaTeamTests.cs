using System;
using Xunit;
using RacingSystem.Core;

namespace RacingSystem.Tests
{
    public class FormulaTeamTests
    {
        [Fact]
        public void F1Team_CalculatePoints_Correct()
        {
            var team = new F1Team();
            Assert.Equal(25, team.CalculatePoints(1));
            Assert.Equal(18, team.CalculatePoints(2));
            Assert.Equal(15, team.CalculatePoints(3));
            Assert.Equal(12, team.CalculatePoints(4));
            Assert.Equal(10, team.CalculatePoints(5));
            Assert.Equal(8, team.CalculatePoints(6));
            Assert.Equal(6, team.CalculatePoints(7));
            Assert.Equal(4, team.CalculatePoints(8));
            Assert.Equal(2, team.CalculatePoints(9));
            Assert.Equal(1, team.CalculatePoints(10));
            Assert.Equal(0, team.CalculatePoints(11));
            Assert.Equal(0, team.CalculatePoints(0));
            Assert.Equal(0, team.CalculatePoints(-5));
        }

        [Fact]
        public void F2Team_CalculatePoints_Correct()
        {
            var team = new F2Team();
            Assert.Equal(15, team.CalculatePoints(1));
            Assert.Equal(12, team.CalculatePoints(2));
            Assert.Equal(10, team.CalculatePoints(3));
            Assert.Equal(8, team.CalculatePoints(4));
            Assert.Equal(6, team.CalculatePoints(5));
            Assert.Equal(5, team.CalculatePoints(6));
            Assert.Equal(4, team.CalculatePoints(7));
            Assert.Equal(3, team.CalculatePoints(8));
            Assert.Equal(2, team.CalculatePoints(9));
            Assert.Equal(1, team.CalculatePoints(10));
            Assert.Equal(0, team.CalculatePoints(11));
            Assert.Equal(0, team.CalculatePoints(0));
        }

        [Fact]
        public void FormulaETeam_CalculatePoints_Correct()
        {
            var team = new FormulaETeam();
            Assert.Equal(25, team.CalculatePoints(1));
            Assert.Equal(18, team.CalculatePoints(2));
            Assert.Equal(15, team.CalculatePoints(3));
            Assert.Equal(1, team.CalculatePoints(10));
            Assert.Equal(0, team.CalculatePoints(11));
        }

        [Fact]
        public void GetSummary_ReturnsCorrectFormat()
        {
            var f1 = new F1Team("Ferrari", "Vasseur", "Maranello", "Ferrari", 135.0, 3, 200, 1, 4);
            var f2 = new F2Team("PREMA", "Rosin", "Grisignano", "Dallara", 3, true, 80, 2, 5);
            var fe = new FormulaETeam("Porsche", "Modlinger", "Weissach", "Porsche", 80, 38.5, 90, 3, 6);

            Assert.Contains("[Formula 1] Ferrari | Pts: 200 | Wins: 1", f1.GetSummary());
            Assert.Contains("PU: Ferrari | Budget: $135M", f1.GetSummary());

            Assert.Contains("[Formula 2] PREMA | Pts: 80 | Wins: 2", f2.GetSummary());
            Assert.Contains("Chassis: Dallara | F1 Grads: 3", f2.GetSummary());

            Assert.Contains("[Formula E] Porsche | Pts: 90 | Wins: 3", fe.GetSummary());
            Assert.Contains("Eco: 80/100 | Battery: 38.5 kWh", fe.GetSummary());
        }

        [Fact]
        public void OperatorOverloading_ComparesChampionshipPoints()
        {
            var teamA = new F1Team { ChampionshipPoints = 150 };
            var teamB = new F1Team { ChampionshipPoints = 100 };

            Assert.True(teamA > teamB);
            Assert.False(teamA < teamB);
            Assert.True(teamB < teamA);
        }

        [Fact]
        public void AddPoints_IncrementsCorrectly()
        {
            var team = new F1Team { ChampionshipPoints = 50 };
            team.AddPoints(25);
            Assert.Equal(75, team.ChampionshipPoints);

            team.AddPoints(-5); // Negatives should be ignored
            Assert.Equal(75, team.ChampionshipPoints);
        }

        [Fact]
        public void FormulaDriver_CreationAndProperties()
        {
            var driver = new FormulaDriver("Lewis", "Hamilton", 44, 100)
            {
                Id = 1,
                TeamId = 2
            };

            Assert.Equal(1, driver.Id);
            Assert.Equal(2, driver.TeamId);
            Assert.Equal("Lewis", driver.FirstName);
            Assert.Equal("Hamilton", driver.LastName);
            Assert.Equal(44, driver.RacingNumber);
            Assert.Equal(100, driver.Points);
            Assert.Equal("Lewis Hamilton", driver.FullName);
            Assert.Equal("Lewis Hamilton #44", driver.DisplayString);

            driver.AddPoints(10);
            Assert.Equal(110, driver.Points);
        }

        [Fact]
        public void GetWinRate_CalculatesCorrectPercentage()
        {
            var team = new F1Team { RaceWins = 5 };
            Assert.Equal(50.0, team.GetWinRate(10));
            Assert.Equal(25.0, team.GetWinRate(20));
        }

        [Fact]
        public void GetWinRate_HandlesZeroOrNegativeRaces()
        {
            var team = new F1Team { RaceWins = 5 };
            Assert.Equal(0.0, team.GetWinRate(0));
            Assert.Equal(0.0, team.GetWinRate(-10));
        }

        [Fact]
        public void AddWin_IncrementsWinsAndPodiums()
        {
            var team = new F1Team { RaceWins = 2, Podiums = 4 };
            team.AddWin();
            Assert.Equal(3, team.RaceWins);
            Assert.Equal(5, team.Podiums);
        }

        [Fact]
        public void AddPodium_IncrementsPodiumsOnly()
        {
            var team = new F1Team { RaceWins = 2, Podiums = 4 };
            team.AddPodium();
            Assert.Equal(2, team.RaceWins);
            Assert.Equal(5, team.Podiums);
        }

        [Fact]
        public void F2Team_GetFeederStatus_Correct()
        {
            var team = new F2Team { IsFeederSeries = false };
            Assert.Equal("Independent", team.GetFeederStatus());

            team.IsFeederSeries = true;
            team.F1Graduates = 1;
            Assert.Equal("Developing Feeder", team.GetFeederStatus());

            team.F1Graduates = 3;
            Assert.Equal("Active Feeder", team.GetFeederStatus());

            team.F1Graduates = 6;
            Assert.Equal("Elite Feeder (5+ F1 graduates)", team.GetFeederStatus());
        }

        [Fact]
        public void FormulaETeam_GetBatteryEfficiencyRating_Correct()
        {
            var team = new FormulaETeam { BatteryCapacityKwh = 35.0 };
            Assert.Equal("Legacy", team.GetBatteryEfficiencyRating());

            team.BatteryCapacityKwh = 39.0;
            Assert.Equal("Gen3 Standard (38 kWh)", team.GetBatteryEfficiencyRating());

            team.BatteryCapacityKwh = 42.0;
            Assert.Equal("Next-Gen (40+ kWh)", team.GetBatteryEfficiencyRating());
        }
    }
}
