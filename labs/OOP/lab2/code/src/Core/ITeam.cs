using System.Collections.Generic;

namespace RacingSystem.Core
{
  public interface ITeam
  {
    int Id { get; set; }
    string TeamName { get; set; }
    string PrincipalName { get; set; }
    string Headquarters { get; set; }
    string? TeamColor { get; set; }
    int ChampionshipPoints { get; set; }
    int RaceWins { get; set; }
    int Podiums { get; set; }
    List<FormulaDriver> Drivers { get; set; }

    void AddPoints(int pts);
    void AddWin();
    void AddPodium();
    double GetWinRate(int totalRaces);
    string GetSeriesName();
    int CalculatePoints(int position);
    string GetSummary();
  }
}
