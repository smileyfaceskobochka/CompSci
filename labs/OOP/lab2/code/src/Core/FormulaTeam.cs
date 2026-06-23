using System;
using System.Collections.Generic;
using System.Text.Json.Serialization;

namespace RacingSystem.Core
{
  [JsonPolymorphic(TypeDiscriminatorPropertyName = "type")]
  [JsonDerivedType(typeof(F1Team), "f1")]
  [JsonDerivedType(typeof(F2Team), "f2")]
  [JsonDerivedType(typeof(FormulaETeam), "fe")]
  public abstract class FormulaTeam : ITeam
  {
    // Enforce encapsulation with Properties
    public int Id { get; set; }
    public string TeamName { get; set; } = "Unknown";
    public string PrincipalName { get; set; } = "Unknown";
    public string Headquarters { get; set; } = "Unknown";
    [JsonPropertyName("team_color")]
    public string? TeamColor { get; set; }
    public int ChampionshipPoints { get; set; }
    public int RaceWins { get; set; }
    public int Podiums { get; set; }
    public List<FormulaDriver> Drivers { get; set; } = new List<FormulaDriver>();

    protected FormulaTeam() : this("Unknown", "Unknown", "Unknown") { }

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

    [JsonPropertyName("visible_color")]
    public string VisibleColor => GetVisibleTeamColor(TeamColor);

    private static string GetVisibleTeamColor(string? hex)
    {
      if (string.IsNullOrWhiteSpace(hex)) return "#3b82f6";

      string cleanHex = hex.Replace("#", "").Trim();
      if (cleanHex.Equals("000000", StringComparison.OrdinalIgnoreCase) ||
          cleanHex.Equals("000", StringComparison.OrdinalIgnoreCase) ||
          cleanHex.Equals("1E293B", StringComparison.OrdinalIgnoreCase) ||
          cleanHex.Equals("18181b", StringComparison.OrdinalIgnoreCase))
      {
        return "#a1a1aa";
      }

      if (cleanHex.Length == 3)
      {
        cleanHex = $"{cleanHex[0]}{cleanHex[0]}" +
                   $"{cleanHex[1]}{cleanHex[1]}" +
                   $"{cleanHex[2]}{cleanHex[2]}";
      }

      if (cleanHex.Length != 6) return hex;

      try
      {
        int rVal = Convert.ToInt32(cleanHex.Substring(0, 2), 16);
        int gVal = Convert.ToInt32(cleanHex.Substring(2, 2), 16);
        int bVal = Convert.ToInt32(cleanHex.Substring(4, 2), 16);

        double r = rVal / 255.0;
        double g = gVal / 255.0;
        double b = bVal / 255.0;

        double luminance = 0.2126 * r + 0.7152 * g + 0.0722 * b;

        if (luminance < 0.35)
        {
          double max = Math.Max(r, Math.Max(g, b));
          double min = Math.Min(r, Math.Min(g, b));
          double h = 0, s = 0, l = (max + min) / 2.0;

          if (max != min)
          {
            double d = max - min;
            s = l > 0.5 ? d / (2.0 - max - min) : d / (max + min);
            if (max == r)
            {
              h = (g - b) / d + (g < b ? 6.0 : 0.0);
            }
            else if (max == g)
            {
              h = (b - r) / d + 2.0;
            }
            else if (max == b)
            {
              h = (r - g) / d + 4.0;
            }
            h /= 6.0;
          }

          l = Math.Max(l, 0.55);
          s = Math.Max(s, 0.65);

          double Hue2Rgb(double pVal, double qVal, double tVal)
          {
            if (tVal < 0) tVal += 1.0;
            if (tVal > 1.0) tVal -= 1.0;
            if (tVal < 1.0 / 6.0) return pVal + (qVal - pVal) * 6.0 * tVal;
            if (tVal < 1.0 / 2.0) return qVal;
            if (tVal < 2.0 / 3.0) return pVal + (qVal - pVal) * (2.0 / 3.0 - tVal) * 6.0;
            return pVal;
          }

          double q = l < 0.5 ? l * (1.0 + s) : l + s - l * s;
          double p = 2.0 * l - q;

          r = Hue2Rgb(p, q, h + 1.0 / 3.0);
          g = Hue2Rgb(p, q, h);
          b = Hue2Rgb(p, q, h - 1.0 / 3.0);

          string ToHex(double val)
          {
            int intVal = (int)Math.Round(val * 255.0);
            return intVal.ToString("X2").ToLower();
          }

          return $"#{ToHex(r)}{ToHex(g)}{ToHex(b)}";
        }
      }
      catch
      {
        return hex;
      }

      return hex;
    }

    // Operator overloading 
    public static bool operator >(FormulaTeam a, FormulaTeam b) => a.ChampionshipPoints > b.ChampionshipPoints;
    public static bool operator <(FormulaTeam a, FormulaTeam b) => a.ChampionshipPoints < b.ChampionshipPoints;

    public override string ToString() => GetSummary();
  }
}
