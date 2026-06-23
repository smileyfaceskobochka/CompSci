using System;
using System.IO;
using System.Net;
using System.Text;
using System.Text.Json;
using System.Linq;
using System.Threading.Tasks;
using System.Collections.Generic;
using RacingSystem.Core;
using RacingSystem.Database;

namespace RacingSystem;

public class Program
{
  private static DatabaseManager db = new DatabaseManager();
  private static readonly JsonSerializerOptions jsonOptions = new JsonSerializerOptions
  {
    PropertyNamingPolicy = JsonNamingPolicy.CamelCase
  };

  public static void Main(string[] args)
  {
    // HttpListener 
    var listener = new HttpListener();
    listener.Prefixes.Add("http://localhost:5070/");

    try
    {
      listener.Start();
      Console.WriteLine("[RUN] Native C# Server running at http://localhost:5070/");
    }
    catch (Exception ex)
    {
      Console.WriteLine("CRITICAL: Failed to start HttpListener: " + ex.Message);
      return;
    }

    while (true)
    {
      try
      {
        var context = listener.GetContext();
        Task.Run(() => ProcessRequest(context));
      }
      catch (Exception ex)
      {
        Console.WriteLine("Error dispatching context: " + ex.Message);
      }
    }
  }

  private static void ProcessRequest(HttpListenerContext context)
  {
    var request = context.Request;
    var response = context.Response;

    // Set CORS headers
    response.Headers.Add("Access-Control-Allow-Origin", "*");
    response.Headers.Add("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS");
    response.Headers.Add("Access-Control-Allow-Headers", "Content-Type, Accept");

    if (request.HttpMethod == "OPTIONS")
    {
      response.StatusCode = 204;
      response.Close();
      return;
    }

    string path = request.Url?.AbsolutePath ?? "/";
    string method = request.HttpMethod;

    Console.WriteLine($"[HTTP] {method} {path}");

    try
    {
      var pathParts = path.Split('/', StringSplitOptions.RemoveEmptyEntries);

      // Routes matching: /api/teams <--
      if (pathParts.Length == 2 && pathParts[0] == "api" && pathParts[1] == "teams")
      {
        if (method == "GET")
        {
          var teams = db.GetAllTeams();
          SendJson(response, teams);
          return;
        }
        else if (method == "POST")
        {
          string body = ReadBody(request);
          var team = JsonSerializer.Deserialize<FormulaTeam>(body, jsonOptions);
          if (team == null) throw new ArgumentException("Invalid team payload");
          db.SaveTeam(team);
          SendJson(response, team, 201);
          return;
        }
      }

      // Routes matching: /api/teams/{id} <--
      if (pathParts.Length == 3 && pathParts[0] == "api" && pathParts[1] == "teams" && int.TryParse(pathParts[2], out int teamId))
      {
        if (method == "PUT")
        {
          string body = ReadBody(request);
          var team = JsonSerializer.Deserialize<FormulaTeam>(body, jsonOptions);
          if (team == null) throw new ArgumentException("Invalid team payload");
          team.Id = teamId;
          db.SaveTeam(team);
          SendJson(response, team);
          return;
        }
        else if (method == "DELETE")
        {
          db.DeleteTeam(teamId);
          response.StatusCode = 204;
          response.Close();
          return;
        }
      }

      // Routes matching: /api/teams/{id}/simulate-race
      if (pathParts.Length == 4 && pathParts[0] == "api" && pathParts[1] == "teams" && int.TryParse(pathParts[2], out int simTeamId) && pathParts[3] == "simulate-race")
      {
        if (method == "POST")
        {
          var query = request.QueryString;
          int position = int.TryParse(query["position"], out int pos) ? pos : 1;
          int simDriverId = int.TryParse(query["driverId"], out int dId) ? dId : 0;

          var teams = db.GetAllTeams();
          var team = teams.FirstOrDefault(t => t.Id == simTeamId);
          if (team == null)
          {
            SendError(response, $"Team with ID {simTeamId} not found.", 404);
            return;
          }

          var driver = team.Drivers.FirstOrDefault(d => d.Id == simDriverId);
          if (driver == null)
          {
            SendError(response, "A valid driver signed with this team must be selected.", 400);
            return;
          }

          int points = team.CalculatePoints(position);

          // OOP update methods on both Driver and Team
          driver.AddPoints(points);
          team.AddPoints(points);

          if (position == 1)
          {
            team.AddWin();
          }
          else if (position <= 3)
          {
            team.AddPodium();
          }

          db.SaveDriver(driver);
          db.SaveTeam(team);

          var result = new
          {
            message = $"Simulated position P{position} for {driver.FirstName} {driver.LastName} (#{driver.RacingNumber}). Calculated {points} pts added to driver and team.",
            teamSummary = team.GetSummary(),
            team = team
          };
          SendJson(response, result);
          return;
        }
      }

      // Routes matching: /api/teams/compare
      if (pathParts.Length == 3 && pathParts[0] == "api" && pathParts[1] == "teams" && pathParts[2] == "compare")
      {
        if (method == "GET")
        {
          var query = request.QueryString;
          int teamIdA = int.TryParse(query["teamIdA"], out int idA) ? idA : 0;
          int teamIdB = int.TryParse(query["teamIdB"], out int idB) ? idB : 0;

          var teams = db.GetAllTeams();
          var teamA = teams.FirstOrDefault(t => t.Id == teamIdA);
          var teamB = teams.FirstOrDefault(t => t.Id == teamIdB);

          if (teamA == null || teamB == null)
          {
            SendError(response, "One or both teams not found.", 400);
            return;
          }

          bool aIsGreater = teamA > teamB;
          bool bIsGreater = teamA < teamB;

          string resultMessage = aIsGreater
              ? $"{teamA.TeamName} is ahead of {teamB.TeamName} in the championship."
              : bIsGreater
                  ? $"{teamB.TeamName} is ahead of {teamA.TeamName} in the championship."
                  : "Both teams have equal championship points.";

          var result = new
          {
            message = resultMessage,
            teamA = new { id = teamA.Id, name = teamA.TeamName, points = teamA.ChampionshipPoints },
            teamB = new { id = teamB.Id, name = teamB.TeamName, points = teamB.ChampionshipPoints },
            winnerId = aIsGreater ? teamA.Id : (bIsGreater ? teamB.Id : 0)
          };
          SendJson(response, result);
          return;
        }
      }

      // Routes matching: /api/simulation/race
      if (pathParts.Length == 3 && pathParts[0] == "api" && pathParts[1] == "simulation" && pathParts[2] == "race")
      {
        if (method == "POST")
        {
          string body = ReadBody(request);
          var raceResults = JsonSerializer
              .Deserialize<List<DriverPositionResult>>(body, jsonOptions);
          if (raceResults == null) throw new ArgumentException("Invalid race results payload");

          var teams = db.GetAllTeams();

          foreach (var result in raceResults)
          {
            Console.WriteLine($"[SIM] Processing driver ID {result.DriverId} at position {result.Position}");
            FormulaDriver? driver = null;
            FormulaTeam? team = null;

            foreach (var t in teams)
            {
              var d = t.Drivers.FirstOrDefault(drv => drv.Id == result.DriverId);
              if (d != null)
              {
                driver = d;
                team = t;
                break;
              }
            }

            if (driver == null || team == null)
            {
              Console.WriteLine($"[SIM] Driver ID {result.DriverId} or their team not found in memory!");
              continue;
            }

            // Calculate points via C# polymorphic subclass rule
            int points = team.CalculatePoints(result.Position);
            Console.WriteLine($"[SIM] Driver {driver.FullName} (# {driver.RacingNumber}) of team {team.TeamName} finished P{result.Position}. Calculated {points} points.");

            driver.AddPoints(points);
            team.AddPoints(points);

            if (result.Position == 1)
            {
              team.AddWin();
            }
            else if (result.Position <= 3)
            {
              team.AddPodium();
            }

            db.SaveTeam(team);
            Console.WriteLine($"[SIM] Saved team {team.TeamName} (new points: {team.ChampionshipPoints}) and driver {driver.FullName} (new points: {driver.Points})");
          }

          SendJson(response, new { message = "Global race simulation completed successfully. Driver and team standings updated." });
          return;
        }
      }

      // Routes matching: /api/stats
      if (pathParts.Length == 2 && pathParts[0] == "api" && pathParts[1] == "stats")
      {
        if (method == "GET")
        {
          var query = request.QueryString;
          string series = query["series"] ?? "all";

          var teams = db.GetAllTeams();
          var drivers = db.GetAllDrivers();

          bool MatchesSeries(FormulaTeam t, string ser)
          {
            if (ser == "all") return true;
            if (ser == "f1" && t is F1Team) return true;
            if (ser == "f2" && t is F2Team) return true;
            if (ser == "fe" && t is FormulaETeam) return true;
            return false;
          }

          var filteredTeams = teams.Where(t => MatchesSeries(t, series)).ToList();
          var filteredDrivers = drivers.Where(d =>
          {
            var t = teams.FirstOrDefault(team => team.Id == d.TeamId);
            return t != null && MatchesSeries(t, series);
          }).ToList();

          var leadingTeam = filteredTeams.OrderByDescending(t => t.ChampionshipPoints).FirstOrDefault();
          var leadingDriver = filteredDrivers.OrderByDescending(d => d.Points).FirstOrDefault();

          double avgPoints = filteredDrivers.Count > 0
              ? filteredDrivers.Average(d => d.Points)
              : 0.0;

          var stats = new
          {
            registeredTeamsCount = filteredTeams.Count,
            activeDriversCount = filteredDrivers.Count,
            averagePoints = Math.Round(avgPoints, 1),
            leadingTeamName = leadingTeam?.TeamName ?? "N/A",
            leadingTeamPoints = leadingTeam?.ChampionshipPoints ?? 0,
            leadingTeamWins = leadingTeam?.RaceWins ?? 0,
            leadingDriverName = leadingDriver != null ? $"{leadingDriver.FirstName} {leadingDriver.LastName}" : "N/A",
            leadingDriverPoints = leadingDriver?.Points ?? 0,
            leadingDriverNumber = leadingDriver?.RacingNumber ?? 0
          };

          SendJson(response, stats);
          return;
        }
      }

      // Routes matching: /api/status
      if (pathParts.Length == 2 && pathParts[0] == "api" && pathParts[1] == "status")
      {
        if (method == "GET")
        {
          SendJson(response, new { lab = "lab2", name = "C# (.NET)" });
          return;
        }
      }

      // Routes matching: /api/drivers
      if (pathParts.Length == 2 && pathParts[0] == "api" && pathParts[1] == "drivers")
      {
        if (method == "GET")
        {
          var drivers = db.GetAllDrivers();
          SendJson(response, drivers);
          return;
        }
        else if (method == "POST")
        {
          string body = ReadBody(request);
          var driver = JsonSerializer.Deserialize<FormulaDriver>(body, jsonOptions);
          if (driver == null) throw new ArgumentException("Invalid driver payload");
          db.SaveDriver(driver);
          SendJson(response, driver, 201);
          return;
        }
      }

      // Routes matching: /api/drivers/{id}
      if (pathParts.Length == 3 && pathParts[0] == "api" && pathParts[1] == "drivers" && int.TryParse(pathParts[2], out int driverId))
      {
        if (method == "PUT")
        {
          string body = ReadBody(request);
          var driver = JsonSerializer.Deserialize<FormulaDriver>(body, jsonOptions);
          if (driver == null) throw new ArgumentException("Invalid driver payload");
          driver.Id = driverId;
          db.SaveDriver(driver);
          SendJson(response, driver);
          return;
        }
        else if (method == "DELETE")
        {
          db.DeleteDriver(driverId);
          response.StatusCode = 204;
          response.Close();
          return;
        }
      }

      // Catch-all
      SendError(response, "Not Found", 404);
    }
    catch (Exception ex)
    {
      SendError(response, ex.Message, 500);
    }
  }

  private static string ReadBody(HttpListenerRequest request)
  {
    using var reader = new StreamReader(request.InputStream, request.ContentEncoding ?? Encoding.UTF8);
    return reader.ReadToEnd();
  }

  private static void SendJson(HttpListenerResponse response, object data, int statusCode = 200)
  {
    byte[] buffer = Encoding.UTF8.GetBytes(JsonSerializer.Serialize(data, jsonOptions));
    response.StatusCode = statusCode;
    response.ContentType = "application/json";
    response.ContentLength64 = buffer.Length;
    response.OutputStream.Write(buffer, 0, buffer.Length);
    response.Close();
  }

  private static void SendError(HttpListenerResponse response, string message, int statusCode)
  {
    byte[] buffer = Encoding.UTF8.GetBytes(JsonSerializer.Serialize(new { error = message }, jsonOptions));
    response.StatusCode = statusCode;
    response.ContentType = "application/json";
    response.ContentLength64 = buffer.Length;
    response.OutputStream.Write(buffer, 0, buffer.Length);
    response.Close();
  }
}

public class DriverPositionResult
{
  public int DriverId { get; set; }
  public int Position { get; set; }

  public DriverPositionResult() : this(0, 0) { }

  public DriverPositionResult(int driverId, int position)
  {
    DriverId = driverId;
    Position = position;
  }
}
