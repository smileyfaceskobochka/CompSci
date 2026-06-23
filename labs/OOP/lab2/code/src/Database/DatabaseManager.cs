using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.EntityFrameworkCore;
using RacingSystem.Core;

namespace RacingSystem.Database;

public class DatabaseManager
{
  public DatabaseManager() : this("") { }

  public DatabaseManager(string dbPath)
  { }

  public List<FormulaTeam> GetAllTeams()
  {
    using var context = new RacingDbContext();
    return context.Teams
        .Include(t => t.Drivers)
        .OrderByDescending(t => t.ChampionshipPoints)
        .ToList();
  }

  public List<FormulaDriver> GetAllDrivers()
  {
    using var context = new RacingDbContext();
    return context.Drivers
        .Where(d => context.Teams.Any(t => t.Id == d.TeamId))
        .OrderByDescending(d => d.Points)
        .ToList();
  }

  public void SaveTeam(FormulaTeam team)
  {
    using var context = new RacingDbContext();
    if (team.Id == 0)
    {
      context.Teams.Add(team);
    }
    else
    {
      context.Teams.Update(team);
    }
    context.SaveChanges();
  }

  public void SaveDriver(FormulaDriver driver)
  {
    using var context = new RacingDbContext();
    if (driver.Id == 0)
    {
      context.Drivers.Add(driver);
    }
    else
    {
      context.Drivers.Update(driver);
    }
    context.SaveChanges();
  }

  public void DeleteDriver(int id)
  {
    using var context = new RacingDbContext();
    var driver = context.Drivers.Find(id);
    if (driver != null)
    {
      context.Drivers.Remove(driver);
      context.SaveChanges();
    }
  }

  public void DeleteTeam(int id)
  {
    using var context = new RacingDbContext();
    var team = context.Teams.Include(t => t.Drivers).FirstOrDefault(t => t.Id == id);
    if (team != null)
    {
      context.Drivers.RemoveRange(team.Drivers);
      context.Teams.Remove(team);
      context.SaveChanges();
    }
  }
}
