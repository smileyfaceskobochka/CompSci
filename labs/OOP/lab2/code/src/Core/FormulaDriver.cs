using System;

namespace RacingSystem.Core
{
  public class FormulaDriver
  {
    public int Id { get; set; }
    public int TeamId { get; set; }
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public int RacingNumber { get; set; }
    public int Points { get; set; }

    public FormulaDriver() : this(string.Empty, string.Empty, 0, 0) { }

    public FormulaDriver(string firstName, string lastName, int number, int points = 0)
    {
      FirstName = firstName;
      LastName = lastName;
      RacingNumber = number;
      Points = points;
    }

    public string FullName => $"{FirstName} {LastName}";
    public string DisplayString => $"{FullName} #{RacingNumber}";

    public void AddPoints(int pts) => Points += pts;
  }
}
