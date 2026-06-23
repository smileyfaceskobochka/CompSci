namespace RacingSystem.Core
{
  public interface IDriver
  {
    int Id { get; set; }
    int TeamId { get; set; }
    string FirstName { get; set; }
    string LastName { get; set; }
    int RacingNumber { get; set; }
    int Points { get; set; }
    string FullName { get; }
    string DisplayString { get; }
    void AddPoints(int pts);
  }
}
