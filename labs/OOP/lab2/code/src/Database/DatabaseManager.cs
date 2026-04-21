using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Microsoft.Data.Sqlite;
using Dapper;
using RacingSystem.Core;

namespace RacingSystem.Database
{
    public class DatabaseManager
    {
        private readonly string _connectionString;

        public DatabaseManager(string dbPath = "racing.db")
        {
            _connectionString = $"Data Source={dbPath}";
            InitializeDatabase();
        }

        private void InitializeDatabase()
        {
            using var connection = new SqliteConnection(_connectionString);
            connection.Open();

            // Create Tables
            connection.Execute(@"
                CREATE TABLE IF NOT EXISTS Teams (
                    Id INTEGER PRIMARY KEY AUTOINCREMENT,
                    Type TEXT NOT NULL,
                    Name TEXT NOT NULL,
                    Principal TEXT,
                    HQ TEXT,
                    Points INTEGER DEFAULT 0,
                    Wins INTEGER DEFAULT 0,
                    Podiums INTEGER DEFAULT 0
                );

                CREATE TABLE IF NOT EXISTS F1Details (
                    TeamId INTEGER PRIMARY KEY,
                    PowerUnit TEXT,
                    BudgetCap REAL,
                    ConstructorPos INTEGER,
                    FOREIGN KEY(TeamId) REFERENCES Teams(Id) ON DELETE CASCADE
                );

                CREATE TABLE IF NOT EXISTS F2Details (
                    TeamId INTEGER PRIMARY KEY,
                    ChassisModel TEXT,
                    Graduates INTEGER,
                    IsFeeder INTEGER,
                    FOREIGN KEY(TeamId) REFERENCES Teams(Id) ON DELETE CASCADE
                );

                CREATE TABLE IF NOT EXISTS FEDetails (
                    TeamId INTEGER PRIMARY KEY,
                    EnergyPartner TEXT,
                    BatteryKwh REAL,
                    SustainScore INTEGER,
                    FOREIGN KEY(TeamId) REFERENCES Teams(Id) ON DELETE CASCADE
                );

                CREATE TABLE IF NOT EXISTS FormulaDrivers (
                    Id INTEGER PRIMARY KEY AUTOINCREMENT,
                    TeamId INTEGER,
                    FirstName TEXT,
                    LastName TEXT,
                    RacingNumber INTEGER,
                    Points INTEGER DEFAULT 0,
                    FOREIGN KEY(TeamId) REFERENCES Teams(Id) ON DELETE CASCADE
                );
            ");

            SeedTeams(connection);
            SeedDrivers(connection);
        }

        private void SeedTeams(SqliteConnection connection)
        {
            if (connection.QuerySingle<int>("SELECT COUNT(*) FROM Teams") > 0) return;

            using var transaction = connection.BeginTransaction();

            void InsertTeam(string type, string name, string principal, string hq, string extra1, double extra2 = 0, int extra3 = 0, int pts = 0, int wins = 0, int pods = 0)
            {
                long id = connection.QuerySingle<long>(@"
                    INSERT INTO Teams (Type, Name, Principal, HQ, Points, Wins, Podiums)
                    VALUES (@Type, @Name, @Principal, @HQ, @Pts, @Wins, @Pods);
                    SELECT last_insert_rowid();",
                    new { Type = type, Name = name, Principal = principal, HQ = hq, Pts = pts, Wins = wins, Pods = pods }, transaction);

                if (type == "F1")
                    connection.Execute("INSERT INTO F1Details (TeamId, PowerUnit, BudgetCap, ConstructorPos) VALUES (@Id, @PU, @Cap, @Pos)", new { Id = id, PU = extra1, Cap = extra2, Pos = extra3 }, transaction);
                else if (type == "F2")
                    connection.Execute("INSERT INTO F2Details (TeamId, ChassisModel, Graduates, IsFeeder) VALUES (@Id, @Chassis, @Grads, @Feeder)", new { Id = id, Chassis = extra1, Grads = (int)extra2, Feeder = extra3 }, transaction);
                else if (type == "FE")
                    connection.Execute("INSERT INTO FEDetails (TeamId, EnergyPartner, BatteryKwh, SustainScore) VALUES (@Id, @Partner, @Battery, @Score)", new { Id = id, Partner = extra1, Battery = extra2, Score = extra3 }, transaction);
            }

            InsertTeam("F1", "Mercedes-AMG Petronas", "Toto Wolff", "Brackley, UK", "Mercedes", 135, 2, 110, 0, 2);
            InsertTeam("F1", "Scuderia Ferrari", "Fred Vasseur", "Maranello, Italy", "Ferrari", 135, 3, 195, 3, 8);
            InsertTeam("F1", "Red Bull Racing", "Christian Horner", "Milton Keynes, UK", "Ford RBPT", 135, 1, 395, 12, 15);
            InsertTeam("F1", "McLaren F1 Team", "Andrea Stella", "Woking, UK", "Mercedes", 135, 2, 305, 5, 12);
            InsertTeam("F1", "Aston Martin Aramco", "Mike Krack", "Silverstone, UK", "Honda", 135, 5, 95, 0, 1);
            InsertTeam("F1", "Audi F1 Team", "Mattia Binotto", "Hinwil, Switzerland", "Audi", 135, 10, 34, 0, 0);
            InsertTeam("F1", "Cadillac Racing", "Michael Andretti", "Fishers, USA", "Cadillac", 135, 9, 82, 0, 0);

            InsertTeam("F2", "Prema Racing", "René Rosin", "Grisignano, Italy", "Dallara F2 2024", 15, 1, 107, 2, 5);
            InsertTeam("F2", "ART Grand Prix", "Sébastien Philippe", "Villeneuve-la-Garenne, France", "Dallara F2 2024", 14, 1, 58, 1, 3);

            InsertTeam("FE", "Jaguar TCS Racing", "James Barclay", "Silverstone, UK", "Castrol", 38, 88, 297, 6, 12);
            InsertTeam("FE", "Nissan Formula E Team", "Tommaso Volpe", "Yokohama, Japan", "Nissan Energy", 38, 88, 155, 1, 4);

            transaction.Commit();
        }

        private void SeedDrivers(SqliteConnection connection)
        {
            var driverCount = connection.QuerySingle<int>("SELECT COUNT(*) FROM FormulaDrivers");
            if (driverCount == 0)
            {
                var teamMap = connection.Query("SELECT Id, Name FROM Teams").ToDictionary(row => (string)row.Name, row => (int)(long)row.Id);

                var seedDrivers = new List<(string Name, string Driver1, int Num1, int Pts1, string Driver2, int Num2, int Pts2)>
                {
                    ("Mercedes-AMG Petronas", "George Russell", 63, 110, "Kimi Antonelli", 12, 15),
                    ("Scuderia Ferrari", "Lewis Hamilton", 44, 185, "Charles Leclerc", 16, 170),
                    ("Red Bull Racing", "Max Verstappen", 1, 395, "Isack Hadjar", 20, 60),
                    ("McLaren F1 Team", "Lando Norris", 4, 305, "Oscar Piastri", 81, 260),
                    ("Aston Martin Aramco", "Fernando Alonso", 14, 95, "Lance Stroll", 18, 48),
                    ("Audi F1 Team", "Nico Hulkenberg", 27, 34, "Gabriel Bortoleto", 5, 5),
                    ("Cadillac Racing", "Sergio Perez", 11, 82, "Valtteri Bottas", 77, 12),
                    ("Prema Racing", "Ugo Ugochukwu", 3, 45, "Arvid Lindblad", 4, 62),
                    ("ART Grand Prix", "James Wharton", 1, 30, "Tuukka Taponen", 2, 28),
                    ("Jaguar TCS Racing", "Mitch Evans", 9, 155, "Nick Cassidy", 37, 142),
                    ("Nissan Formula E Team", "Oliver Rowland", 22, 110, "Norman Nato", 17, 45)
                };

                foreach (var st in seedDrivers)
                {
                    if (teamMap.TryGetValue(st.Name, out int teamId))
                    {
                        var names1 = st.Driver1.Split(' ');
                        var names2 = st.Driver2.Split(' ');
                        connection.Execute("INSERT INTO FormulaDrivers (TeamId, FirstName, LastName, RacingNumber, Points) VALUES (@Id, @F, @L, @N, @P)", new { Id = teamId, F = names1[0], L = names1[1], N = st.Num1, P = st.Pts1 });
                        connection.Execute("INSERT INTO FormulaDrivers (TeamId, FirstName, LastName, RacingNumber, Points) VALUES (@Id, @F, @L, @N, @P)", new { Id = teamId, F = names2[0], L = names2[1], N = st.Num2, P = st.Pts2 });
                    }
                }
            }
        }

        public List<FormulaTeam> GetAllTeams()
        {
            using var connection = new SqliteConnection(_connectionString);
            connection.Open();

            var teams = new List<FormulaTeam>();

            var baseTeams = connection.Query("SELECT * FROM Teams").ToList();
            var allDrivers = connection.Query<FormulaDriver>("SELECT * FROM FormulaDrivers").ToList();

            foreach (var row in baseTeams)
            {
                FormulaTeam team;
                int id = (int)(long)row.Id;
                string type = (string)row.Type;

                if (type == "F1")
                {
                    var details = connection.QuerySingle("SELECT * FROM F1Details WHERE TeamId = @Id", new { Id = id });
                    team = new F1Team((string)row.Name, (string)row.Principal, (string)row.HQ, (string)details.PowerUnit, (double)details.BudgetCap, (int)(long)details.ConstructorPos, (int)(long)row.Points, (int)(long)row.Wins, (int)(long)row.Podiums);
                }
                else if (type == "F2")
                {
                    var details = connection.QuerySingle("SELECT * FROM F2Details WHERE TeamId = @Id", new { Id = id });
                    team = new F2Team((string)row.Name, (string)row.Principal, (string)row.HQ, (string)details.ChassisModel, (int)(long)details.Graduates, (long)details.IsFeeder != 0, (int)(long)row.Points, (int)(long)row.Wins, (int)(long)row.Podiums);
                }
                else // FE
                {
                    var details = connection.QuerySingle("SELECT * FROM FEDetails WHERE TeamId = @Id", new { Id = id });
                    team = new FormulaETeam((string)row.Name, (string)row.Principal, (string)row.HQ, (string)details.EnergyPartner, (int)(long)details.SustainScore, (double)details.BatteryKwh, (int)(long)row.Points, (int)(long)row.Wins, (int)(long)row.Podiums);
                }

                team.Id = id;
                team.Drivers = allDrivers.Where(d => d.TeamId == id).ToList();
                teams.Add(team);
            }

            return teams;
        }

        public List<FormulaDriver> GetAllDrivers()
        {
            using var connection = new SqliteConnection(_connectionString);
            connection.Open();
            return connection.Query<FormulaDriver>("SELECT * FROM FormulaDrivers ORDER BY Points DESC").ToList();
        }

        public void SaveTeam(FormulaTeam team)
        {
            using var connection = new SqliteConnection(_connectionString);
            connection.Open();
            using var transaction = connection.BeginTransaction();

            string type = team switch
            {
                F1Team => "F1",
                F2Team => "F2",
                FormulaETeam => "FE",
                _ => throw new ArgumentException("Unknown team type")
            };

            if (team.Id == 0) // Insert
            {
                long id = connection.QuerySingle<long>(@"
                    INSERT INTO Teams (Type, Name, Principal, HQ, Points, Wins, Podiums)
                    VALUES (@Type, @Name, @Principal, @HQ, @Points, @Wins, @Podiums);
                    SELECT last_insert_rowid();",
                    new { Type = type, Name = team.TeamName, Principal = team.PrincipalName, HQ = team.Headquarters, Points = team.ChampionshipPoints, Wins = team.RaceWins, Podiums = team.Podiums },
                    transaction);

                team.Id = (int)id;

                if (team is F1Team f1)
                {
                    connection.Execute("INSERT INTO F1Details (TeamId, PowerUnit, BudgetCap, ConstructorPos) VALUES (@Id, @PU, @Budget, @Pos)",
                        new { Id = id, PU = f1.PowerUnit, Budget = f1.BudgetCapMln, Pos = f1.ConstructorPos }, transaction);
                }
                else if (team is F2Team f2)
                {
                    connection.Execute("INSERT INTO F2Details (TeamId, ChassisModel, Graduates, IsFeeder) VALUES (@Id, @Chassis, @Grads, @Feeder)",
                        new { Id = id, Chassis = f2.ChassisModel, Grads = f2.F1Graduates, Feeder = f2.IsFeederSeries ? 1 : 0 }, transaction);
                }
                else if (team is FormulaETeam fe)
                {
                    connection.Execute("INSERT INTO FEDetails (TeamId, EnergyPartner, BatteryKwh, SustainScore) VALUES (@Id, @Partner, @Battery, @Score)",
                        new { Id = id, Partner = fe.EnergyPartner, Battery = fe.BatteryCapacityKwh, Score = fe.SustainabilityScore }, transaction);
                }
            }
            else // Update
            {
                connection.Execute(@"
                    UPDATE Teams SET Name = @Name, Principal = @Principal, HQ = @HQ, Points = @Points, Wins = @Wins, Podiums = @Podiums
                    WHERE Id = @Id",
                    new { Id = team.Id, Name = team.TeamName, Principal = team.PrincipalName, HQ = team.Headquarters, Points = team.ChampionshipPoints, Wins = team.RaceWins, Podiums = team.Podiums },
                    transaction);

                if (team is F1Team f1)
                {
                    connection.Execute("UPDATE F1Details SET PowerUnit = @PU, BudgetCap = @Budget, ConstructorPos = @Pos WHERE TeamId = @Id",
                        new { Id = f1.Id, PU = f1.PowerUnit, Budget = f1.BudgetCapMln, Pos = f1.ConstructorPos }, transaction);
                }
                else if (team is F2Team f2)
                {
                    connection.Execute("UPDATE F2Details SET ChassisModel = @Chassis, Graduates = @Grads, IsFeeder = @Feeder WHERE TeamId = @Id",
                        new { Id = f2.Id, Chassis = f2.ChassisModel, Grads = f2.F1Graduates, Feeder = f2.IsFeederSeries ? 1 : 0 }, transaction);
                }
                else if (team is FormulaETeam fe)
                {
                    connection.Execute("UPDATE FEDetails SET EnergyPartner = @Partner, BatteryKwh = @Battery, SustainScore = @Score WHERE TeamId = @Id",
                        new { Id = fe.Id, Partner = fe.EnergyPartner, Battery = fe.BatteryCapacityKwh, Score = fe.SustainabilityScore }, transaction);
                }
            }

            transaction.Commit();
        }

        public void DeleteTeam(int id)
        {
            using var connection = new SqliteConnection(_connectionString);
            connection.Open();
            connection.Execute("DELETE FROM Teams WHERE Id = @Id", new { Id = id });
        }
    }
}
