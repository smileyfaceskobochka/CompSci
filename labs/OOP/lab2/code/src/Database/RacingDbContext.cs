using Microsoft.EntityFrameworkCore;
using RacingSystem.Core;

namespace RacingSystem.Database;

public class RacingDbContext : DbContext
{
  public DbSet<FormulaTeam> Teams { get; set; } = null!;
  public DbSet<FormulaDriver> Drivers { get; set; } = null!;

  public RacingDbContext() : this(new DbContextOptions<RacingDbContext>()) { }

  public RacingDbContext(DbContextOptions<RacingDbContext> options) : base(options) { }

  protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
  {
    // Postgres
    optionsBuilder.UseNpgsql(
        "Host=localhost;Database=postgres;" +
        "Username=postgres;Password=postgres");
  }

  protected override void OnModelCreating(ModelBuilder modelBuilder)
  {
    // Map Table-per-Type (TPT)
    modelBuilder.Entity<FormulaTeam>()
        .ToTable("racing_teams");

    modelBuilder.Entity<FormulaTeam>()
        .Property<long>("series_id")
        .HasColumnName("series_id");

    modelBuilder.Entity<FormulaTeam>()
        .HasQueryFilter(t => EF.Property<long>(t, "series_id") >= 1 && EF.Property<long>(t, "series_id") <= 3);

    modelBuilder.Entity<FormulaTeam>(entity =>
    {
      entity.HasKey(t => t.Id);
      entity.Property(t => t.Id).HasColumnName("id").ValueGeneratedOnAdd();
      entity.Property(t => t.TeamName).HasColumnName("name");
      entity.Property(t => t.PrincipalName).HasColumnName("principal_name");
      entity.Property(t => t.Headquarters).HasColumnName("base_location");
      entity.Property(t => t.TeamColor).HasColumnName("team_color");
      entity.Property(t => t.ChampionshipPoints).HasColumnName("points");
      entity.Property(t => t.RaceWins).HasColumnName("wins");
      entity.Property(t => t.Podiums).HasColumnName("podiums");

      entity.HasMany(t => t.Drivers)
                .WithOne()
                .HasForeignKey(d => d.TeamId);
    });

    // Map F1Team 
    modelBuilder.Entity<F1Team>(entity =>
    {
      entity.ToTable("f1_teams");
      entity.Property(t => t.PowerUnit).HasColumnName("power_unit");
      entity.Property(t => t.BudgetCapMln).HasColumnName("budget_cap");
      entity.Property(t => t.ConstructorPos).HasColumnName("constructor_pos");
    });

    // Map F2Team
    modelBuilder.Entity<F2Team>(entity =>
    {
      entity.ToTable("f2_teams");
      entity.Property(t => t.ChassisModel).HasColumnName("chassis_model");
      entity.Property(t => t.F1Graduates).HasColumnName("graduates");
      entity.Property(t => t.IsFeederSeries).HasColumnName("is_feeder");
    });

    // Map FormulaETeam
    modelBuilder.Entity<FormulaETeam>(entity =>
    {
      entity.ToTable("formula_e_teams");
      entity.Property(t => t.EnergyPartner).HasColumnName("energy_partner");
      entity.Property(t => t.BatteryCapacityKwh).HasColumnName("battery_kwh");
      entity.Property(t => t.SustainabilityScore).HasColumnName("sustain_score");
    });

    // Map FormulaDriver
    modelBuilder.Entity<FormulaDriver>(entity =>
    {
      entity.ToTable("racing_drivers");
      entity.HasKey(d => d.Id);
      entity.Property(d => d.Id).HasColumnName("id").ValueGeneratedOnAdd();
      entity.Property(d => d.TeamId).HasColumnName("team_id");
      entity.Property(d => d.FirstName).HasColumnName("first_name");
      entity.Property(d => d.LastName).HasColumnName("last_name");
      entity.Property(d => d.RacingNumber).HasColumnName("racing_number");
      entity.Property(d => d.Points).HasColumnName("points");
    });
  }

  public override int SaveChanges()
  {
    SetSeriesIds();
    return base.SaveChanges();
  }

  public override System.Threading.Tasks.Task<int> SaveChangesAsync(System.Threading.CancellationToken cancellationToken = default)
  {
    SetSeriesIds();
    return base.SaveChangesAsync(cancellationToken);
  }

  private void SetSeriesIds()
  {
    foreach (var entry in ChangeTracker.Entries<FormulaTeam>())
    {
      if (entry.State == EntityState.Added || entry.State == EntityState.Modified)
      {
        long seriesId = entry.Entity switch
        {
          F1Team => 1,
          F2Team => 2,
          FormulaETeam => 3,
          _ => 0
        };
        entry.Property("series_id").CurrentValue = seriesId;
      }
    }
  }
}
