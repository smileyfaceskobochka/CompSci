using System;
using System.Collections.Generic;
using CommunityToolkit.Mvvm.ComponentModel;
using RacingSystem.Core;

namespace RacingSystem.ViewModels
{
    public partial class TeamEditorViewModel : ObservableObject
    {
        [ObservableProperty] private string _name = "";
        [ObservableProperty] private string _principal = "";
        [ObservableProperty] private string _hq = "";
        [ObservableProperty] private int _points;
        [ObservableProperty] private int _wins;
        [ObservableProperty] private int _podiums;

        [ObservableProperty] private string _selectedType = "F1";
        public bool IsF1 => SelectedType == "F1";
        public bool IsF2 => SelectedType == "F2";
        public bool IsFE => SelectedType == "FE";

        partial void OnSelectedTypeChanged(string value)
        {
            OnPropertyChanged(nameof(IsF1));
            OnPropertyChanged(nameof(IsF2));
            OnPropertyChanged(nameof(IsFE));
        }

        public string[] TeamTypes => new[] { "F1", "F2", "FE" };

        // F1 specific
        [ObservableProperty] private string _powerUnit = "";
        [ObservableProperty] private double _budget;
        [ObservableProperty] private int _ctorPos;

        // F2 specific
        [ObservableProperty] private string _chassis = "";
        [ObservableProperty] private int _graduates;
        [ObservableProperty] private bool _isFeeder = true;

        // FE specific
        [ObservableProperty] private string _energyPartner = "";
        [ObservableProperty] private double _batteryKwh;
        [ObservableProperty] private int _sustainScore;

        public TeamEditorViewModel() { }

        public TeamEditorViewModel(FormulaTeam team)
        {
            Name = team.TeamName;
            Principal = team.PrincipalName;
            Hq = team.Headquarters;
            Points = team.ChampionshipPoints;
            Wins = team.RaceWins;
            Podiums = team.Podiums;

            if (team is F1Team f1)
            {
                SelectedType = "F1";
                PowerUnit = f1.PowerUnit;
                Budget = f1.BudgetCapMln;
                CtorPos = f1.ConstructorPos;
            }
            else if (team is F2Team f2)
            {
                SelectedType = "F2";
                Chassis = f2.ChassisModel;
                Graduates = f2.F1Graduates;
                IsFeeder = f2.IsFeederSeries;
            }
            else if (team is FormulaETeam fe)
            {
                SelectedType = "FE";
                EnergyPartner = fe.EnergyPartner;
                BatteryKwh = fe.BatteryCapacityKwh;
                SustainScore = fe.SustainabilityScore;
            }
        }

        public FormulaTeam ToTeam(int id = 0)
        {
            FormulaTeam team = SelectedType switch
            {
                "F1" => new F1Team(Name, Principal, Hq, PowerUnit, Budget, CtorPos, Points, Wins, Podiums),
                "F2" => new F2Team(Name, Principal, Hq, Chassis, Graduates, IsFeeder, Points, Wins, Podiums),
                "FE" => new FormulaETeam(Name, Principal, Hq, EnergyPartner, SustainScore, BatteryKwh, Points, Wins, Podiums),
                _ => throw new Exception("Invalid Type")
            };
            team.Id = id;
            return team;
        }
        public List<FormulaTeam> Templates { get; } = new()
        {
            new F1Team("Mercedes-AMG Petronas", "Toto Wolff", "Brackley, UK", "Mercedes"),
            new F1Team("Scuderia Ferrari", "Fred Vasseur", "Maranello, Italy", "Ferrari", 135.0, 3, 195, 8, 24),
            new F1Team("Red Bull Racing", "Christian Horner", "Milton Keynes, UK", "Honda RBPT", 135.0, 1, 860, 21, 41),
            new F1Team("McLaren F1 Team", "Andrea Stella", "Woking, UK", "Mercedes", 135.0, 2, 666, 6, 20),
            new F2Team("Prema Racing", "René Rosin", "Grisignano, Italy", "Dallara F2 2024"),
            new F2Team("ART Grand Prix", "Sébastien Philippe", "Villeneuve-la-Garenne, France", "Dallara F2 2024", 14, true, 340, 9, 22),
            new FormulaETeam("Jaguar TCS Racing", "James Barclay", "Silverstone, UK", "Castrol"),
            new FormulaETeam("Nissan Formula E Team", "Tommaso Volpe", "Yokohama, Japan", "Nissan Energy", 88, 38.0, 210, 5, 14)
        };

        private FormulaTeam? _selectedTemplate;
        public FormulaTeam? SelectedTemplate
        {
            get => _selectedTemplate;
            set
            {
                if (SetProperty(ref _selectedTemplate, value) && value != null)
                {
                    ApplyTemplate(value);
                }
            }
        }

        private void ApplyTemplate(FormulaTeam team)
        {
            Name = team.TeamName;
            Principal = team.PrincipalName;
            Hq = team.Headquarters;
            Points = team.ChampionshipPoints;
            Wins = team.RaceWins;
            Podiums = team.Podiums;

            if (team is F1Team f1)
            {
                SelectedType = "F1";
                PowerUnit = f1.PowerUnit;
                Budget = f1.BudgetCapMln;
                CtorPos = f1.ConstructorPos;
            }
            else if (team is F2Team f2)
            {
                SelectedType = "F2";
                Chassis = f2.ChassisModel;
                Graduates = f2.F1Graduates;
                IsFeeder = f2.IsFeederSeries;
            }
            else if (team is FormulaETeam fe)
            {
                SelectedType = "FE";
                EnergyPartner = fe.EnergyPartner;
                BatteryKwh = fe.BatteryCapacityKwh;
                SustainScore = fe.SustainabilityScore;
            }
        }
    }
}
