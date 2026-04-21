using System.Collections.Generic;
using System.Linq;
using CommunityToolkit.Mvvm.ComponentModel;
using RacingSystem.Core;

namespace RacingSystem.ViewModels
{
    public partial class ComparisonViewModel : ObservableObject
    {
        [ObservableProperty] private FormulaTeam _teamA;
        
        private FormulaTeam? _teamB;
        public FormulaTeam? TeamB
        {
            get => _teamB;
            set
            {
                if (SetProperty(ref _teamB, value))
                {
                    OnPropertyChanged(nameof(ComparisonResult));
                }
            }
        }

        public List<FormulaTeam> AvailableTeams { get; }

        public string ComparisonResult
        {
            get
            {
                if (TeamB == null) return "Select a team to compare";
                if (TeamA.Id == TeamB.Id) return "Same Team Selected";
                
                if (TeamA > TeamB) return $"{TeamA.TeamName} is leading in the championship!";
                if (TeamB > TeamA) return $"{TeamB.TeamName} is leading in the championship!";
                
                return "Teams are equal in points.";
            }
        }

        public ComparisonViewModel(FormulaTeam a, IEnumerable<FormulaTeam> allTeams)
        {
            TeamA = a;
            AvailableTeams = allTeams.ToList();
            
            // Default select the first other team
            TeamB = AvailableTeams.FirstOrDefault(t => t.Id != a.Id);
        }
    }
}
