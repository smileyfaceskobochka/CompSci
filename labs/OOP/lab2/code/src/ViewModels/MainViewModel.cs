using System.Collections.ObjectModel;
using System.Threading.Tasks;
using System.Windows.Input;
using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using RacingSystem.Core;
using RacingSystem.Database;

namespace RacingSystem.ViewModels
{
    public class SeriesHeader
    {
        public string Title { get; set; } = string.Empty;
    }

    public partial class MainViewModel : ObservableObject
    {
        private readonly DatabaseManager _db;

        [ObservableProperty]
        private ObservableCollection<object> _listItems = new();

        [ObservableProperty]
        private FormulaTeam? _selectedTeam;

        [ObservableProperty]
        private bool _isStandingsView;

        [ObservableProperty]
        private bool _isDriverStandingsView;

        partial void OnIsStandingsViewChanged(bool value)
        {
            if (value) IsDriverStandingsView = false;
            LoadList();
        }

        partial void OnIsDriverStandingsViewChanged(bool value)
        {
            if (value) IsStandingsView = false;
            LoadList();
        }

        public ICommand LoadTeamsCommand { get; }
        public ICommand DeleteTeamCommand { get; }

        public MainViewModel()
        {
            _db = new DatabaseManager();
            LoadTeamsCommand = new RelayCommand(LoadList);
            DeleteTeamCommand = new RelayCommand(DeleteTeam, () => SelectedTeam != null);
            
            LoadList();
        }

        public void LoadList()
        {
            var list = _db.GetAllTeams();
            ListItems.Clear();

            if (IsDriverStandingsView)
            {
                var drivers = _db.GetAllDrivers();
                foreach (var d in drivers) ListItems.Add(d);
            }
            else if (IsStandingsView)
            {
                // Sort by points descending (no headers)
                list.Sort((a, b) => b.ChampionshipPoints.CompareTo(a.ChampionshipPoints));
                foreach (var t in list) ListItems.Add(t);
            }
            else
            {
                // Sort by series name
                list.Sort((a, b) => string.Compare(a.SeriesName, b.SeriesName, System.StringComparison.Ordinal));
                
                string? currentSeries = null;
                foreach (var t in list)
                {
                    if (t.SeriesName != currentSeries)
                    {
                        currentSeries = t.SeriesName;
                        ListItems.Add(new SeriesHeader { Title = currentSeries });
                    }
                    ListItems.Add(t);
                }
            }
            
            // Keep actual Teams list up-to-date for comparison tool
            Teams = new ObservableCollection<FormulaTeam>(list);
            OnPropertyChanged(nameof(Teams));
        }

        // We expose Teams separately so Comparison tool can still just access Teams
        public ObservableCollection<FormulaTeam> Teams { get; private set; } = new();

        private void DeleteTeam()
        {
            if (SelectedTeam != null)
            {
                DeleteTeamById(SelectedTeam.Id);
            }
        }

        public void DeleteTeamById(int id)
        {
            _db.DeleteTeam(id);
            if (SelectedTeam?.Id == id) SelectedTeam = null;
            LoadList();
        }

        public void SaveTeam(FormulaTeam team)
        {
            _db.SaveTeam(team);
            LoadList();
        }
    }
}
