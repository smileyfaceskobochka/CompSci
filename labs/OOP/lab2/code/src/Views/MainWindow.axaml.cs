using System;
using System.Linq;
using Avalonia.Controls;
using Avalonia.Interactivity;
using RacingSystem.ViewModels;
using RacingSystem.Core;

namespace RacingSystem.Views
{
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
            DataContext = new MainViewModel();
        }

        private async void OnAddTeamClick(object? sender, RoutedEventArgs e)
        {
            var dialog = new TeamEditorWindow
            {
                DataContext = new TeamEditorViewModel()
            };

            await dialog.ShowDialog(this);

            if (dialog.IsSaved && dialog.DataContext is TeamEditorViewModel vm)
            {
                ((MainViewModel)DataContext!).SaveTeam(vm.ToTeam());
            }
        }

        private async void OnEditTeamClick(object? sender, RoutedEventArgs e)
        {
            var viewModel = (MainViewModel)DataContext!;
            if (viewModel.SelectedTeam == null) return;

            var dialog = new TeamEditorWindow
            {
                DataContext = new TeamEditorViewModel(viewModel.SelectedTeam)
            };

            await dialog.ShowDialog(this);

            if (dialog.IsSaved && dialog.DataContext is TeamEditorViewModel vm)
            {
                viewModel.SaveTeam(vm.ToTeam(viewModel.SelectedTeam.Id));
            }
        }

        private async void OnSimulateRaceClick(object? sender, RoutedEventArgs e)
        {
            var viewModel = (MainViewModel)DataContext!;
            if (viewModel.SelectedTeam == null) return;

            var dialog = new RaceSimulationWindow();
            await dialog.ShowDialog(this);

            if (dialog.ResultPosition.HasValue)
            {
                int pos = dialog.ResultPosition.Value;
                int pts = viewModel.SelectedTeam.CalculatePoints(pos);
                
                viewModel.SelectedTeam.AddPoints(pts);
                if (pos == 1) viewModel.SelectedTeam.AddWin();
                else if (pos <= 3) viewModel.SelectedTeam.AddPodium();

                viewModel.SaveTeam(viewModel.SelectedTeam);
            }
        }

        private async void OnCompareClick(object? sender, RoutedEventArgs e)
        {
            var viewModel = (MainViewModel)DataContext!;
            if (viewModel.SelectedTeam == null) return;

            var dialog = new TeamComparisonWindow
            {
                DataContext = new ComparisonViewModel(viewModel.SelectedTeam, viewModel.Teams)
            };

            await dialog.ShowDialog(this);
        }

        // Inline delete button on each team card
        private void OnInlineDeleteClick(object? sender, RoutedEventArgs e)
        {
            if (sender is Button btn && btn.DataContext is FormulaTeam team)
            {
                var viewModel = (MainViewModel)DataContext!;
                viewModel.DeleteTeamById(team.Id);
            }
        }

        // Delete selected team from the details panel
        private void OnDeleteSelectedClick(object? sender, RoutedEventArgs e)
        {
            var viewModel = (MainViewModel)DataContext!;
            if (viewModel.SelectedTeam != null)
            {
                viewModel.DeleteTeamById(viewModel.SelectedTeam.Id);
            }
        }
    }
}