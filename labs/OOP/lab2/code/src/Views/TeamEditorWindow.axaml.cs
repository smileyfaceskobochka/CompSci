using Avalonia;
using Avalonia.Controls;
using Avalonia.Interactivity;
using RacingSystem.ViewModels;

namespace RacingSystem.Views
{
    public partial class TeamEditorWindow : Window
    {
        public bool IsSaved { get; private set; }

        public TeamEditorWindow()
        {
            InitializeComponent();
        }

        private void OnSaveClick(object? sender, RoutedEventArgs e)
        {
            IsSaved = true;
            Close();
        }

        private void OnCancelClick(object? sender, RoutedEventArgs e)
        {
            IsSaved = false;
            Close();
        }
    }
}
