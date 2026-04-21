using Avalonia.Controls;
using Avalonia.Interactivity;

namespace RacingSystem.Views
{
    public partial class RaceSimulationWindow : Window
    {
        public int? ResultPosition { get; private set; }

        public RaceSimulationWindow()
        {
            InitializeComponent();
        }

        private void OnOkClick(object? sender, RoutedEventArgs e)
        {
            ResultPosition = (int?)this.Find<NumericUpDown>("PosInput")?.Value;
            Close();
        }

        private void OnCancelClick(object? sender, RoutedEventArgs e)
        {
            ResultPosition = null;
            Close();
        }
    }
}
