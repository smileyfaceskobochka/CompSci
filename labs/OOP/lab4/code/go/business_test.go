package main

import (
	"testing"
)

func TestCalculateF1Points(t *testing.T) {
	tests := []struct {
		pos    int
		expect int
	}{
		{1, 25}, {2, 18}, {3, 15}, {4, 12}, {5, 10},
		{6, 8}, {7, 6}, {8, 4}, {9, 2}, {10, 1},
		{11, 0}, {0, 0}, {-1, 0}, {20, 0},
	}
	for _, tc := range tests {
		got := CalculateF1Points(tc.pos)
		if got != tc.expect {
			t.Errorf("CalculateF1Points(%d) = %d; want %d", tc.pos, got, tc.expect)
		}
	}
}

func TestCalculateF2Points(t *testing.T) {
	tests := []struct {
		pos    int
		expect int
	}{
		{1, 25}, {2, 18}, {3, 15}, {10, 1},
		{11, 0}, {15, 0}, {0, 0},
	}
	for _, tc := range tests {
		got := CalculateF2Points(tc.pos)
		if got != tc.expect {
			t.Errorf("CalculateF2Points(%d) = %d; want %d", tc.pos, got, tc.expect)
		}
	}
}

func TestGetWinRate(t *testing.T) {
	tests := []struct {
		wins, total int
		expect      float64
	}{
		{10, 20, 50.0},
		{0, 10, 0},
		{5, 0, 0},
		{3, 4, 75.0},
	}
	for _, tc := range tests {
		got := GetWinRate(tc.wins, tc.total)
		if got != tc.expect {
			t.Errorf("GetWinRate(%d, %d) = %f; want %f", tc.wins, tc.total, got, tc.expect)
		}
	}
}

func TestFilterTopTeams(t *testing.T) {
	teams := []Team{
		{Name: "Team A", Points: 100},
		{Name: "Team B", Points: 50},
		{Name: "Team C", Points: 200},
	}
	filtered := FilterTopTeams(teams, 75)
	if len(filtered) != 2 {
		t.Errorf("expected 2 teams, got %d", len(filtered))
	}
	for _, ft := range filtered {
		if ft.Points < 75 {
			t.Errorf("team %s has %d points, below threshold", ft.Name, ft.Points)
		}
	}
}

func TestSortTeams(t *testing.T) {
	teams := []Team{
		{Name: "Beta", Points: 50},
		{Name: "Alpha", Points: 100},
		{Name: "Gamma", Points: 75},
	}

	sorted := SortTeams(teams, "name", "asc")
	if sorted[0].Name != "Alpha" || sorted[1].Name != "Beta" || sorted[2].Name != "Gamma" {
		t.Errorf("name asc sort failed: %+v", sorted)
	}

	sorted = SortTeams(teams, "points", "desc")
	if sorted[0].Points != 100 || sorted[1].Points != 75 || sorted[2].Points != 50 {
		t.Errorf("points desc sort failed: %+v", sorted)
	}
}

func TestSortDrivers(t *testing.T) {
	drivers := []Driver{
		{FullName: "Z Driver", Points: 10, RacingNumber: 5},
		{FullName: "A Driver", Points: 30, RacingNumber: 1},
		{FullName: "M Driver", Points: 20, RacingNumber: 99},
	}

	sorted := SortDrivers(drivers, "name", "asc")
	if sorted[0].FullName != "A Driver" {
		t.Errorf("name asc sort failed: %+v", sorted)
	}

	sorted = SortDrivers(drivers, "points", "desc")
	if sorted[0].Points != 30 {
		t.Errorf("points desc sort failed: %+v", sorted)
	}

	sorted = SortDrivers(drivers, "number", "asc")
	if sorted[0].RacingNumber != 1 {
		t.Errorf("number asc sort failed: %+v", sorted)
	}
}

func TestValidateTeam(t *testing.T) {
	errs := ValidateTeam(Team{})
	if len(errs) == 0 {
		t.Error("expected errors for empty team")
	}

	errs = ValidateTeam(Team{
		Name:          "Valid Team",
		PrincipalName: "John Doe",
		Points:        100,
		SeriesID:      1,
	})
	if len(errs) != 0 {
		t.Errorf("expected no errors, got %v", errs)
	}

	errs = ValidateTeam(Team{Name: "Ok", PrincipalName: "P", Points: -1, SeriesID: 1})
	found := false
	for _, e := range errs {
		if e == "Points cannot be negative" {
			found = true
		}
	}
	if !found {
		t.Errorf("expected negative points error, got %v", errs)
	}
}

func TestValidateDriver(t *testing.T) {
	errs := ValidateDriver(Driver{})
	if len(errs) == 0 {
		t.Error("expected errors for empty driver")
	}

	errs = ValidateDriver(Driver{
		FirstName:    "Test",
		LastName:     "Driver",
		RacingNumber: 55,
		Points:       10,
		TeamID:       1,
	})
	if len(errs) != 0 {
		t.Errorf("expected no errors, got %v", errs)
	}

	errs = ValidateDriver(Driver{
		FirstName:    "Test",
		LastName:     "Driver",
		RacingNumber: 100,
		Points:       0,
		TeamID:       1,
	})
	found := false
	for _, e := range errs {
		if e == "Racing number must be between 1 and 99" {
			found = true
		}
	}
	if !found {
		t.Errorf("expected racing number error, got %v", errs)
	}
}

func TestComputeStats(t *testing.T) {
	teams := []Team{
		{Name: "Top Team", Points: 200},
		{Name: "Mid Team", Points: 100},
	}
	drivers := []Driver{
		{FullName: "Best Driver", Points: 150},
		{FullName: "Other Driver", Points: 50},
	}
	series := []Series{
		{Name: "F1"},
		{Name: "F2"},
	}

	stats := ComputeStats(teams, drivers, series)
	if stats.TotalTeams != 2 {
		t.Errorf("expected 2 teams, got %d", stats.TotalTeams)
	}
	if stats.TotalDrivers != 2 {
		t.Errorf("expected 2 drivers, got %d", stats.TotalDrivers)
	}
	if stats.TotalSeries != 2 {
		t.Errorf("expected 2 series, got %d", stats.TotalSeries)
	}
	if stats.AvgTeamPoints != 150.0 {
		t.Errorf("expected avg 150.0, got %f", stats.AvgTeamPoints)
	}
	if stats.TopTeam.Name != "Top Team" {
		t.Errorf("expected 'Top Team', got %s", stats.TopTeam.Name)
	}
	if stats.TopDriver.FullName != "Best Driver" {
		t.Errorf("expected 'Best Driver', got %s", stats.TopDriver.FullName)
	}
}

func TestComputeStatsEmpty(t *testing.T) {
	stats := ComputeStats(nil, nil, nil)
	if stats.TotalTeams != 0 {
		t.Errorf("expected 0 teams, got %d", stats.TotalTeams)
	}
	if stats.TopTeam != nil {
		t.Errorf("expected nil TopTeam")
	}
	if stats.TopDriver != nil {
		t.Errorf("expected nil TopDriver")
	}
}
