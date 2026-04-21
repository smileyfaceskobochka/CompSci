package main

import (
	"sort"
	"strings"
)

func CalculateF1Points(position int) int {
	pts := []int{25, 18, 15, 12, 10, 8, 6, 4, 2, 1}
	if position >= 1 && position <= 10 {
		return pts[position-1]
	}
	return 0
}

func CalculateF2Points(position int) int {
	pts := []int{25, 18, 15, 12, 10, 8, 6, 4, 2, 1, 0, 0, 0, 0, 0}
	if position >= 1 && position <= len(pts) {
		return pts[position-1]
	}
	return 0
}

func GetWinRate(wins, totalRaces int) float64 {
	if totalRaces <= 0 {
		return 0
	}
	return float64(wins) / float64(totalRaces) * 100.0
}

func FilterTopTeams(teams []Team, minPoints int) []Team {
	var result []Team
	for _, t := range teams {
		if t.Points >= minPoints {
			result = append(result, t)
		}
	}
	return result
}

func SortTeams(teams []Team, field, order string) []Team {
	cp := make([]Team, len(teams))
	copy(cp, teams)
	sort.Slice(cp, func(i, j int) bool {
		var less bool
		switch field {
		case "name":
			less = strings.ToLower(cp[i].Name) < strings.ToLower(cp[j].Name)
		case "points":
			less = cp[i].Points < cp[j].Points
		default:
			less = cp[i].Points < cp[j].Points
		}
		if order == "desc" {
			return !less
		}
		return less
	})
	return cp
}

func SortDrivers(drivers []Driver, field, order string) []Driver {
	cp := make([]Driver, len(drivers))
	copy(cp, drivers)
	sort.Slice(cp, func(i, j int) bool {
		var less bool
		switch field {
		case "name":
			less = strings.ToLower(cp[i].FullName) < strings.ToLower(cp[j].FullName)
		case "number":
			less = cp[i].RacingNumber < cp[j].RacingNumber
		default:
			less = cp[i].Points < cp[j].Points
		}
		if order == "desc" {
			return !less
		}
		return less
	})
	return cp
}

type AggStats struct {
	TotalTeams    int
	TotalDrivers  int
	TotalSeries   int
	AvgTeamPoints float64
	TopTeam       *Team
	TopDriver     *Driver
}

func ComputeStats(teams []Team, drivers []Driver, series []Series) AggStats {
	s := AggStats{
		TotalTeams:   len(teams),
		TotalDrivers: len(drivers),
		TotalSeries:  len(series),
	}
	if len(teams) > 0 {
		sum := 0
		for _, t := range teams {
			sum += t.Points
		}
		s.AvgTeamPoints = float64(sum) / float64(len(teams))
		top := teams[0]
		for _, t := range teams {
			if t.Points > top.Points {
				top = t
			}
		}
		s.TopTeam = &top
	}
	if len(drivers) > 0 {
		top := drivers[0]
		for _, d := range drivers {
			if d.Points > top.Points {
				top = d
			}
		}
		s.TopDriver = &top
	}
	return s
}

func ValidateTeam(t Team) []string {
	var errs []string
	if strings.TrimSpace(t.Name) == "" {
		errs = append(errs, "Team name is required")
	}
	if strings.TrimSpace(t.PrincipalName) == "" {
		errs = append(errs, "Principal name is required")
	}
	if t.Points < 0 {
		errs = append(errs, "Points cannot be negative")
	}
	if t.SeriesID == 0 {
		errs = append(errs, "Series is required")
	}
	return errs
}

func ValidateDriver(d Driver) []string {
	var errs []string
	if strings.TrimSpace(d.FirstName) == "" {
		errs = append(errs, "First name is required")
	}
	if strings.TrimSpace(d.LastName) == "" {
		errs = append(errs, "Last name is required")
	}
	if d.RacingNumber < 1 || d.RacingNumber > 99 {
		errs = append(errs, "Racing number must be between 1 and 99")
	}
	if d.Points < 0 {
		errs = append(errs, "Points cannot be negative")
	}
	if d.TeamID == 0 {
		errs = append(errs, "Team is required")
	}
	return errs
}
