package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"math"
	"net/http"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/gorilla/mux"
)

// ─── Python API Struct Mapping ───────────────────────────────────────────────

type PythonSeries struct {
	ID            int64  `json:"id"`
	Name          string `json:"name"`
	OfficialName  string `json:"official_name"`
	Headquarters  string `json:"headquarters"`
	GoverningBody string `json:"governing_body"`
	FoundedYear   int    `json:"founded_year"`
	Rounds        int    `json:"rounds"`
}

type PythonDriver struct {
	ID           int64       `json:"id"`
	FirstName    string      `json:"first_name"`
	LastName     string      `json:"last_name"`
	RacingNumber int         `json:"racing_number"`
	Points       int         `json:"points"`
	TeamID       int64       `json:"team_id"`
	Team         *PythonTeam `json:"team,omitempty"`
}

type PythonTeam struct {
	ID             int64          `json:"id"`
	Name           string         `json:"name"`
	PrincipalName  string         `json:"principal_name"`
	BaseLocation   string         `json:"base_location"`
	PowerUnit      string         `json:"power_unit"`
	Points         int            `json:"points"`
	SeriesID       int64          `json:"series_id"`
	TeamColor      string         `json:"team_color"`
	Wins           int            `json:"wins"`
	Podiums        int            `json:"podiums"`
	BudgetCap      float64        `json:"budget_cap"`
	ConstructorPos int            `json:"constructor_pos"`
	ChassisModel   string         `json:"chassis_model"`
	Graduates      int            `json:"graduates"`
	IsFeeder       bool           `json:"is_feeder"`
	EnergyPartner  string         `json:"energy_partner"`
	BatteryKwh     float64        `json:"battery_kwh"`
	SustainScore   int            `json:"sustain_score"`
	Series         *PythonSeries  `json:"series,omitempty"`
	Drivers        []PythonDriver `json:"drivers,omitempty"`
}

// ─── DTO Conversion ──────────────────────────────────────────────────────────

func toTeamDto(t PythonTeam) FormulaTeamDto {
	dto := FormulaTeamDto{
		ID:                 &t.ID,
		TeamName:           t.Name,
		PrincipalName:      t.PrincipalName,
		Headquarters:       t.BaseLocation,
		TeamColor:          t.TeamColor,
		ChampionshipPoints: &t.Points,
		RaceWins:           &t.Wins,
		Podiums:            &t.Podiums,
	}

	switch t.SeriesID {
	case 1:
		dto.Type = "f1"
		dto.SeriesName = "Formula 1"
		dto.PowerUnit = t.PowerUnit
		dto.BudgetCapMln = &t.BudgetCap
		dto.ConstructorPos = &t.ConstructorPos
		dto.EngineStatus = getPowerUnitStatus(t.Wins, t.PowerUnit)
		dto.Summary = fmt.Sprintf("[Formula 1] %s | Pts: %d | Wins: %d | PU: %s | Budget: $%.1fM",
			dto.TeamName, t.Points, t.Wins, dto.PowerUnit, t.BudgetCap)
	case 2:
		dto.Type = "f2"
		dto.SeriesName = "Formula 2"
		dto.ChassisModel = t.ChassisModel
		dto.F1Graduates = &t.Graduates
		dto.IsFeederSeries = &t.IsFeeder
		dto.FeederStatusText = getFeederStatus(t.IsFeeder, t.Graduates)
		dto.Summary = fmt.Sprintf("[Formula 2] %s | Pts: %d | Wins: %d | Chassis: %s | F1 Grads: %d",
			dto.TeamName, t.Points, t.Wins, dto.ChassisModel, t.Graduates)
	case 3:
		dto.Type = "fe"
		dto.SeriesName = "Formula E"
		dto.EnergyPartner = t.EnergyPartner
		dto.BatteryCapacityKwh = &t.BatteryKwh
		dto.SustainabilityScore = &t.SustainScore
		dto.BatteryRatingText = getBatteryEfficiencyRating(t.BatteryKwh)
		dto.Summary = fmt.Sprintf("[Formula E] %s | Pts: %d | Wins: %d | Eco: %d/100 | Battery: %.1f kWh",
			dto.TeamName, t.Points, t.Wins, t.SustainScore, t.BatteryKwh)
	}

	dto.VisibleColor = getVisibleTeamColor(dto.TeamColor)

	dto.Drivers = []FormulaDriverDto{}
	for _, d := range t.Drivers {
		dto.Drivers = append(dto.Drivers, toDriverDto(d))
	}

	return dto
}

func toDriverDto(d PythonDriver) FormulaDriverDto {
	fullName := fmt.Sprintf("%s %s", d.FirstName, d.LastName)
	return FormulaDriverDto{
		ID:            &d.ID,
		TeamID:        &d.TeamID,
		FirstName:     d.FirstName,
		LastName:      d.LastName,
		RacingNumber:  &d.RacingNumber,
		Points:        &d.Points,
		FullName:      fullName,
		DisplayString: fmt.Sprintf("%s #%d", fullName, d.RacingNumber),
	}
}

func toPythonTeamMap(dto FormulaTeamDto) map[string]any {
	res := map[string]any{
		"name":           dto.TeamName,
		"principal_name": dto.PrincipalName,
		"base_location":  dto.Headquarters,
		"team_color":     dto.TeamColor,
	}
	if dto.ChampionshipPoints != nil {
		res["points"] = *dto.ChampionshipPoints
	} else {
		res["points"] = 0
	}
	if dto.RaceWins != nil {
		res["wins"] = *dto.RaceWins
	} else {
		res["wins"] = 0
	}
	if dto.Podiums != nil {
		res["podiums"] = *dto.Podiums
	} else {
		res["podiums"] = 0
	}

	tType := strings.ToLower(dto.Type)
	var seriesID int64 = 1
	if tType == "fe" {
		seriesID = 3
	} else if tType == "f2" {
		seriesID = 2
	}
	res["series_id"] = seriesID

	switch tType {
	case "f2":
		res["chassis_model"] = dto.ChassisModel
		if dto.F1Graduates != nil {
			res["graduates"] = *dto.F1Graduates
		} else {
			res["graduates"] = 0
		}
		if dto.IsFeederSeries != nil {
			res["is_feeder"] = *dto.IsFeederSeries
		} else {
			res["is_feeder"] = true
		}
		res["power_unit"] = ""
		res["budget_cap"] = 0.0
		res["constructor_pos"] = 0
		res["energy_partner"] = ""
		res["battery_kwh"] = 0.0
		res["sustain_score"] = 0
	case "fe":
		res["energy_partner"] = dto.EnergyPartner
		if dto.BatteryCapacityKwh != nil {
			res["battery_kwh"] = *dto.BatteryCapacityKwh
		} else {
			res["battery_kwh"] = 38.0
		}
		if dto.SustainabilityScore != nil {
			res["sustain_score"] = *dto.SustainabilityScore
		} else {
			res["sustain_score"] = 50
		}
		res["power_unit"] = ""
		res["budget_cap"] = 0.0
		res["constructor_pos"] = 0
		res["chassis_model"] = ""
		res["graduates"] = 0
		res["is_feeder"] = false
	default: // F1
		res["power_unit"] = dto.PowerUnit
		if dto.BudgetCapMln != nil {
			res["budget_cap"] = *dto.BudgetCapMln
		} else {
			res["budget_cap"] = 135.0
		}
		if dto.ConstructorPos != nil {
			res["constructor_pos"] = *dto.ConstructorPos
		} else {
			res["constructor_pos"] = 1
		}
		res["chassis_model"] = ""
		res["graduates"] = 0
		res["is_feeder"] = false
		res["energy_partner"] = ""
		res["battery_kwh"] = 0.0
		res["sustain_score"] = 0
	}

	return res
}

func toPythonDriverMap(dto FormulaDriverDto) map[string]any {
	res := map[string]any{
		"first_name": dto.FirstName,
		"last_name":  dto.LastName,
	}
	if dto.RacingNumber != nil {
		res["racing_number"] = *dto.RacingNumber
	} else {
		res["racing_number"] = 0
	}
	if dto.Points != nil {
		res["points"] = *dto.Points
	} else {
		res["points"] = 0
	}
	if dto.TeamID != nil {
		res["team_id"] = *dto.TeamID
	}
	return res
}

// ─── Helpers ─────────────────────────────────────────────────────────────────

// writeJSON is a helper to write JSON responses with proper headers.
func writeJSON(w http.ResponseWriter, status int, data any) {
	w.Header().Set("Content-Type", "application/json")
	if status != http.StatusOK {
		w.WriteHeader(status)
	}
	json.NewEncoder(w).Encode(data)
}

// writeError returns an error response, using 503 for circuit breaker errors.
func writeError(w http.ResponseWriter, err error, fallbackCode int) {
	if errors.Is(err, ErrCircuitOpen) {
		http.Error(w, "Service temporarily unavailable: Python backend is down", http.StatusServiceUnavailable)
		return
	}
	http.Error(w, err.Error(), fallbackCode)
}

// ─── TEAMS HANDLERS ──────────────────────────────────────────────────────────

func handleGetTeams(w http.ResponseWriter, r *http.Request) {
	var pTeams []PythonTeam
	if err := apiGet("/api/teams", &pTeams); err != nil {
		writeError(w, err, 500)
		return
	}

	var filtered []PythonTeam
	for _, t := range pTeams {
		if t.SeriesID >= 1 && t.SeriesID <= 3 {
			filtered = append(filtered, t)
		}
	}

	sort.Slice(filtered, func(i, j int) bool {
		return filtered[i].Points > filtered[j].Points
	})

	dtos := make([]FormulaTeamDto, 0, len(filtered))
	for _, t := range filtered {
		dtos = append(dtos, toTeamDto(t))
	}

	writeJSON(w, http.StatusOK, dtos)
}

func handleCreateTeam(w http.ResponseWriter, r *http.Request) {
	var dto FormulaTeamDto
	if err := json.NewDecoder(r.Body).Decode(&dto); err != nil {
		http.Error(w, err.Error(), 400)
		return
	}

	tType := strings.ToLower(dto.Type)
	var seriesID int64 = 1
	if tType == "fe" {
		seriesID = 3
	} else if tType == "f2" {
		seriesID = 2
	}

	var points int = 0
	if dto.ChampionshipPoints != nil {
		points = *dto.ChampionshipPoints
	}

	t := Team{
		Name:          dto.TeamName,
		PrincipalName: dto.PrincipalName,
		Points:        points,
		SeriesID:      seriesID,
	}

	if errs := ValidateTeam(t); len(errs) > 0 {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusBadRequest)
		_ = json.NewEncoder(w).Encode(map[string]any{"errors": errs})
		return
	}

	payload := toPythonTeamMap(dto)
	resp, err := apiPost("/api/teams", payload)
	if err != nil {
		writeError(w, err, 500)
		return
	}
	defer resp.Body.Close()

	if resp.StatusCode >= 400 {
		body, _ := io.ReadAll(resp.Body)
		http.Error(w, fmt.Sprintf("Python error: %s", body), resp.StatusCode)
		return
	}

	var created PythonTeam
	if err := json.NewDecoder(resp.Body).Decode(&created); err != nil {
		http.Error(w, err.Error(), 500)
		return
	}

	writeJSON(w, http.StatusCreated, toTeamDto(created))
}

func handleUpdateTeam(w http.ResponseWriter, r *http.Request) {
	id := mux.Vars(r)["id"]
	var dto FormulaTeamDto
	if err := json.NewDecoder(r.Body).Decode(&dto); err != nil {
		http.Error(w, err.Error(), 400)
		return
	}

	tType := strings.ToLower(dto.Type)
	var seriesID int64 = 1
	if tType == "fe" {
		seriesID = 3
	} else if tType == "f2" {
		seriesID = 2
	}

	var points int = 0
	if dto.ChampionshipPoints != nil {
		points = *dto.ChampionshipPoints
	}

	t := Team{
		Name:          dto.TeamName,
		PrincipalName: dto.PrincipalName,
		Points:        points,
		SeriesID:      seriesID,
	}

	if errs := ValidateTeam(t); len(errs) > 0 {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusBadRequest)
		_ = json.NewEncoder(w).Encode(map[string]any{"errors": errs})
		return
	}

	payload := toPythonTeamMap(dto)
	resp, err := apiPut("/api/teams/"+id, payload)
	if err != nil {
		writeError(w, err, 500)
		return
	}
	defer resp.Body.Close()

	if resp.StatusCode >= 400 {
		body, _ := io.ReadAll(resp.Body)
		http.Error(w, fmt.Sprintf("Python error: %s", body), resp.StatusCode)
		return
	}

	var updated PythonTeam
	if err := apiGet("/api/teams/"+id, &updated); err != nil {
		writeError(w, err, 500)
		return
	}

	writeJSON(w, http.StatusOK, toTeamDto(updated))
}

func handleDeleteTeam(w http.ResponseWriter, r *http.Request) {
	id := mux.Vars(r)["id"]
	if err := apiDelete("/api/teams/" + id); err != nil {
		writeError(w, err, 500)
		return
	}
	w.WriteHeader(http.StatusNoContent)
}

// ─── DRIVERS HANDLERS ────────────────────────────────────────────────────────

func handleGetDrivers(w http.ResponseWriter, r *http.Request) {
	var pDrivers []PythonDriver
	if err := apiGet("/api/drivers", &pDrivers); err != nil {
		writeError(w, err, 500)
		return
	}

	var filtered []PythonDriver
	for _, d := range pDrivers {
		if d.Team != nil && d.Team.SeriesID >= 1 && d.Team.SeriesID <= 3 {
			filtered = append(filtered, d)
		}
	}

	sort.Slice(filtered, func(i, j int) bool {
		return filtered[i].Points > filtered[j].Points
	})

	dtos := make([]FormulaDriverDto, 0, len(filtered))
	for _, d := range filtered {
		dtos = append(dtos, toDriverDto(d))
	}

	writeJSON(w, http.StatusOK, dtos)
}

func handleCreateDriver(w http.ResponseWriter, r *http.Request) {
	var dto FormulaDriverDto
	if err := json.NewDecoder(r.Body).Decode(&dto); err != nil {
		http.Error(w, err.Error(), 400)
		return
	}

	var racingNumber int = 0
	if dto.RacingNumber != nil {
		racingNumber = *dto.RacingNumber
	}
	var points int = 0
	if dto.Points != nil {
		points = *dto.Points
	}
	var teamID int64 = 0
	if dto.TeamID != nil {
		teamID = *dto.TeamID
	}

	d := Driver{
		FirstName:    dto.FirstName,
		LastName:     dto.LastName,
		RacingNumber: racingNumber,
		Points:       points,
		TeamID:       teamID,
	}

	if errs := ValidateDriver(d); len(errs) > 0 {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusBadRequest)
		_ = json.NewEncoder(w).Encode(map[string]any{"errors": errs})
		return
	}

	payload := toPythonDriverMap(dto)
	resp, err := apiPost("/api/drivers", payload)
	if err != nil {
		writeError(w, err, 500)
		return
	}
	defer resp.Body.Close()

	if resp.StatusCode >= 400 {
		body, _ := io.ReadAll(resp.Body)
		http.Error(w, fmt.Sprintf("Python error: %s", body), resp.StatusCode)
		return
	}

	var created PythonDriver
	if err := json.NewDecoder(resp.Body).Decode(&created); err != nil {
		http.Error(w, err.Error(), 500)
		return
	}

	writeJSON(w, http.StatusCreated, toDriverDto(created))
}

func handleUpdateDriver(w http.ResponseWriter, r *http.Request) {
	id := mux.Vars(r)["id"]
	var dto FormulaDriverDto
	if err := json.NewDecoder(r.Body).Decode(&dto); err != nil {
		http.Error(w, err.Error(), 400)
		return
	}

	var racingNumber int = 0
	if dto.RacingNumber != nil {
		racingNumber = *dto.RacingNumber
	}
	var points int = 0
	if dto.Points != nil {
		points = *dto.Points
	}
	var teamID int64 = 0
	if dto.TeamID != nil {
		teamID = *dto.TeamID
	}

	d := Driver{
		FirstName:    dto.FirstName,
		LastName:     dto.LastName,
		RacingNumber: racingNumber,
		Points:       points,
		TeamID:       teamID,
	}

	if errs := ValidateDriver(d); len(errs) > 0 {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusBadRequest)
		_ = json.NewEncoder(w).Encode(map[string]any{"errors": errs})
		return
	}

	payload := toPythonDriverMap(dto)
	resp, err := apiPut("/api/drivers/"+id, payload)
	if err != nil {
		writeError(w, err, 500)
		return
	}
	defer resp.Body.Close()

	if resp.StatusCode >= 400 {
		body, _ := io.ReadAll(resp.Body)
		http.Error(w, fmt.Sprintf("Python error: %s", body), resp.StatusCode)
		return
	}

	var updated PythonDriver
	if err := json.NewDecoder(resp.Body).Decode(&updated); err != nil {
		http.Error(w, err.Error(), 500)
		return
	}

	writeJSON(w, http.StatusOK, toDriverDto(updated))
}

func handleDeleteDriver(w http.ResponseWriter, r *http.Request) {
	id := mux.Vars(r)["id"]
	if err := apiDelete("/api/drivers/" + id); err != nil {
		writeError(w, err, 500)
		return
	}
	w.WriteHeader(http.StatusNoContent)
}

// ─── SIMULATION & COMPARISON ─────────────────────────────────────────────────

func handleSimulateRace(w http.ResponseWriter, r *http.Request) {
	teamID := mux.Vars(r)["id"]
	positionStr := r.URL.Query().Get("position")
	driverIDStr := r.URL.Query().Get("driverId")

	position, err := strconv.Atoi(positionStr)
	if err != nil {
		http.Error(w, "Invalid position", 400)
		return
	}

	driverID, err := strconv.ParseInt(driverIDStr, 10, 64)
	if err != nil {
		http.Error(w, "Invalid driverId", 400)
		return
	}

	// ── Parallel fetch: team and driver simultaneously via goroutines ──
	var team PythonTeam
	var driver PythonDriver
	var teamErr, driverErr error

	var wg sync.WaitGroup
	wg.Add(2)

	go func() {
		defer wg.Done()
		teamErr = apiGet("/api/teams/"+teamID, &team)
	}()

	go func() {
		defer wg.Done()
		driverErr = apiGet("/api/drivers/"+strconv.FormatInt(driverID, 10), &driver)
	}()

	wg.Wait()

	if teamErr != nil {
		writeError(w, teamErr, 404)
		return
	}
	if driverErr != nil {
		writeError(w, driverErr, 400)
		return
	}

	if driver.TeamID != team.ID {
		http.Error(w, "Driver does not belong to team", 400)
		return
	}

	points := calculatePolymorphicPoints(team.SeriesID, position)

	driver.Points += points
	team.Points += points

	if position == 1 {
		team.Wins++
		team.Podiums++
	} else if position <= 3 {
		team.Podiums++
	}

	driverPayload := toPythonDriverMap(toDriverDto(driver))
	if _, err := apiPut("/api/drivers/"+strconv.FormatInt(driverID, 10), driverPayload); err != nil {
		writeError(w, err, 500)
		return
	}

	teamPayload := toPythonTeamMap(toTeamDto(team))
	if _, err := apiPut("/api/teams/"+teamID, teamPayload); err != nil {
		writeError(w, err, 500)
		return
	}

	if err := apiGet("/api/teams/"+teamID, &team); err != nil {
		writeError(w, err, 500)
		return
	}

	teamDto := toTeamDto(team)
	result := map[string]any{
		"message": fmt.Sprintf("Simulated position P%d for %s %s (#%d). Calculated %d pts added to driver and team.",
			position, driver.FirstName, driver.LastName, driver.RacingNumber, points),
		"teamSummary": teamDto.Summary,
		"team":        teamDto,
	}

	writeJSON(w, http.StatusOK, result)
}

// handleCompareTeams fetches two teams in PARALLEL using goroutines,
// then compares their championship standings.
func handleCompareTeams(w http.ResponseWriter, r *http.Request) {
	teamIdA := r.URL.Query().Get("teamIdA")
	teamIdB := r.URL.Query().Get("teamIdB")

	var teamA, teamB PythonTeam
	var errA, errB error

	// ── Parallel fetch: both teams at the same time ──
	var wg sync.WaitGroup
	wg.Add(2)

	go func() {
		defer wg.Done()
		errA = apiGet("/api/teams/"+teamIdA, &teamA)
	}()

	go func() {
		defer wg.Done()
		errB = apiGet("/api/teams/"+teamIdB, &teamB)
	}()

	wg.Wait()

	if errA != nil {
		writeError(w, errA, 400)
		return
	}
	if errB != nil {
		writeError(w, errB, 400)
		return
	}

	pointsA := teamA.Points
	pointsB := teamB.Points

	var resultMessage string
	var winnerID int64 = 0

	if pointsA > pointsB {
		resultMessage = fmt.Sprintf("%s is ahead of %s in the championship.", teamA.Name, teamB.Name)
		winnerID = teamA.ID
	} else if pointsA < pointsB {
		resultMessage = fmt.Sprintf("%s is ahead of %s in the championship.", teamB.Name, teamA.Name)
		winnerID = teamB.ID
	} else {
		resultMessage = "Both teams have equal championship points."
	}

	result := map[string]any{
		"message":  resultMessage,
		"teamA":    map[string]any{"id": teamA.ID, "name": teamA.Name, "points": pointsA},
		"teamB":    map[string]any{"id": teamB.ID, "name": teamB.Name, "points": pointsB},
		"winnerId": winnerID,
	}

	writeJSON(w, http.StatusOK, result)
}

// handleSimulateGlobalRace processes a batch of race results. Each driver's
// data is fetched and updated CONCURRENTLY using a goroutine pool, demonstrating
// Go's strength in parallel I/O-bound workloads in distributed systems.
func handleSimulateGlobalRace(w http.ResponseWriter, r *http.Request) {
	var raceResults []DriverPositionResult
	if err := json.NewDecoder(r.Body).Decode(&raceResults); err != nil {
		http.Error(w, err.Error(), 400)
		return
	}

	// Use a mutex to protect shared team state across goroutines.
	// Multiple drivers can belong to the same team, so concurrent
	// updates to team points must be serialized.
	teamUpdates := make(map[int64]*PythonTeam)
	var mu sync.Mutex
	var wg sync.WaitGroup

	for _, result := range raceResults {
		if result.DriverID == nil || result.Position == nil {
			continue
		}

		wg.Add(1)
		go func(driverID int64, position int) {
			defer wg.Done()

			dIDStr := strconv.FormatInt(driverID, 10)
			var driver PythonDriver
			if err := apiGet("/api/drivers/"+dIDStr, &driver); err != nil {
				return
			}

			tIDStr := strconv.FormatInt(driver.TeamID, 10)

			mu.Lock()
			team, exists := teamUpdates[driver.TeamID]
			if !exists {
				team = &PythonTeam{}
				if err := apiGet("/api/teams/"+tIDStr, team); err != nil {
					mu.Unlock()
					return
				}
				teamUpdates[driver.TeamID] = team
			}

			points := calculatePolymorphicPoints(team.SeriesID, position)
			driver.Points += points
			team.Points += points

			if position == 1 {
				team.Wins++
				team.Podiums++
			} else if position <= 3 {
				team.Podiums++
			}
			mu.Unlock()

			driverPayload := toPythonDriverMap(toDriverDto(driver))
			_, _ = apiPut("/api/drivers/"+dIDStr, driverPayload)
		}(*result.DriverID, *result.Position)
	}

	wg.Wait()

	// Flush accumulated team updates
	for tID, team := range teamUpdates {
		tIDStr := strconv.FormatInt(tID, 10)
		teamPayload := toPythonTeamMap(toTeamDto(*team))
		_, _ = apiPut("/api/teams/"+tIDStr, teamPayload)
	}

	writeJSON(w, http.StatusOK, map[string]string{
		"message": "Global race simulation completed successfully. Driver and team standings updated.",
	})
}

// handleGetStats computes aggregated statistics. Teams and drivers are fetched
// in PARALLEL via goroutines, cutting latency in half compared to sequential.
func handleGetStats(w http.ResponseWriter, r *http.Request) {
	seriesFilter := r.URL.Query().Get("series")
	if seriesFilter == "" {
		seriesFilter = "all"
	}

	// ── Parallel fetch: teams and drivers simultaneously ──
	var allTeams []PythonTeam
	var allDrivers []PythonDriver
	var teamsErr, driversErr error

	var wg sync.WaitGroup
	wg.Add(2)

	go func() {
		defer wg.Done()
		teamsErr = apiGet("/api/teams", &allTeams)
	}()

	go func() {
		defer wg.Done()
		driversErr = apiGet("/api/drivers", &allDrivers)
	}()

	wg.Wait()

	if teamsErr != nil {
		writeError(w, teamsErr, 500)
		return
	}
	if driversErr != nil {
		writeError(w, driversErr, 500)
		return
	}

	var filteredTeams []PythonTeam
	for _, t := range allTeams {
		if matchesSeries(t.SeriesID, seriesFilter) {
			filteredTeams = append(filteredTeams, t)
		}
	}

	var filteredDrivers []PythonDriver
	for _, d := range allDrivers {
		if d.Team != nil && matchesSeries(d.Team.SeriesID, seriesFilter) {
			filteredDrivers = append(filteredDrivers, d)
		}
	}

	var leadingTeam *PythonTeam
	for i, t := range filteredTeams {
		if leadingTeam == nil || t.Points > leadingTeam.Points {
			leadingTeam = &filteredTeams[i]
		}
	}

	var leadingDriver *PythonDriver
	for i, d := range filteredDrivers {
		if leadingDriver == nil || d.Points > leadingDriver.Points {
			leadingDriver = &filteredDrivers[i]
		}
	}

	var avgPoints float64
	if len(filteredDrivers) > 0 {
		sum := 0
		for _, d := range filteredDrivers {
			sum += d.Points
		}
		avgPoints = float64(sum) / float64(len(filteredDrivers))
	}
	roundedAvg := math.Round(avgPoints*10.0) / 10.0

	leadingTeamName := "N/A"
	leadingTeamPoints := 0
	leadingTeamWins := 0
	if leadingTeam != nil {
		leadingTeamName = leadingTeam.Name
		leadingTeamPoints = leadingTeam.Points
		leadingTeamWins = leadingTeam.Wins
	}

	leadingDriverName := "N/A"
	leadingDriverPoints := 0
	leadingDriverNumber := 0
	if leadingDriver != nil {
		leadingDriverName = leadingDriver.FirstName + " " + leadingDriver.LastName
		leadingDriverPoints = leadingDriver.Points
		leadingDriverNumber = leadingDriver.RacingNumber
	}

	stats := map[string]any{
		"registeredTeamsCount": len(filteredTeams),
		"activeDriversCount":   len(filteredDrivers),
		"averagePoints":        roundedAvg,
		"leadingTeamName":      leadingTeamName,
		"leadingTeamPoints":    leadingTeamPoints,
		"leadingTeamWins":      leadingTeamWins,
		"leadingDriverName":    leadingDriverName,
		"leadingDriverPoints":  leadingDriverPoints,
		"leadingDriverNumber":  leadingDriverNumber,
	}

	writeJSON(w, http.StatusOK, stats)
}

// ─── HEALTH CHECK ────────────────────────────────────────────────────────────

// handleHealthCheck probes the Go BFF itself and the Python backend IN PARALLEL,
// returning the availability and latency of each service component.
func handleHealthCheck(w http.ResponseWriter, r *http.Request) {
	type serviceStatus struct {
		Status  string `json:"status"`
		Latency string `json:"latency"`
	}

	goStatus := serviceStatus{Status: "up", Latency: "0ms"}

	var pythonStatus serviceStatus
	var pythonWg sync.WaitGroup
	pythonWg.Add(1)

	go func() {
		defer pythonWg.Done()
		start := time.Now()
		resp, err := httpClient.Get(pythonBase + "/api/series")
		latency := time.Since(start)

		if err != nil {
			pythonStatus = serviceStatus{Status: "down", Latency: latency.String()}
			return
		}
		defer resp.Body.Close()

		if resp.StatusCode >= 500 {
			pythonStatus = serviceStatus{Status: "degraded", Latency: latency.String()}
			return
		}
		pythonStatus = serviceStatus{Status: "up", Latency: latency.String()}
	}()

	pythonWg.Wait()

	overallStatus := "healthy"
	if pythonStatus.Status != "up" {
		overallStatus = "degraded"
	}

	result := map[string]any{
		"status":          overallStatus,
		"circuit_breaker": breaker.Stats(),
		"services": map[string]serviceStatus{
			"go_bff":         goStatus,
			"python_data_api": pythonStatus,
		},
	}

	writeJSON(w, http.StatusOK, result)
}

// handleStatus returns the lab identifier and backend name.
// It also probes the Python data API to report its availability.
func handleStatus(w http.ResponseWriter, r *http.Request) {
	pyStatus := "unknown"
	resp, err := httpClient.Get(pythonBase + "/api/series")
	if err != nil {
		pyStatus = "down"
	} else {
		resp.Body.Close()
		if resp.StatusCode >= 500 {
			pyStatus = "degraded"
		} else {
			pyStatus = "up"
		}
	}

	writeJSON(w, http.StatusOK, map[string]any{
		"lab":           "lab4",
		"name":          "Python + Go",
		"pythonHealthy": pyStatus == "up",
	})
}

// ─── DOMAIN HELPERS ──────────────────────────────────────────────────────────

func matchesSeries(seriesID int64, seriesFilter string) bool {
	if seriesID < 1 || seriesID > 3 {
		return false
	}
	f := strings.ToLower(seriesFilter)
	if f == "" || f == "all" {
		return true
	}
	if f == "f1" && seriesID == 1 {
		return true
	}
	if f == "f2" && seriesID == 2 {
		return true
	}
	if f == "fe" && seriesID == 3 {
		return true
	}
	return false
}

func calculatePolymorphicPoints(seriesID int64, position int) int {
	if position < 1 || position > 10 {
		return 0
	}
	if seriesID == 2 { // Formula 2
		pts := []int{15, 12, 10, 8, 6, 5, 4, 3, 2, 1}
		return pts[position-1]
	}
	// Formula 1 or Formula E
	pts := []int{25, 18, 15, 12, 10, 8, 6, 4, 2, 1}
	return pts[position-1]
}

func getPowerUnitStatus(wins int, powerUnit string) string {
	if wins > 10 {
		return powerUnit + " [Dominant]"
	}
	if wins > 4 {
		return powerUnit + " [Competitive]"
	}
	return powerUnit + " [Development]"
}

func getFeederStatus(isFeeder bool, graduates int) string {
	if !isFeeder {
		return "Independent"
	}
	if graduates >= 5 {
		return "Elite Feeder (5+ F1 graduates)"
	}
	if graduates >= 2 {
		return "Active Feeder"
	}
	return "Developing Feeder"
}

func getBatteryEfficiencyRating(batteryKwh float64) string {
	if batteryKwh >= 40.0 {
		return "Next-Gen (40+ kWh)"
	}
	if batteryKwh >= 38.0 {
		return "Gen3 Standard (38 kWh)"
	}
	return "Legacy"
}

func getVisibleTeamColor(hex string) string {
	hex = strings.TrimSpace(hex)
	if hex == "" {
		return "#3b82f6"
	}
	cleanHex := strings.TrimPrefix(hex, "#")
	if strings.EqualFold(cleanHex, "000000") || strings.EqualFold(cleanHex, "000") ||
		strings.EqualFold(cleanHex, "1e293b") || strings.EqualFold(cleanHex, "18181b") {
		return "#a1a1aa"
	}
	if len(cleanHex) == 3 {
		cleanHex = string([]byte{
			cleanHex[0], cleanHex[0],
			cleanHex[1], cleanHex[1],
			cleanHex[2], cleanHex[2],
		})
	}
	if len(cleanHex) != 6 {
		return hex
	}
	rVal, err1 := strconv.ParseInt(cleanHex[0:2], 16, 64)
	gVal, err2 := strconv.ParseInt(cleanHex[2:4], 16, 64)
	bVal, err3 := strconv.ParseInt(cleanHex[4:6], 16, 64)
	if err1 != nil || err2 != nil || err3 != nil {
		return hex
	}

	rv := float64(rVal) / 255.0
	gv := float64(gVal) / 255.0
	bv := float64(bVal) / 255.0

	luminance := 0.2126*rv + 0.7152*gv + 0.0722*bv
	if luminance < 0.35 {
		mx := rv
		if gv > mx {
			mx = gv
		}
		if bv > mx {
			mx = bv
		}
		mn := rv
		if gv < mn {
			mn = gv
		}
		if bv < mn {
			mn = bv
		}
		h, s, l := 0.0, 0.0, (mx+mn)/2.0

		if mx != mn {
			d := mx - mn
			if l > 0.5 {
				s = d / (2.0 - mx - mn)
			} else {
				s = d / (mx + mn)
			}
			if mx == rv {
				h = (gv - bv) / d
				if gv < bv {
					h += 6.0
				}
			} else if mx == gv {
				h = (bv-rv)/d + 2.0
			} else {
				h = (rv-gv)/d + 4.0
			}
			h /= 6.0
		}

		if l < 0.55 {
			l = 0.55
		}
		if s < 0.65 {
			s = 0.65
		}

		var q float64
		if l < 0.5 {
			q = l * (1.0 + s)
		} else {
			q = l + s - l*s
		}
		p := 2.0*l - q

		rv = hue2Rgb(p, q, h+1.0/3.0)
		gv = hue2Rgb(p, q, h)
		bv = hue2Rgb(p, q, h-1.0/3.0)

		return fmt.Sprintf("#%02x%02x%02x",
			int(math.Round(rv*255.0)),
			int(math.Round(gv*255.0)),
			int(math.Round(bv*255.0)))
	}
	return hex
}

func hue2Rgb(p, q, t float64) float64 {
	if t < 0 {
		t += 1.0
	}
	if t > 1.0 {
		t -= 1.0
	}
	if t < 1.0/6.0 {
		return p + (q-p)*6.0*t
	}
	if t < 1.0/2.0 {
		return q
	}
	if t < 2.0/3.0 {
		return p + (q-p)*(2.0/3.0-t)*6.0
	}
	return p
}
