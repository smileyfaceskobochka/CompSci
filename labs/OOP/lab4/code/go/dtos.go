package main

type FormulaDriverDto struct {
	ID            *int64 `json:"id"`
	TeamID        *int64 `json:"teamId"`
	FirstName     string `json:"firstName"`
	LastName      string `json:"lastName"`
	RacingNumber  *int   `json:"racingNumber"`
	Points        *int   `json:"points"`
	FullName      string `json:"fullName"`
	DisplayString string `json:"displayString"`
}

type FormulaTeamDto struct {
	ID                 *int64              `json:"id"`
	Type               string              `json:"type"` // "f1", "f2", "fe"
	TeamName           string              `json:"teamName"`
	PrincipalName      string              `json:"principalName"`
	Headquarters       string              `json:"headquarters"`
	TeamColor          string              `json:"team_color"`
	ChampionshipPoints *int                `json:"championshipPoints"`
	RaceWins           *int                `json:"raceWins"`
	Podiums            *int                `json:"podiums"`
	Drivers            []FormulaDriverDto  `json:"drivers"`
	SeriesName         string              `json:"seriesName"`
	Summary            string              `json:"summary"`
	VisibleColor       string              `json:"visible_color"`

	// F1
	PowerUnit      string   `json:"powerUnit,omitempty"`
	BudgetCapMln   *float64 `json:"budgetCapMln,omitempty"`
	ConstructorPos *int     `json:"constructorPos,omitempty"`
	EngineStatus   string   `json:"engine_status,omitempty"`

	// F2
	ChassisModel     string `json:"chassisModel,omitempty"`
	F1Graduates      *int   `json:"f1Graduates,omitempty"`
	IsFeederSeries   *bool  `json:"isFeederSeries,omitempty"`
	FeederStatusText string `json:"feeder_status_text,omitempty"`

	// FE
	EnergyPartner       string   `json:"energyPartner,omitempty"`
	BatteryCapacityKwh  *float64 `json:"batteryCapacityKwh,omitempty"`
	SustainabilityScore *int     `json:"sustainabilityScore,omitempty"`
	BatteryRatingText   string   `json:"battery_rating_text,omitempty"`
}

type DriverPositionResult struct {
	DriverID *int64 `json:"driverId"`
	Position *int   `json:"position"`
}
