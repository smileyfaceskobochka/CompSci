package main

type Series struct {
	ID            int64  `json:"id"`
	Name          string `json:"name"`
	OfficialName  string `json:"official_name"`
	Headquarters  string `json:"headquarters"`
	GoverningBody string `json:"governing_body"`
	FoundedYear   int    `json:"founded_year"`
	Rounds        int    `json:"rounds"`
	Teams         []Team `json:"teams,omitempty"`
}

type Team struct {
	ID            int64    `json:"id"`
	Name          string   `json:"name"`
	PrincipalName string   `json:"principal_name"`
	BaseLocation  string   `json:"base_location"`
	PowerUnit     string   `json:"power_unit"`
	Points        int      `json:"points"`
	SeriesID      int64    `json:"series_id"`
	TeamColor     string   `json:"team_color"`
	Series        *Series  `json:"series,omitempty"`
	Drivers       []Driver `json:"drivers,omitempty"`
}

type Driver struct {
	ID           int64  `json:"id"`
	FirstName    string `json:"first_name"`
	LastName     string `json:"last_name"`
	FullName     string `json:"full_name"`
	RacingNumber int    `json:"racing_number"`
	Points       int    `json:"points"`
	TeamID       int64  `json:"team_id"`
	Team         *Team  `json:"team,omitempty"`
}
