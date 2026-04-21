package main

import (
	"fmt"
	"html/template"
	"log"
	"net/http"
	"net/http/httputil"
	"net/url"
	"strconv"

	"github.com/gorilla/mux"
)

var templates = make(map[string]*template.Template)

func loadTemplates() {
	funcMap := template.FuncMap{
		"add":         func(a, b int) int { return a + b },
		"f1pts":       CalculateF1Points,
		"winrate":     GetWinRate,
		"formatFloat": func(f float64) string { return fmt.Sprintf("%.1f", f) },
		"seriesClass": func(name string) string {
			switch name {
			case "Formula 1": return "f1"
			case "Formula 2": return "f2"
			case "Formula E": return "fe"
			case "WEC":       return "wec"
			case "IndyCar":   return "indy"
			case "MotoGP":    return "moto"
			default:          return ""
			}
		},
	}
	
	pages := []string{"index.html", "teams.html", "team_form.html", "drivers.html", "driver_form.html"}
	for _, page := range pages {
		t := template.New(page).Funcs(funcMap)
		t = template.Must(t.ParseFiles("templates/layout.html", "templates/"+page))
		templates[page] = t
	}
}

func render(w http.ResponseWriter, name string, data any) {
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	t, ok := templates[name]
	if !ok {
		http.Error(w, "template not found", 500)
		return
	}
	if err := t.ExecuteTemplate(w, "layout.html", data); err != nil {
		log.Printf("template error: %v", err)
		http.Error(w, "render error", 500)
	}
}

func handleIndex(w http.ResponseWriter, r *http.Request) {
	var teams []Team
	var drivers []Driver
	var series []Series
	_ = apiGet("/api/teams", &teams)
	_ = apiGet("/api/drivers", &drivers)
	_ = apiGet("/api/series", &series)
	stats := ComputeStats(teams, drivers, series)
	render(w, "index.html", map[string]any{
		"Teams":   teams,
		"Drivers": drivers,
		"Series":  series,
		"Stats":   stats,
	})
}

func handleTeams(w http.ResponseWriter, r *http.Request) {
	var teams []Team
	_ = apiGet("/api/teams", &teams)

	sort_ := r.URL.Query().Get("sort")
	order := r.URL.Query().Get("order")
	minPts := r.URL.Query().Get("min_points")

	if minPts != "" {
		if n, err := strconv.Atoi(minPts); err == nil {
			teams = FilterTopTeams(teams, n)
		}
	}
	if sort_ != "" {
		if order == "" {
			order = "desc"
		}
		teams = SortTeams(teams, sort_, order)
	}

	var series []Series
	_ = apiGet("/api/series", &series)
	render(w, "teams.html", map[string]any{
		"Teams":     teams,
		"Series":    series,
		"SortField": sort_,
		"SortOrder": order,
		"MinPoints": minPts,
	})
}

func handleTeamNew(w http.ResponseWriter, r *http.Request) {
	var series []Series
	_ = apiGet("/api/series", &series)
	render(w, "team_form.html", map[string]any{
		"Team":   Team{},
		"Series": series,
		"Errors": []string{},
		"IsNew":  true,
	})
}

func handleTeamCreate(w http.ResponseWriter, r *http.Request) {
	r.ParseForm()
	points, _ := strconv.Atoi(r.FormValue("points"))
	seriesID, _ := strconv.ParseInt(r.FormValue("series_id"), 10, 64)

	t := Team{
		Name:          r.FormValue("name"),
		PrincipalName: r.FormValue("principal_name"),
		BaseLocation:  r.FormValue("base_location"),
		PowerUnit:     r.FormValue("power_unit"),
		Points:        points,
		SeriesID:      seriesID,
	}

	if errs := ValidateTeam(t); len(errs) > 0 {
		var series []Series
		_ = apiGet("/api/series", &series)
		render(w, "team_form.html", map[string]any{"Team": t, "Series": series, "Errors": errs, "IsNew": true})
		return
	}

	resp, err := apiPost("/api/teams", t)
	if err != nil || resp.StatusCode >= 400 {
		http.Error(w, "Failed to create team", 500)
		return
	}
	http.Redirect(w, r, "/teams", http.StatusSeeOther)
}

func handleTeamEdit(w http.ResponseWriter, r *http.Request) {
	id := mux.Vars(r)["id"]
	var t Team
	if err := apiGet("/api/teams/"+id, &t); err != nil {
		http.Error(w, "Not found", 404)
		return
	}
	var series []Series
	_ = apiGet("/api/series", &series)
	render(w, "team_form.html", map[string]any{"Team": t, "Series": series, "Errors": []string{}, "IsNew": false})
}

func handleTeamUpdate(w http.ResponseWriter, r *http.Request) {
	id := mux.Vars(r)["id"]
	r.ParseForm()
	points, _ := strconv.Atoi(r.FormValue("points"))
	seriesID, _ := strconv.ParseInt(r.FormValue("series_id"), 10, 64)

	t := Team{
		Name:          r.FormValue("name"),
		PrincipalName: r.FormValue("principal_name"),
		BaseLocation:  r.FormValue("base_location"),
		PowerUnit:     r.FormValue("power_unit"),
		Points:        points,
		SeriesID:      seriesID,
	}

	if errs := ValidateTeam(t); len(errs) > 0 {
		var series []Series
		_ = apiGet("/api/series", &series)
		render(w, "team_form.html", map[string]any{"Team": t, "Series": series, "Errors": errs, "IsNew": false})
		return
	}

	resp, err := apiPut("/api/teams/"+id, t)
	if err != nil || resp.StatusCode >= 400 {
		http.Error(w, "Failed to update team", 500)
		return
	}
	http.Redirect(w, r, "/teams", http.StatusSeeOther)
}

func handleTeamDelete(w http.ResponseWriter, r *http.Request) {
	id := mux.Vars(r)["id"]
	_ = apiDelete("/api/teams/" + id)
	http.Redirect(w, r, "/teams", http.StatusSeeOther)
}

func handleDrivers(w http.ResponseWriter, r *http.Request) {
	var drivers []Driver
	_ = apiGet("/api/drivers", &drivers)

	sortF := r.URL.Query().Get("sort")
	order := r.URL.Query().Get("order")
	seriesF := r.URL.Query().Get("series_id")

	if seriesF != "" {
		var filtered []Driver
		_ = apiGet("/api/drivers?series_id="+seriesF, &filtered)
		drivers = filtered
	}
	if sortF != "" {
		if order == "" {
			order = "desc"
		}
		drivers = SortDrivers(drivers, sortF, order)
	}

	var teams []Team
	_ = apiGet("/api/teams", &teams)
	var series []Series
	_ = apiGet("/api/series", &series)

	render(w, "drivers.html", map[string]any{
		"Drivers":        drivers,
		"Teams":          teams,
		"Series":         series,
		"SelectedSeries": seriesF,
	})
}

func handleDriverNew(w http.ResponseWriter, r *http.Request) {
	var teams []Team
	_ = apiGet("/api/teams", &teams)
	render(w, "driver_form.html", map[string]any{
		"Driver": Driver{},
		"Teams":  teams,
		"Errors": []string{},
		"IsNew":  true,
	})
}

func handleDriverCreate(w http.ResponseWriter, r *http.Request) {
	r.ParseForm()
	number, _ := strconv.Atoi(r.FormValue("racing_number"))
	points, _ := strconv.Atoi(r.FormValue("points"))
	teamID, _ := strconv.ParseInt(r.FormValue("team_id"), 10, 64)

	d := Driver{
		FirstName:    r.FormValue("first_name"),
		LastName:     r.FormValue("last_name"),
		RacingNumber: number,
		Points:       points,
		TeamID:       teamID,
	}

	if errs := ValidateDriver(d); len(errs) > 0 {
		var teams []Team
		_ = apiGet("/api/teams", &teams)
		render(w, "driver_form.html", map[string]any{"Driver": d, "Teams": teams, "Errors": errs, "IsNew": true})
		return
	}

	resp, err := apiPost("/api/drivers", d)
	if err != nil || resp.StatusCode >= 400 {
		http.Error(w, "Failed to create driver", 500)
		return
	}
	http.Redirect(w, r, "/drivers", http.StatusSeeOther)
}

func handleDriverEdit(w http.ResponseWriter, r *http.Request) {
	id := mux.Vars(r)["id"]
	var d Driver
	if err := apiGet("/api/drivers/"+id, &d); err != nil {
		http.Error(w, "Not found", 404)
		return
	}
	var teams []Team
	_ = apiGet("/api/teams", &teams)
	render(w, "driver_form.html", map[string]any{"Driver": d, "Teams": teams, "Errors": []string{}, "IsNew": false})
}

func handleDriverUpdate(w http.ResponseWriter, r *http.Request) {
	id := mux.Vars(r)["id"]
	r.ParseForm()
	number, _ := strconv.Atoi(r.FormValue("racing_number"))
	points, _ := strconv.Atoi(r.FormValue("points"))
	teamID, _ := strconv.ParseInt(r.FormValue("team_id"), 10, 64)

	d := Driver{
		FirstName:    r.FormValue("first_name"),
		LastName:     r.FormValue("last_name"),
		RacingNumber: number,
		Points:       points,
		TeamID:       teamID,
	}

	if errs := ValidateDriver(d); len(errs) > 0 {
		var teams []Team
		_ = apiGet("/api/teams", &teams)
		render(w, "driver_form.html", map[string]any{"Driver": d, "Teams": teams, "Errors": errs, "IsNew": false})
		return
	}

	resp, err := apiPut("/api/drivers/"+id, d)
	if err != nil || resp.StatusCode >= 400 {
		http.Error(w, "Failed to update driver", 500)
		return
	}
	http.Redirect(w, r, "/drivers", http.StatusSeeOther)
}

func handleDriverDelete(w http.ResponseWriter, r *http.Request) {
	id := mux.Vars(r)["id"]
	_ = apiDelete("/api/drivers/" + id)
	http.Redirect(w, r, "/drivers", http.StatusSeeOther)
}

func apiProxy(w http.ResponseWriter, r *http.Request) {
	target, _ := url.Parse(pythonBase)
	proxy := httputil.NewSingleHostReverseProxy(target)
	r.URL.Host = target.Host
	r.URL.Scheme = target.Scheme
	r.Header.Set("X-Forwarded-Host", r.Header.Get("Host"))
	r.Host = target.Host
	proxy.ServeHTTP(w, r)
}
