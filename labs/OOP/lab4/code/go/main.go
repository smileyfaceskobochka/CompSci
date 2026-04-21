package main

import (
	"log"
	"net/http"

	"github.com/gorilla/mux"
)

const goPort = ":8080"

func main() {
	loadTemplates()

	r := mux.NewRouter()

	// Static files
	r.PathPrefix("/static/").Handler(
		http.StripPrefix("/static/", http.FileServer(http.Dir("static"))),
	)

	// Pages
	r.HandleFunc("/", handleIndex).Methods("GET")
	r.HandleFunc("/teams", handleTeams).Methods("GET")
	r.HandleFunc("/teams/new", handleTeamNew).Methods("GET")
	r.HandleFunc("/teams", handleTeamCreate).Methods("POST")
	r.HandleFunc("/teams/{id}/edit", handleTeamEdit).Methods("GET")
	r.HandleFunc("/teams/{id}", handleTeamUpdate).Methods("POST")
	r.HandleFunc("/teams/{id}/delete", handleTeamDelete).Methods("POST")

	r.HandleFunc("/drivers", handleDrivers).Methods("GET")
	r.HandleFunc("/drivers/new", handleDriverNew).Methods("GET")
	r.HandleFunc("/drivers", handleDriverCreate).Methods("POST")
	r.HandleFunc("/drivers/{id}/edit", handleDriverEdit).Methods("GET")
	r.HandleFunc("/drivers/{id}", handleDriverUpdate).Methods("POST")
	r.HandleFunc("/drivers/{id}/delete", handleDriverDelete).Methods("POST")

	// JSON API proxy
	r.PathPrefix("/api/").HandlerFunc(apiProxy)

	log.Printf("Go server listening on %s", goPort)
	log.Fatal(http.ListenAndServe(goPort, r))
}
