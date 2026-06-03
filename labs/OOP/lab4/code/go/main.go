package main

import (
	"context"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/gorilla/mux"
)

var goPort = ":" + getEnv("GO_PORT", "5000")

func main() {
	r := mux.NewRouter()

	// ── Middleware Pipeline ───────────────────────────────────────────────
	// Order matters: Recovery wraps everything (outermost), then Logging,
	// then CORS, then Timing (innermost, closest to handlers).
	r.Use(RecoveryMiddleware)
	r.Use(LoggingMiddleware)
	r.Use(corsMiddleware)
	r.Use(TimingMiddleware)

	// ── REST API Routes ──────────────────────────────────────────────────
	r.HandleFunc("/api/teams", handleGetTeams).Methods("GET", "OPTIONS")
	r.HandleFunc("/api/teams", handleCreateTeam).Methods("POST", "OPTIONS")
	r.HandleFunc("/api/teams/{id}", handleUpdateTeam).Methods("PUT", "OPTIONS")
	r.HandleFunc("/api/teams/{id}", handleDeleteTeam).Methods("DELETE", "OPTIONS")

	r.HandleFunc("/api/drivers", handleGetDrivers).Methods("GET", "OPTIONS")
	r.HandleFunc("/api/drivers", handleCreateDriver).Methods("POST", "OPTIONS")
	r.HandleFunc("/api/drivers/{id}", handleUpdateDriver).Methods("PUT", "OPTIONS")
	r.HandleFunc("/api/drivers/{id}", handleDeleteDriver).Methods("DELETE", "OPTIONS")

	r.HandleFunc("/api/teams/{id}/simulate-race", handleSimulateRace).Methods("POST", "OPTIONS")
	r.HandleFunc("/api/teams/compare", handleCompareTeams).Methods("GET", "OPTIONS")
	r.HandleFunc("/api/simulation/race", handleSimulateGlobalRace).Methods("POST", "OPTIONS")
	r.HandleFunc("/api/stats", handleGetStats).Methods("GET", "OPTIONS")

	// ── Health & Diagnostics ─────────────────────────────────────────────
	r.HandleFunc("/api/health", handleHealthCheck).Methods("GET", "OPTIONS")
	r.HandleFunc("/api/status", handleStatus).Methods("GET", "OPTIONS")

	// ── HTTP Server with Timeouts ────────────────────────────────────────
	srv := &http.Server{
		Addr:         goPort,
		Handler:      r,
		ReadTimeout:  15 * time.Second,
		WriteTimeout: 15 * time.Second,
		IdleTimeout:  60 * time.Second,
	}

	// ── Graceful Shutdown ────────────────────────────────────────────────
	// Start the server in a goroutine so that it doesn't block the
	// signal listener below.
	go func() {
		log.Printf("Go BFF server listening on %s (Python API: %s)", goPort, pythonBase)
		log.Printf("Middleware: Recovery → Logging → CORS → Timing")
		log.Printf("Circuit Breaker: threshold=%d, timeout=30s", breaker.threshold)
		if err := srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatalf("Server error: %v", err)
		}
	}()

	// Listen for OS interrupt signals (Ctrl+C, docker stop, kill)
	quit := make(chan os.Signal, 1)
	signal.Notify(quit, syscall.SIGINT, syscall.SIGTERM)
	sig := <-quit

	log.Printf("Received signal %v — initiating graceful shutdown...", sig)

	// Create a deadline context: allow 5 seconds for in-flight requests
	// to complete before forcing shutdown.
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	if err := srv.Shutdown(ctx); err != nil {
		log.Fatalf("Server forced to shutdown: %v", err)
	}

	log.Println("Server exited gracefully.")
}

func corsMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization, X-Requested-With")
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
		if r.Method == "OPTIONS" {
			w.WriteHeader(http.StatusOK)
			return
		}
		next.ServeHTTP(w, r)
	})
}

func getEnv(key, fallback string) string {
	if v := os.Getenv(key); v != "" {
		return v
	}
	return fallback
}
