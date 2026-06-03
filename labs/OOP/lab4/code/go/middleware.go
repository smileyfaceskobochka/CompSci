package main

import (
	"log"
	"net/http"
	"time"
)

// responseCapture wraps http.ResponseWriter to capture the status code
// for logging purposes without interfering with the response.
type responseCapture struct {
	http.ResponseWriter
	statusCode int
}

func (rc *responseCapture) WriteHeader(code int) {
	rc.statusCode = code
	rc.ResponseWriter.WriteHeader(code)
}

// LoggingMiddleware logs every incoming HTTP request with method, path,
// response status code, and processing latency. This provides observability
// into the BFF gateway's traffic patterns and performance.
func LoggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		start := time.Now()

		rc := &responseCapture{ResponseWriter: w, statusCode: http.StatusOK}
		next.ServeHTTP(rc, r)

		latency := time.Since(start)

		log.Printf("[%s] %s %s — %d (%s)",
			r.Method,
			r.URL.Path,
			r.URL.RawQuery,
			rc.statusCode,
			latency.Round(time.Microsecond),
		)
	})
}

// TimingMiddleware adds an X-Response-Time header to every response,
// indicating server-side processing time in milliseconds.
func TimingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		start := time.Now()
		next.ServeHTTP(w, r)
		elapsed := time.Since(start)
		w.Header().Set("X-Response-Time", elapsed.String())
	})
}

// RecoveryMiddleware catches panics in downstream handlers and converts
// them to HTTP 500 responses instead of crashing the entire server process.
// This is critical in distributed systems where one malformed request
// must not take down the gateway for all other clients.
func RecoveryMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			if err := recover(); err != nil {
				log.Printf("[PANIC RECOVERED] %s %s — %v", r.Method, r.URL.Path, err)
				http.Error(w, "Internal Server Error", http.StatusInternalServerError)
			}
		}()
		next.ServeHTTP(w, r)
	})
}
