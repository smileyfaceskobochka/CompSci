package main

import (
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestResponseCapture_WriteHeader(t *testing.T) {
	rec := httptest.NewRecorder()
	rc := &responseCapture{ResponseWriter: rec, statusCode: http.StatusOK}

	rc.WriteHeader(http.StatusCreated)
	if rc.statusCode != http.StatusCreated {
		t.Errorf("expected statusCode to be %d, got %d", http.StatusCreated, rc.statusCode)
	}
	if rec.Code != http.StatusCreated {
		t.Errorf("expected recorder code to be %d, got %d", http.StatusCreated, rec.Code)
	}
}

func TestLoggingMiddleware(t *testing.T) {
	handler := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusAccepted)
	})

	loggedHandler := LoggingMiddleware(handler)

	req := httptest.NewRequest("GET", "/test-path", nil)
	rec := httptest.NewRecorder()

	loggedHandler.ServeHTTP(rec, req)

	if rec.Code != http.StatusAccepted {
		t.Errorf("expected status %d, got %d", http.StatusAccepted, rec.Code)
	}
}

func TestTimingMiddleware(t *testing.T) {
	handler := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
	})

	timedHandler := TimingMiddleware(handler)

	req := httptest.NewRequest("GET", "/test-path", nil)
	rec := httptest.NewRecorder()

	timedHandler.ServeHTTP(rec, req)

	header := rec.Header().Get("X-Response-Time")
	if header == "" {
		t.Error("expected X-Response-Time header to be set, but it was empty")
	}
}

func TestRecoveryMiddleware_NoPanic(t *testing.T) {
	handler := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write([]byte("ok"))
	})

	recoveredHandler := RecoveryMiddleware(handler)

	req := httptest.NewRequest("GET", "/test-path", nil)
	rec := httptest.NewRecorder()

	recoveredHandler.ServeHTTP(rec, req)

	if rec.Code != http.StatusOK {
		t.Errorf("expected status 200, got %d", rec.Code)
	}
	if rec.Body.String() != "ok" {
		t.Errorf("expected body 'ok', got %q", rec.Body.String())
	}
}

func TestRecoveryMiddleware_Panic(t *testing.T) {
	handler := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		panic("something went terribly wrong")
	})

	recoveredHandler := RecoveryMiddleware(handler)

	req := httptest.NewRequest("GET", "/test-path", nil)
	rec := httptest.NewRecorder()

	recoveredHandler.ServeHTTP(rec, req)

	if rec.Code != http.StatusInternalServerError {
		t.Errorf("expected status 500, got %d", rec.Code)
	}
	// http.Error adds a newline to the output
	if rec.Body.String() != "Internal Server Error\n" {
		t.Errorf("expected body 'Internal Server Error\\n', got %q", rec.Body.String())
	}
}
