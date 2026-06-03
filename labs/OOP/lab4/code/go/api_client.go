package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"time"
)

var pythonBase = getEnv("PYTHON_API_URL", "http://localhost:8000")

// httpClient is a shared HTTP client with explicit timeouts.
// In distributed systems, unbounded timeouts cause thread starvation
// and cascading failures. A 10-second timeout ensures the BFF stays responsive.
var httpClient = &http.Client{
	Timeout: 10 * time.Second,
}

// apiGet performs a GET request to the Python backend with:
//  1. Circuit Breaker check — fast-fail if Python is known to be down
//  2. In-memory cache lookup — avoid redundant network calls
//  3. Timeout-bounded HTTP request via shared client
//  4. Cache population on success
func apiGet(path string, out any) error {
	if !breaker.Allow() {
		return ErrCircuitOpen
	}

	if cached, ok := cache.Get(path); ok {
		return json.Unmarshal(cached, out)
	}

	resp, err := httpClient.Get(pythonBase + path)
	if err != nil {
		breaker.RecordFailure()
		return fmt.Errorf("python backend unreachable: %w", err)
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)
	if resp.StatusCode >= 500 {
		breaker.RecordFailure()
		return fmt.Errorf("python API error %d: %s", resp.StatusCode, body)
	}
	if resp.StatusCode >= 400 {
		return fmt.Errorf("python API error %d: %s", resp.StatusCode, body)
	}

	breaker.RecordSuccess()
	cache.Set(path, body)
	return json.Unmarshal(body, out)
}

// apiPost sends a POST request to create a resource. Invalidates
// the entire API cache since the mutation affects list endpoints.
func apiPost(path string, payload any) (*http.Response, error) {
	if !breaker.Allow() {
		return nil, ErrCircuitOpen
	}

	b, _ := json.Marshal(payload)
	resp, err := httpClient.Post(pythonBase+path, "application/json", bytes.NewReader(b))
	if err != nil {
		breaker.RecordFailure()
		return nil, fmt.Errorf("python backend unreachable: %w", err)
	}

	if resp.StatusCode >= 500 {
		breaker.RecordFailure()
	} else {
		breaker.RecordSuccess()
	}

	cache.InvalidatePrefix("/api/")
	return resp, nil
}

// apiPut sends a PUT request to update a resource. Invalidates cache.
func apiPut(path string, payload any) (*http.Response, error) {
	if !breaker.Allow() {
		return nil, ErrCircuitOpen
	}

	b, _ := json.Marshal(payload)
	req, _ := http.NewRequest(http.MethodPut, pythonBase+path, bytes.NewReader(b))
	req.Header.Set("Content-Type", "application/json")
	resp, err := httpClient.Do(req)
	if err != nil {
		breaker.RecordFailure()
		return nil, fmt.Errorf("python backend unreachable: %w", err)
	}

	if resp.StatusCode >= 500 {
		breaker.RecordFailure()
	} else {
		breaker.RecordSuccess()
	}

	cache.InvalidatePrefix("/api/")
	return resp, nil
}

// apiDelete sends a DELETE request to remove a resource. Invalidates cache.
func apiDelete(path string) error {
	if !breaker.Allow() {
		return ErrCircuitOpen
	}

	req, _ := http.NewRequest(http.MethodDelete, pythonBase+path, nil)
	resp, err := httpClient.Do(req)
	if err != nil {
		breaker.RecordFailure()
		return fmt.Errorf("python backend unreachable: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode >= 500 {
		breaker.RecordFailure()
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("python API error %d: %s", resp.StatusCode, body)
	}
	if resp.StatusCode >= 400 {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("python API error %d: %s", resp.StatusCode, body)
	}

	breaker.RecordSuccess()
	cache.InvalidatePrefix("/api/")
	return nil
}
