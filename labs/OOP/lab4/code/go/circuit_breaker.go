package main

import (
	"errors"
	"sync"
	"time"
)

// Circuit Breaker States
type CircuitState int

const (
	StateClosed   CircuitState = iota // Normal — requests pass through
	StateOpen                         // Tripped — requests fail immediately
	StateHalfOpen                     // Testing — one request allowed to probe
)

func (s CircuitState) String() string {
	switch s {
	case StateClosed:
		return "closed"
	case StateOpen:
		return "open"
	case StateHalfOpen:
		return "half-open"
	default:
		return "unknown"
	}
}

// ErrCircuitOpen is returned when the circuit breaker is open.
var ErrCircuitOpen = errors.New("circuit breaker is open: Python backend unavailable")

// CircuitBreaker implements the Circuit Breaker pattern for fault tolerance
// in distributed systems. It prevents cascading failures by short-circuiting
// requests to an unhealthy downstream service.
//
// State transitions:
//
//	Closed  → Open      (after `threshold` consecutive failures)
//	Open    → HalfOpen  (after `timeout` elapses)
//	HalfOpen → Closed   (on success)
//	HalfOpen → Open     (on failure)
type CircuitBreaker struct {
	mu               sync.Mutex
	state            CircuitState
	failureCount     int
	successCount     int
	threshold        int           // Consecutive failures to trip
	timeout          time.Duration // Duration to stay open before probing
	lastFailureTime  time.Time
	lastStateChange  time.Time
}

// NewCircuitBreaker creates a new CircuitBreaker with the given threshold
// and timeout. The breaker starts in Closed state.
func NewCircuitBreaker(threshold int, timeout time.Duration) *CircuitBreaker {
	return &CircuitBreaker{
		state:           StateClosed,
		threshold:       threshold,
		timeout:         timeout,
		lastStateChange: time.Now(),
	}
}

// Allow checks whether a request is allowed to proceed.
// Returns true if the circuit is Closed or if enough time has passed
// in the Open state to transition to HalfOpen for a probe.
func (cb *CircuitBreaker) Allow() bool {
	cb.mu.Lock()
	defer cb.mu.Unlock()

	switch cb.state {
	case StateClosed:
		return true
	case StateOpen:
		// Check if timeout has elapsed — transition to HalfOpen
		if time.Since(cb.lastFailureTime) > cb.timeout {
			cb.state = StateHalfOpen
			cb.lastStateChange = time.Now()
			return true
		}
		return false
	case StateHalfOpen:
		// Only allow one probe request at a time
		return true
	default:
		return false
	}
}

// RecordSuccess records a successful request. In HalfOpen state,
// this closes the circuit, restoring normal operation.
func (cb *CircuitBreaker) RecordSuccess() {
	cb.mu.Lock()
	defer cb.mu.Unlock()

	cb.successCount++

	switch cb.state {
	case StateHalfOpen:
		// Probe succeeded — circuit recovers
		cb.state = StateClosed
		cb.failureCount = 0
		cb.lastStateChange = time.Now()
	case StateClosed:
		cb.failureCount = 0
	}
}

// RecordFailure records a failed request. If the failure count exceeds
// the threshold, the circuit opens.
func (cb *CircuitBreaker) RecordFailure() {
	cb.mu.Lock()
	defer cb.mu.Unlock()

	cb.failureCount++
	cb.lastFailureTime = time.Now()

	switch cb.state {
	case StateClosed:
		if cb.failureCount >= cb.threshold {
			cb.state = StateOpen
			cb.lastStateChange = time.Now()
		}
	case StateHalfOpen:
		// Probe failed — back to Open
		cb.state = StateOpen
		cb.lastStateChange = time.Now()
	}
}

// State returns the current state of the circuit breaker.
func (cb *CircuitBreaker) State() CircuitState {
	cb.mu.Lock()
	defer cb.mu.Unlock()
	return cb.state
}

// Stats returns diagnostic information about the circuit breaker.
func (cb *CircuitBreaker) Stats() map[string]any {
	cb.mu.Lock()
	defer cb.mu.Unlock()
	return map[string]any{
		"state":            cb.state.String(),
		"failure_count":    cb.failureCount,
		"success_count":    cb.successCount,
		"threshold":        cb.threshold,
		"timeout_seconds":  cb.timeout.Seconds(),
		"last_state_change": cb.lastStateChange.Format(time.RFC3339),
	}
}

// Global circuit breaker instance for the Python backend
var breaker = NewCircuitBreaker(5, 30*time.Second)
