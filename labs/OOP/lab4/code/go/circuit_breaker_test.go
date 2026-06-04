package main

import (
	"testing"
	"time"
)

func TestCircuitBreaker_StateString(t *testing.T) {
	if StateClosed.String() != "closed" {
		t.Errorf("expected closed, got %s", StateClosed.String())
	}
	if StateOpen.String() != "open" {
		t.Errorf("expected open, got %s", StateOpen.String())
	}
	if StateHalfOpen.String() != "half-open" {
		t.Errorf("expected half-open, got %s", StateHalfOpen.String())
	}
	if CircuitState(99).String() != "unknown" {
		t.Errorf("expected unknown, got %s", CircuitState(99).String())
	}
}

func TestCircuitBreaker_InitialState(t *testing.T) {
	cb := NewCircuitBreaker(3, 100*time.Millisecond)
	if cb.State() != StateClosed {
		t.Errorf("expected state Closed, got %v", cb.State())
	}
	if !cb.Allow() {
		t.Error("expected Allow() to be true in Closed state")
	}
}

func TestCircuitBreaker_TripToOpen(t *testing.T) {
	cb := NewCircuitBreaker(3, 100*time.Millisecond)

	// Record 2 failures (below threshold of 3)
	cb.RecordFailure()
	cb.RecordFailure()
	if cb.State() != StateClosed {
		t.Errorf("expected state Closed, got %v", cb.State())
	}
	if !cb.Allow() {
		t.Error("expected Allow() to be true")
	}

	// 3rd failure trips the breaker
	cb.RecordFailure()
	if cb.State() != StateOpen {
		t.Errorf("expected state Open, got %v", cb.State())
	}
	if cb.Allow() {
		t.Error("expected Allow() to be false in Open state")
	}
}

func TestCircuitBreaker_TimeoutToHalfOpen(t *testing.T) {
	cb := NewCircuitBreaker(2, 50*time.Millisecond)

	// Trip it
	cb.RecordFailure()
	cb.RecordFailure()
	if cb.State() != StateOpen {
		t.Errorf("expected state Open, got %v", cb.State())
	}

	// Immediately, it shouldn't allow requests
	if cb.Allow() {
		t.Error("expected Allow() to be false immediately after tripping")
	}

	// Wait for timeout to elapse
	time.Sleep(60 * time.Millisecond)

	// Allow() should now transition to HalfOpen and return true
	if !cb.Allow() {
		t.Error("expected Allow() to be true after timeout elapses")
	}
	if cb.State() != StateHalfOpen {
		t.Errorf("expected state HalfOpen, got %v", cb.State())
	}
}

func TestCircuitBreaker_HalfOpenSuccess(t *testing.T) {
	cb := NewCircuitBreaker(2, 50*time.Millisecond)

	// Trip it
	cb.RecordFailure()
	cb.RecordFailure()

	// Wait for timeout
	time.Sleep(60 * time.Millisecond)

	// Transition to HalfOpen
	if !cb.Allow() {
		t.Error("expected Allow() to return true after timeout")
	}

	// Record success -> state should go back to Closed
	cb.RecordSuccess()
	if cb.State() != StateClosed {
		t.Errorf("expected state Closed, got %v", cb.State())
	}

	// Success in Closed should reset failure count
	cb.RecordFailure()
	cb.RecordSuccess() // resets failure count
	cb.RecordFailure()
	if cb.State() != StateClosed {
		t.Errorf("expected state Closed after reset, got %v", cb.State())
	}
}

func TestCircuitBreaker_HalfOpenFailure(t *testing.T) {
	cb := NewCircuitBreaker(2, 50*time.Millisecond)

	// Trip it
	cb.RecordFailure()
	cb.RecordFailure()

	// Wait for timeout
	time.Sleep(60 * time.Millisecond)

	// Transition to HalfOpen
	if !cb.Allow() {
		t.Error("expected Allow() to return true after timeout")
	}

	// Record failure -> state should go back to Open
	cb.RecordFailure()
	if cb.State() != StateOpen {
		t.Errorf("expected state Open, got %v", cb.State())
	}
}

func TestCircuitBreaker_Stats(t *testing.T) {
	cb := NewCircuitBreaker(5, 30*time.Second)
	cb.RecordFailure()
	cb.RecordSuccess()

	stats := cb.Stats()
	if stats["state"] != "closed" {
		t.Errorf("expected closed state in stats, got %v", stats["state"])
	}
	if stats["failure_count"] != 0 { // got reset by RecordSuccess()
		t.Errorf("expected 0 failure count, got %v", stats["failure_count"])
	}
	if stats["success_count"] != 1 {
		t.Errorf("expected 1 success count, got %v", stats["success_count"])
	}
	if stats["threshold"] != 5 {
		t.Errorf("expected threshold 5, got %v", stats["threshold"])
	}
}
