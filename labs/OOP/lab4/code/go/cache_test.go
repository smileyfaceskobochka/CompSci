package main

import (
	"testing"
	"time"
)

func TestCacheSetGet(t *testing.T) {
	c := NewCache()
	c.Set("key1", []byte("hello"))
	data, ok := c.Get("key1")
	if !ok {
		t.Fatal("expected key1 to be found")
	}
	if string(data) != "hello" {
		t.Errorf("expected 'hello', got '%s'", string(data))
	}
}

func TestCacheMiss(t *testing.T) {
	c := NewCache()
	_, ok := c.Get("nonexistent")
	if ok {
		t.Error("expected miss for nonexistent key")
	}
}

func TestCacheDelete(t *testing.T) {
	c := NewCache()
	c.Set("key1", []byte("data"))
	c.Delete("key1")
	_, ok := c.Get("key1")
	if ok {
		t.Error("expected key1 to be deleted")
	}
}

func TestCacheInvalidatePrefix(t *testing.T) {
	c := NewCache()
	c.Set("/api/teams", []byte("t1"))
	c.Set("/api/drivers", []byte("d1"))
	c.Set("/other", []byte("o1"))

	c.InvalidatePrefix("/api/")

	if _, ok := c.Get("/api/teams"); ok {
		t.Error("expected /api/teams to be invalidated")
	}
	if _, ok := c.Get("/api/drivers"); ok {
		t.Error("expected /api/drivers to be invalidated")
	}
	if _, ok := c.Get("/other"); !ok {
		t.Error("expected /other to still exist")
	}
}

func TestCacheExpiry(t *testing.T) {
	c := &Cache{entries: make(map[string]cacheEntry)}
	c.Set("key1", []byte("data"))
	// Set expiration in the past
	c.mu.Lock()
	c.entries["key1"] = cacheEntry{data: []byte("data"), expiresAt: time.Now().Add(-1 * time.Second)}
	c.mu.Unlock()

	_, ok := c.Get("key1")
	if ok {
		t.Error("expected expired key to not be found")
	}
}
