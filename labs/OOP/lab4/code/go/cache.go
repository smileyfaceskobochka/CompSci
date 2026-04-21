package main

import (
	"strings"
	"sync"
	"time"
)

const cacheTTL = 5 * time.Minute

type cacheEntry struct {
	data      []byte
	expiresAt time.Time
}

type Cache struct {
	mu      sync.RWMutex
	entries map[string]cacheEntry
}

func NewCache() *Cache {
	c := &Cache{entries: make(map[string]cacheEntry)}
	go func() {
		for range time.Tick(time.Minute) {
			c.evict()
		}
	}()
	return c
}

func (c *Cache) Get(key string) ([]byte, bool) {
	c.mu.RLock()
	defer c.mu.RUnlock()
	e, ok := c.entries[key]
	if !ok || time.Now().After(e.expiresAt) {
		return nil, false
	}
	return e.data, true
}

func (c *Cache) Set(key string, data []byte) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.entries[key] = cacheEntry{data: data, expiresAt: time.Now().Add(cacheTTL)}
}

func (c *Cache) Delete(key string) {
	c.mu.Lock()
	defer c.mu.Unlock()
	delete(c.entries, key)
}

func (c *Cache) InvalidatePrefix(prefix string) {
	c.mu.Lock()
	defer c.mu.Unlock()
	for k := range c.entries {
		if strings.HasPrefix(k, prefix) {
			delete(c.entries, k)
		}
	}
}

func (c *Cache) evict() {
	c.mu.Lock()
	defer c.mu.Unlock()
	now := time.Now()
	for k, e := range c.entries {
		if now.After(e.expiresAt) {
			delete(c.entries, k)
		}
	}
}

var cache = NewCache()
