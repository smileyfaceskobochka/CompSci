package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
)

var pythonBase = getEnv("PYTHON_API_URL", "http://localhost:8000")

func apiGet(path string, out any) error {
	if cached, ok := cache.Get(path); ok {
		return json.Unmarshal(cached, out)
	}
	resp, err := http.Get(pythonBase + path)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)
	if resp.StatusCode >= 400 {
		return fmt.Errorf("python API error %d: %s", resp.StatusCode, body)
	}
	cache.Set(path, body)
	return json.Unmarshal(body, out)
}

func apiPost(path string, payload any) (*http.Response, error) {
	b, _ := json.Marshal(payload)
	resp, err := http.Post(pythonBase+path, "application/json", bytes.NewReader(b))
	if err == nil {
		cache.InvalidatePrefix("/api/")
	}
	return resp, err
}

func apiPut(path string, payload any) (*http.Response, error) {
	b, _ := json.Marshal(payload)
	req, _ := http.NewRequest(http.MethodPut, pythonBase+path, bytes.NewReader(b))
	req.Header.Set("Content-Type", "application/json")
	resp, err := http.DefaultClient.Do(req)
	if err == nil {
		cache.InvalidatePrefix("/api/")
	}
	return resp, err
}

func apiDelete(path string) error {
	req, _ := http.NewRequest(http.MethodDelete, pythonBase+path, nil)
	resp, err := http.DefaultClient.Do(req)
	if err == nil && resp != nil && resp.StatusCode < 400 {
		cache.InvalidatePrefix("/api/")
	}
	return err
}
