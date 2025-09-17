#include "game.h"

#include <raylib.h>



void game_init(GameState *state) {
  for (int i = 0; i < NUM_PEGS; i++) {
    state->ringCounts[i] = 0;
    state->pegColors[i] =
        ColorFromHSV((float)i / NUM_PEGS * 360.0f, 0.6f, 0.95f);
    for (int gen = 0; gen < 3; gen++) {
      state->generatorTimer[gen][i] = 0.0f;
    }
    for (int r = 0; r < MAX_RINGS; r++) {
      state->ringAnimationTimers[r][i] = 0.0f; // No ring animations initially
    }
  }
  state->emergencyChance = 0;
  state->running = false; // Game starts stopped - user must click Start
  state->finished = false;
  state->stepAccumulatorSec = 0.0f;
  state->stepIntervalSec = 1.0f / 60.0f; // Default 60 ticks/second
  state->emergencySuccessTimer = 0.0f;
  state->shakeTimer = 0.0f;
}

void game_reset(GameState *state) {
  for (int i = 0; i < NUM_PEGS; i++) {
    state->ringCounts[i] = 0;
    for (int gen = 0; gen < 3; gen++) {
      state->generatorTimer[gen][i] = 0.0f;
    }
    for (int r = 0; r < MAX_RINGS; r++) {
      state->ringAnimationTimers[r][i] = 0.0f; // Reset ring animations
    }
  }
  // Keep current emergency chance and tick speed settings
  // Only reset game state, timers, and stop game
  state->running = false;
  state->finished = false;
  state->stepAccumulatorSec = 0.0f;
  state->emergencySuccessTimer = 0.0f;
  state->shakeTimer = 0.0f;
}

bool game_all_full(const GameState *state) {
  for (int i = 0; i < NUM_PEGS; i++)
    if (state->ringCounts[i] < MAX_RINGS)
      return false;
  return true;
}

void game_set_emergency_chance(GameState *state, int chance) {
  if (chance >= 0 && chance <= 100) {
    state->emergencyChance = chance;
  }
}

void game_set_tick_speed(GameState *state, int ticksPerSecond) {
  if (ticksPerSecond > 0 && ticksPerSecond <= 256) {
    state->stepIntervalSec = 1.0f / (float)ticksPerSecond;
  }
}

void game_set_seed(unsigned int seed) {
  if (seed != 0) {
    SetRandomSeed(seed);
  }
}

void game_update_timers(GameState *state, float deltaTime) {
  // Update generator highlight timers
  for (int gen = 0; gen < 3; gen++) {
    for (int i = 0; i < NUM_PEGS; i++) {
      if (state->generatorTimer[gen][i] > 0) {
        state->generatorTimer[gen][i] -= deltaTime;
        if (state->generatorTimer[gen][i] < 0) {
          state->generatorTimer[gen][i] = 0;
        }
      }
    }
  }

  // Update emergency success glow timer
  if (state->emergencySuccessTimer > 0) {
    state->emergencySuccessTimer -= deltaTime;
    if (state->emergencySuccessTimer < 0) {
      state->emergencySuccessTimer = 0;
    }
  }

  // Update ring animation timers
  for (int i = 0; i < NUM_PEGS; i++) {
    for (int r = 0; r < MAX_RINGS; r++) {
      if (state->ringAnimationTimers[r][i] > 0) {
        state->ringAnimationTimers[r][i] -= deltaTime;
        if (state->ringAnimationTimers[r][i] < 0) {
          state->ringAnimationTimers[r][i] = 0;
        }
      }
    }
  }

  // Update screen shake timer
  if (state->shakeTimer > 0) {
    state->shakeTimer -= deltaTime;
    if (state->shakeTimer < 0) {
      state->shakeTimer = 0;
    }
  }
}

void game_update(GameState *state, float deltaTime) {
  if (state->running) {
    state->stepAccumulatorSec += deltaTime;
    while (state->stepAccumulatorSec >= state->stepIntervalSec) {
      game_step(state);
      state->stepAccumulatorSec -= state->stepIntervalSec;
    }
  }
}

void game_step(GameState *state) {
  if (state->finished)
    return;

  // Generator #1: add ring to random peg
  int pegAdd = GetRandomValue(0, NUM_PEGS - 1);
  if (state->ringCounts[pegAdd] < MAX_RINGS) {
    int newRing = state->ringCounts[pegAdd];
    state->ringCounts[pegAdd]++;
    // Start ring animation for the new ring
    state->ringAnimationTimers[newRing][pegAdd] = 0.3f; // Animation duration
    // Highlight the peg for Generator 1 visual feedback
    state->generatorTimer[0][pegAdd] = 0.2f; // Blue glow for Gen 1
  }

  // Generator #2: emergency chance
  int roll = GetRandomValue(0, 99);
  if (roll < state->emergencyChance) {
    // Generator #2 succeeds - activate emergency glow and screen shake
    state->emergencySuccessTimer = 0.6f; // Longer glow for emergency success
    state->shakeTimer = 0.3f;            // Screen shake duration

    // Generator #3: pick a peg with >=3 rings
    int candidates[NUM_PEGS];
    int c = 0;
    for (int i = 0; i < NUM_PEGS; i++)
      if (state->ringCounts[i] >= 3)
        candidates[c++] = i;
    if (c > 0) {
      int pegRem = candidates[GetRandomValue(0, c - 1)];
      state->ringCounts[pegRem] -= 3;
      // Highlight the peg for Generator 3 visual feedback
      state->generatorTimer[2][pegRem] = 0.4f; // Red glow for Gen 3
      // Reset ring animation timers for removed rings
      for (int r = 0; r < 3; r++) {
        state->ringAnimationTimers[state->ringCounts[pegRem] + r][pegRem] =
            0.0f;
      }
    }
  }

  if (game_all_full(state)) {
    state->finished = true;
    state->running = false;
  }
}
