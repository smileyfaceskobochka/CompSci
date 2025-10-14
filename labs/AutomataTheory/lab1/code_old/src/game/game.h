#pragma once

#include <stdbool.h>
#include <raylib.h>

#define NUM_PEGS 10
#define MAX_RINGS 12

typedef struct {
  int ringCounts[NUM_PEGS];
  Color pegColors[NUM_PEGS];
  int emergencyChance; // 0..100 percent
  bool running;
  bool finished;
  float stepAccumulatorSec;
  float stepIntervalSec;
  float generatorTimer[3][NUM_PEGS]; // Generator-specific highlight timers [gen][peg]
  float emergencySuccessTimer; // Glow timer for emergency success
  // Animation state
  float shakeTimer; // Screen shake timer for emergencies
  float ringAnimationTimers[MAX_RINGS][NUM_PEGS]; // Animation timers for individual rings
} GameState;

void game_init(GameState *state);
void game_reset(GameState *state);
void game_step(GameState *state);
bool game_all_full(const GameState *state);

void game_set_emergency_chance(GameState *state, int chance);
void game_set_tick_speed(GameState *state, int ticksPerSecond);
void game_set_seed(unsigned int seed);
void game_update_timers(GameState *state, float deltaTime);
void game_update(GameState *state, float deltaTime);
