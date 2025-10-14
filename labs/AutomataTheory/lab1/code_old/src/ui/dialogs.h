#pragma once

#include <stdbool.h>

#include "../options.h"

typedef struct {
  bool showAboutProgram;
  bool showAboutAuthor;
  bool showOptions;
  bool showConfirmExit;
  bool showMessage;
  bool showGameCompleted;

  // Confirmation dialog callbacks
  void (*confirmCallback)(void);

  // Message dialog
  char messageText[256];
  Color messageColor;

  // Scroll offsets for dialogs
  Vector2 aboutProgramScrollOffset;
  Vector2 optionsScrollOffset;

  // Options editing buffers
  Options *optionsRef;
  char widthBuf[16];
  char heightBuf[16];
  char emergencyChanceBuf[16];
  char fontSizeBuf[16];
  char fontSpacingBuf[16];
  char tickSpeedBuf[16];
  char randomSeedBuf[16];
  int selectedTheme;  // 0 = Dark, 1 = Light

  // Temporary colors for editing
  Color tempPegColors[NUM_PEGS];
} DialogsState;

void dialogs_init(DialogsState *state, Options *opts);

// Sync emergency chance from game to options dialog
void dialogs_sync_emergency_chance(DialogsState *state, int currentEmergencyChance);

// Sync current window size to options dialog
void dialogs_sync_window_size(DialogsState *state, int currentWidth, int currentHeight);

// Sync tick speed from options
void dialogs_sync_tick_speed(DialogsState *state, int currentTickSpeed);

// Sync seed from options
void dialogs_sync_seed(DialogsState *state, unsigned int currentSeed);

// Call every frame to draw dialogs when open
void dialogs_draw(DialogsState *state, int screenWidth, int screenHeight);

// Triggers
void open_about_program(DialogsState *state);
void open_about_author(DialogsState *state);
void open_options(DialogsState *state);
void open_confirm_exit(DialogsState *state, void (*callback)(void));
void open_message(DialogsState *state, const char *message, Color color);
void open_game_completed(DialogsState *state);

// Check if any dialog is currently open
bool dialogs_any_open(const DialogsState *state);
