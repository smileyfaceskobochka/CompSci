#pragma once

#include "game.h"
#include "../options.h"
#include <raylib.h>

typedef struct {
  Rectangle boardArea;

  // Shader effects
  Shader emergencyGlowShader;
  Shader ringAnimationShader;
} GameUI;

void game_ui_init(GameUI *ui);
void game_ui_draw(GameUI *ui, GameState *state, Options *options);
