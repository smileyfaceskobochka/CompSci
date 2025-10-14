#include "ui_game.h"
#include "../ui/components.h"
#include "../user_interface.h"
#include <math.h>
#include <raygui.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

void game_ui_init(GameUI *ui) {
  // Calculate centered board area above the UI controls
  float screenWidth = GetScreenWidth();
  float screenHeight = GetScreenHeight();
  float boardWidth = screenWidth * 0.8f; // Use 80% of screen width
  float boardHeight = screenHeight - UI_BOARD_TOP_MARGIN - UI_CONTROLS_HEIGHT -
                      20; // Leave margin above controls
  float boardX = (screenWidth - boardWidth) / 2.0f; // Center horizontally
  float boardY = UI_BOARD_TOP_MARGIN;               // Start below topbar

  ui->boardArea = (Rectangle){boardX, boardY, boardWidth, boardHeight};

  // Load shader references
  ui->emergencyGlowShader = ui_get_emergency_shader();
  ui->ringAnimationShader = ui_get_ring_animation_shader();
}

static void draw_pegs(const GameUI *ui, const GameState *state,
                      const Options *options) {
  const ThemeColors *theme = options_get_current_theme();
  // Space pegs evenly with gaps
  float spacing = ui->boardArea.width / (NUM_PEGS + 1.0f);
  float pegWidth = spacing * 0.8f; // Pegs take 80% of spacing
  float pegBaseY = ui->boardArea.y + ui->boardArea.height - 10;

  for (int i = 0; i < NUM_PEGS; i++) {
    float x = ui->boardArea.x + (i + 1) * spacing;
    Color pegRodColor = theme->overlay0;

    // Check for generator highlights and set peg rod glow colors
    bool isGen1Active = (state->generatorTimer[0][i] > 0);
    bool isGen3Active = (state->generatorTimer[2][i] > 0);

    if (isGen1Active || isGen3Active) {
      // Pulse the whole peg rod
      float time = GetTime();
      float intensity = sinf(time * 12.0f) * 0.4f + 1.4f; // Pulsing intensity

      if (isGen1Active) {
        // Generator 1: Blue glow for addition
        pegRodColor.r = (unsigned char)fminf(50 * intensity, 255);
        pegRodColor.g = (unsigned char)fminf(100 * intensity, 255);
        pegRodColor.b = (unsigned char)fminf(255 * intensity, 255);
        pegRodColor.a = 255;
      } else if (isGen3Active) {
        // Generator 3: Red glow for removal
        pegRodColor.r = (unsigned char)fminf(255 * intensity, 255);
        pegRodColor.g = (unsigned char)fminf(50 * intensity, 255);
        pegRodColor.b = (unsigned char)fminf(50 * intensity, 255);
        pegRodColor.a = 255;
      }
    }

    // Draw the peg rod (line) with glow color if active
    DrawLineEx((Vector2){x, ui->boardArea.y + 20}, (Vector2){x, pegBaseY}, 4,
               pegRodColor);

    // Draw rings
    int rings = state->ringCounts[i];
    float ringHeight = (ui->boardArea.height - 60) / MAX_RINGS;
    for (int r = 0; r < rings; r++) {
      float ry = pegBaseY - (r + 1) * ringHeight;
      Color ringColor = options->pegColors[i];

      // Calculate ring animation scale (for newly added rings)
      float animationScale = 1.0f;
      if (state->ringAnimationTimers[r][i] > 0) {
        float animationProgress = 1.0f - (state->ringAnimationTimers[r][i] /
                                          0.3f); // 0-1 over 0.3 seconds
        // Smooth scale from 0.2 to 1.0 using ease-out
        animationScale =
            0.2f + (0.8f * (1.0f - powf(1.0f - animationProgress, 3.0f)));
      }

      // Apply generator-specific glow to rings
      if (isGen1Active || isGen3Active) {
        float time = GetTime();
        float ringIntensity = sinf(time * 15.0f) * 0.3f + 1.3f; // Ring pulsing

        ringColor.r = (unsigned char)fminf(ringColor.r * ringIntensity, 255);
        ringColor.g = (unsigned char)fminf(ringColor.g * ringIntensity, 255);
        ringColor.b = (unsigned char)fminf(ringColor.b * ringIntensity, 255);
      }

      // Calculate animated ring dimensions
      float animatedWidth = pegWidth * 0.8f * animationScale;
      float animatedHeight = (ringHeight - 2) * animationScale;
      float animatedX = x - (animatedWidth * 0.5f);
      float animatedY =
          ry + ((ringHeight - 2) * (1.0f - animationScale) * 0.5f);

      DrawRectangleV((Vector2){animatedX, animatedY},
                     (Vector2){animatedWidth, animatedHeight}, ringColor);
    }
  }
}

void game_ui_draw(GameUI *ui, GameState *state, Options *options) {
  const ThemeColors *theme = options_get_current_theme();

  // Calculate screen shake offset
  Vector2 shakeOffset = {0, 0};
  if (state->shakeTimer > 0) {
    float time = GetTime();
    float intensity =
        state->shakeTimer / 0.3f; // Normalize to 0-1 over 0.3 seconds
    shakeOffset.x = sinf(time * 50.0f) * 8.0f * intensity; // Horizontal shake
    shakeOffset.y = cosf(time * 45.0f) * 6.0f * intensity; // Vertical shake
  }

  // Bottom controls
  float controlsY = GetScreenHeight() - 50;
  Rectangle controls = (Rectangle){20, controlsY, GetScreenWidth() - 40.0f, 40};

  // Emergency chance controls
  GuiLabel((Rectangle){controls.x, controls.y + 5, 130, controls.height},
           "Вероятность\nаварий:");
  float chance = (float)state->emergencyChance; // Use game state value

  // Emergency glow effect when Generator 2 succeeds
  Rectangle sliderRect =
      (Rectangle){controls.x + 120, controls.y, 150, controls.height};

  // Tick speed controls
  GuiLabel((Rectangle){controls.x + 330, controls.y + 5, 130, controls.height},
           "Скорость\nтика:");
  float tickSpeed = (float)options->tickSpeed;
  Rectangle tickSliderRect =
      (Rectangle){controls.x + 420, controls.y, 150, controls.height};
  if (state->emergencySuccessTimer > 0 && ui->emergencyGlowShader.id != 0) {
    // Begin shader mode for emergency glow effect
    BeginShaderMode(ui->emergencyGlowShader);

    // Set shader uniforms
    float time = GetTime();
    float intensity = (state->emergencySuccessTimer / 0.6f) *
                      (0.3f + 0.7f * sinf(time * 15.0f));
    SetShaderValue(ui->emergencyGlowShader,
                   GetShaderLocation(ui->emergencyGlowShader, "time"), &time,
                   SHADER_UNIFORM_FLOAT);
    SetShaderValue(ui->emergencyGlowShader,
                   GetShaderLocation(ui->emergencyGlowShader, "intensity"),
                   &intensity, SHADER_UNIFORM_FLOAT);

    // Draw the slider with shader effect
    DrawRectangleRec(sliderRect,
                     (Color){200, 50, 50, 120}); // Semi-transparent red base

    EndShaderMode();
  }

  // Draw additional glow layers for extra visual impact
  if (state->emergencySuccessTimer > 0) {
    float time = GetTime();
    for (int i = 0; i < 3; i++) {
      float alpha =
          (0.8f / (i + 1)) * (0.3f + 0.7f * sinf(time * 15.0f - i * 0.5f));
      Color glowColor = (Color){theme->red.r, theme->red.g, theme->red.b,
                                (unsigned char)(alpha * 100)};
      Rectangle glowRect = {sliderRect.x - 5 - i * 3, sliderRect.y - 5 - i * 3,
                            sliderRect.width + 10 + i * 6,
                            sliderRect.height + 10 + i * 6};
      DrawRectangleRounded(glowRect, 0.2f, 6, glowColor);
    }
  }

  // Make slider glow red when emergency chance > 0
  int originalBgColor = GuiGetStyle(SLIDER, BASE_COLOR_NORMAL);
  if (chance > 0) {
    float intensity = 0.5f + 0.5f * sinf(GetTime() * 8.0f); // Pulsing
    Color glowColor = (Color){theme->red.r, theme->red.g, theme->red.b,
                              (unsigned char)(intensity * 255)};
    GuiSetStyle(SLIDER, BASE_COLOR_NORMAL, ColorToInt(glowColor));
  }

  GuiSliderBar(sliderRect, NULL, NULL, &chance, 0.0f, 100.0f);

  // Tick speed slider
  GuiSliderBar(tickSliderRect, NULL, NULL, &tickSpeed, 1.0f, 255.0f);

  // Restore original color
  GuiSetStyle(SLIDER, BASE_COLOR_NORMAL, originalBgColor);

  // Update game state with slider value - use game_set_emergency_chance
  game_set_emergency_chance(state, (int)chance);
  GuiLabel((Rectangle){controls.x + 280, controls.y, 80, controls.height},
           TextFormat("%d%%", (int)chance));

  // Update tick speed
  options->tickSpeed = (int)tickSpeed;
  GuiLabel((Rectangle){controls.x + 580, controls.y, 60, controls.height},
           TextFormat("%d", (int)tickSpeed));

  // Align buttons to right bottom corner
  float screenWidth = GetScreenWidth();
  float buttonWidth = 80;
  float buttonHeight = controls.height;
  float buttonSpacing = 10;
  float rightMargin = 20;

  float sbroscX = screenWidth - rightMargin - buttonWidth;
  float shagX = sbroscX - buttonWidth - buttonSpacing;
  float zapuskX = shagX - buttonWidth - buttonSpacing;

  if (GuiButton((Rectangle){zapuskX, controls.y, buttonWidth, buttonHeight},
                state->running ? "Пауза" : "Запуск")) {
    state->running = !state->running;
  }
  if (GuiButton((Rectangle){shagX, controls.y, buttonWidth, buttonHeight},
                "Шаг")) {
    game_step(state);
  }
  if (GuiButton((Rectangle){sbroscX, controls.y, buttonWidth, buttonHeight},
                "Сброс")) {
    game_reset(state);
    // Reset seed to 0 (random) when resetting game
    options->randomSeed = 0;
    game_set_seed(0);
  }

  // Set tick speed using new function
  game_set_tick_speed(state, options->tickSpeed);

  // Update game state using new function
  game_update(state, GetFrameTime());

  // Apply screen shake to the game board only
  if (state->shakeTimer > 0) {
    BeginMode2D((Camera2D){shakeOffset, (Vector2){0, 0}, 0.0f, 1.0f});
  }

  // Board - now occupies space from topbar to controls
  draw_pegs(ui, state, options);

  // End screen shake mode
  if (state->shakeTimer > 0) {
    EndMode2D();
  }

  // Update timers using new function
  game_update_timers(state, GetFrameTime());
}
