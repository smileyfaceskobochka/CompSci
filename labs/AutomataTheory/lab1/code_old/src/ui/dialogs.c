#include "dialogs.h"
#include "../options.h"
#include "../game/game.h"
#include "components.h"

#include <raygui.h>
#include <raylib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static Rectangle center_modal_rect(int w, int h) {
  int sw = GetScreenWidth();
  int sh = GetScreenHeight();
  return (Rectangle){(sw - w) / 2.0f, (sh - h) / 2.0f, (float)w, (float)h};
}

void dialogs_init(DialogsState *state, Options *opts) {
  state->showAboutProgram = false;
  state->showAboutAuthor = false;
  state->showOptions = false;
  state->showConfirmExit = false;
  state->showMessage = false;
  state->showGameCompleted = false;
  state->confirmCallback = NULL;
  state->aboutProgramScrollOffset = (Vector2){0, 0};
  state->optionsScrollOffset = (Vector2){0, 0};
  state->optionsRef = opts;
  snprintf(state->widthBuf, sizeof(state->widthBuf), "%d", opts->windowWidth);
  snprintf(state->heightBuf, sizeof(state->heightBuf), "%d",
           opts->windowHeight);
  snprintf(state->emergencyChanceBuf, sizeof(state->emergencyChanceBuf), "%d",
           opts->emergencyChance);
  snprintf(state->fontSizeBuf, sizeof(state->fontSizeBuf), "%d",
           opts->fontSize);
  snprintf(state->fontSpacingBuf, sizeof(state->fontSpacingBuf), "%d",
           opts->fontSpacing);
  snprintf(state->tickSpeedBuf, sizeof(state->tickSpeedBuf), "%d",
           opts->tickSpeed);
  snprintf(state->randomSeedBuf, sizeof(state->randomSeedBuf), "%u",
           opts->randomSeed);
  state->selectedTheme = opts->currentTheme;

  // Copy peg colors to temporary buffer
  for (int i = 0; i < NUM_PEGS; i++) {
    state->tempPegColors[i] = opts->pegColors[i];
  }
}

void dialogs_sync_emergency_chance(DialogsState *state,
                                   int currentEmergencyChance) {
  // Update the emergency chance buffer with current value from game
  snprintf(state->emergencyChanceBuf, sizeof(state->emergencyChanceBuf), "%d",
           currentEmergencyChance);
}

void dialogs_sync_window_size(DialogsState *state, int currentWidth,
                              int currentHeight) {
  // Update the window size buffers with current values
  snprintf(state->widthBuf, sizeof(state->widthBuf), "%d", currentWidth);
  snprintf(state->heightBuf, sizeof(state->heightBuf), "%d", currentHeight);
}

void dialogs_sync_tick_speed(DialogsState *state, int currentTickSpeed) {
  snprintf(state->tickSpeedBuf, sizeof(state->tickSpeedBuf), "%d",
           currentTickSpeed);
}

void dialogs_sync_seed(DialogsState *state, unsigned int currentSeed) {
  snprintf(state->randomSeedBuf, sizeof(state->randomSeedBuf), "%u",
           currentSeed);
}

void open_about_program(DialogsState *state) { state->showAboutProgram = true; }
void open_about_author(DialogsState *state) { state->showAboutAuthor = true; }
void open_options(DialogsState *state) { state->showOptions = true; }
void open_confirm_exit(DialogsState *state, void (*callback)(void)) {
  state->showConfirmExit = true;
  state->confirmCallback = callback;
}

void open_message(DialogsState *state, const char *message, Color color) {
  state->showMessage = true;
  snprintf(state->messageText, sizeof(state->messageText), "%s", message);
  state->messageColor = color;
}

void open_game_completed(DialogsState *state) { state->showGameCompleted = true; }

bool dialogs_any_open(const DialogsState *state) {
  return state->showAboutProgram || state->showAboutAuthor ||
         state->showOptions || state->showConfirmExit ||
         state->showMessage || state->showGameCompleted;
}

void dialogs_draw(DialogsState *state, int screenWidth, int screenHeight) {
  const ThemeColors *theme = options_get_current_theme();
  (void)screenWidth;
  (void)screenHeight;

  if (state->showAboutProgram) {
    Rectangle r = center_modal_rect(600, 400);
    GuiPanel(r, "О программе");

    // Scroll panel setup for about program
    Rectangle scrollView = (Rectangle){0, 0, 560, 700}; // Content area (shorter for button)
    Rectangle scrollPanel = (Rectangle){r.x + 10, r.y + 35, r.width - 20, r.height - 100};
    Rectangle viewRect = {0, 0, 0, 0};

    // Draw scroll panel
    GuiScrollPanel(scrollPanel, NULL, scrollView, &state->aboutProgramScrollOffset, &viewRect);

    // Begin scissor mode for content clipping
    BeginScissorMode(scrollPanel.x + 2, scrollPanel.y + 2, scrollPanel.width - 4, scrollPanel.height - 4);

    // Content starts at scroll offset
    float contentY = r.y + 45 + state->aboutProgramScrollOffset.y; // Add for correct direction

    ui_draw_text("Лабораторная работа 1", (Vector2){r.x + 20, contentY}, theme->text);
    ui_draw_text("Теория автоматов: Игра \"Пирамидки\"", (Vector2){r.x + 20, contentY + 20}, theme->subtext0);

    ui_draw_text("Описание игры:", (Vector2){r.x + 20, contentY + 50}, theme->blue);
    ui_draw_text("Игра моделирует работу трёх генераторов, которые\n"
                 "автоматически добавляют и удаляют кольца на колышках.\n"
                 "Цель - наблюдать за поведением системы и её\n"
                 "завершением при заполнении всех колышков.",
                 (Vector2){r.x + 25, contentY + 70}, theme->subtext0);

    ui_draw_text("Генераторы:", (Vector2){r.x + 20, contentY + 165}, theme->blue);
    ui_draw_text("- Генератор 1 (СИНИЙ): Добавляет кольца на случайные\n"
                 "  колышки с каждым тактом",
                 (Vector2){r.x + 25, contentY + 185}, theme->sapphire);
    ui_draw_text("- Генератор 2 (КРАСНЫЙ): С вероятностью, заданной\n"
                 "  слайдером, вызывает аварию",
                 (Vector2){r.x + 25, contentY + 230}, theme->red);
    ui_draw_text("- Генератор 3 (КРАСНЫЙ): При аварии удаляет по 3 кольца\n"
                 "  с каждого заполненного колышка",
                 (Vector2){r.x + 25, contentY + 275}, theme->maroon);

    ui_draw_text("Визуальные индикаторы:", (Vector2){r.x + 20, contentY + 325}, theme->blue);
    ui_draw_text("- Синие колышки: активность генератора 1\n"
                 "- Красный слайдер: успешная активация генератора 2\n"
                 "- Красные колышки: активность генератора 3\n"
                 "- Экранное трясение: визуальный эффект аварии",
                 (Vector2){r.x + 25, contentY + 345}, theme->subtext0);

    ui_draw_text("Управление:", (Vector2){r.x + 20, contentY + 435}, theme->blue);
    ui_draw_text("- ПРОБЕЛ: запуск/остановка симуляции\n"
                 "- R: сброс игры и генерация нового сида\n"
                 "- S: пошаговое выполнение (при остановке)\n"
                 "- Слайдер аварий: настройка вероятности генератора 2\n"
                 "- Слайдер скорости: настройка тактов в секунду",
                 (Vector2){r.x + 25, contentY + 455}, theme->subtext0);

    ui_draw_text("Настройки:", (Vector2){r.x + 20, contentY + 570}, theme->blue);
    ui_draw_text("- Размер окна и шрифта\n"
                 "- Цветовая тема (светлая/тёмная)\n"
                 "- Цвета колышков\n"
                 "- Сид для воспроизводимости результатов",
                 (Vector2){r.x + 25, contentY + 590}, theme->subtext0);

    // End scissor mode
    EndScissorMode();

    // Center the button
    float buttonWidth = 100;
    float buttonHeight = 35;
    float buttonX = r.x + (r.width - buttonWidth) / 2;
    float buttonY = r.y + r.height - 50;

    if (GuiButton((Rectangle){buttonX, buttonY, buttonWidth, buttonHeight},
                  "Закрыть")) {
      state->showAboutProgram = false;
    }
  }

  if (state->showAboutAuthor) {
    Rectangle r = center_modal_rect(420, 240);
    GuiPanel(r, "Об авторе");
    ui_draw_text("Автор: Александр Черкасов\nEmail: your@email",
                 (Vector2){r.x + 20, r.y + 50}, theme->text);

    // Center the button
    float buttonWidth = 100;
    float buttonHeight = 35;
    float buttonX = r.x + (r.width - buttonWidth) / 2;
    float buttonY = r.y + r.height - 50;

    if (GuiButton((Rectangle){buttonX, buttonY, buttonWidth, buttonHeight},
                  "Закрыть")) {
      state->showAboutAuthor = false;
    }
  }

  if (state->showOptions) {
    Rectangle r = center_modal_rect(700, 600);
    GuiPanel(r, "Настройки");

    // Calculate total content height needed (excluding buttons)
    float lineHeight = 35;
    float totalContentHeight = 0;

    // Window Size Section: header + 1 input line + spacing
    totalContentHeight += lineHeight + lineHeight + 10;

    // Game Settings Section: header + 3 input lines + spacing
    totalContentHeight += lineHeight + lineHeight * 3 + 10;

    // Font Settings Section: header + 2 input lines + spacing
    totalContentHeight += lineHeight + lineHeight * 2 + 10;

    // Theme Settings Section: header + 1 input line + spacing
    totalContentHeight += lineHeight + lineHeight + 10;

    // Peg Colors Section: header + 5 input lines + spacing
    totalContentHeight += lineHeight + lineHeight * 5 + 20;

    // Make sure content is taller than panel to enable scrolling
    float panelHeight = r.height - 100; // Leave space for buttons
    if (totalContentHeight < panelHeight + 50) {
      totalContentHeight = panelHeight + 50; // Ensure scrolling is possible
    }

    // Scroll panel setup (buttons will be outside)
    Rectangle scrollView = (Rectangle){0, 0, 650, totalContentHeight + 100};
    Rectangle scrollPanel = (Rectangle){r.x + 10, r.y + 35, r.width - 20, panelHeight};
    Rectangle viewRect = {0, 0, 0, 0};

    // Draw scroll panel
    GuiScrollPanel(scrollPanel, NULL, scrollView, &state->optionsScrollOffset, &viewRect);

    // Begin scissor mode for content clipping
    BeginScissorMode(scrollPanel.x + 2, scrollPanel.y + 2, scrollPanel.width - 4, scrollPanel.height - 4);

    // Content starts at scroll offset
    float yPos = r.y + 45 + state->optionsScrollOffset.y;
    float leftCol = r.x + 20;
    float rightCol = r.x + 350;

    // Window Size Section
    ui_draw_text("Настройки окна", (Vector2){leftCol, yPos}, theme->blue);
    yPos += lineHeight;

    ui_text_input_with_label((Rectangle){leftCol, yPos, 300, 30},
                             "Ширина:", state->widthBuf,
                             sizeof(state->widthBuf));
    ui_text_input_with_label((Rectangle){rightCol, yPos, 300, 30},
                             "Высота:", state->heightBuf,
                             sizeof(state->heightBuf));
    yPos += lineHeight + 10;

    // Game Settings Section
    ui_draw_text("Настройки игры", (Vector2){leftCol, yPos}, theme->blue);
    yPos += lineHeight;

    ui_text_input_with_label((Rectangle){leftCol, yPos, 300, 30},
                             "Вероятность\nаварий:", state->emergencyChanceBuf,
                             sizeof(state->emergencyChanceBuf));
    ui_text_input_with_label((Rectangle){rightCol, yPos, 300, 30},
                             "Скорость\nтика:", state->tickSpeedBuf,
                             sizeof(state->tickSpeedBuf));
    yPos += lineHeight;

    ui_text_input_with_label((Rectangle){leftCol, yPos + 5, 300, 30},
                             "Seed:", state->randomSeedBuf,
                             sizeof(state->randomSeedBuf));
    yPos += lineHeight + 10;

    // Font Settings Section
    ui_draw_text("Настройки шрифта", (Vector2){leftCol, yPos}, theme->blue);
    yPos += lineHeight;

    ui_text_input_with_label((Rectangle){leftCol, yPos, 300, 30},
                             "Размер\nшрифта:", state->fontSizeBuf,
                             sizeof(state->fontSizeBuf));
    ui_text_input_with_label((Rectangle){rightCol, yPos, 300, 30},
                             "Интервал\nшрифта:", state->fontSpacingBuf,
                             sizeof(state->fontSpacingBuf));
    yPos += lineHeight + 10;

    // Theme Settings Section
    ui_draw_text("Настройки темы", (Vector2){leftCol, yPos}, theme->blue);
    yPos += lineHeight;

    // Theme toggle button
    const char *themeText = (state->selectedTheme == THEME_DARK)
                                ? "Светлая тема"
                                : "Тёмная тема";
    if (GuiButton((Rectangle){leftCol, yPos, 250, 30}, themeText)) {
      // Toggle theme
      state->selectedTheme =
          (state->selectedTheme == THEME_DARK) ? THEME_LIGHT : THEME_DARK;

      // Update peg colors when theme changes
      ThemeType newTheme = (ThemeType)state->selectedTheme;
      options_apply_theme_to_pegs(state->tempPegColors, newTheme);
    }

    // Show current theme
    char currentThemeText[50];
    snprintf(currentThemeText, sizeof(currentThemeText), "Текущая: %s",
             (state->selectedTheme == THEME_DARK) ? "Тёмная"
                                                  : "Светлая");
    GuiLabel((Rectangle){leftCol + 260, yPos, 200, 30}, currentThemeText);

    yPos += lineHeight + 10;

    // Peg Colors Section
    ui_draw_text("Цвета колышек", (Vector2){leftCol, yPos}, theme->blue);
    yPos += lineHeight;

    // Draw color palette pickers in two columns with proper spacing
    for (int i = 0; i < NUM_PEGS; i++) {
      float xPos = (i % 2 == 0) ? leftCol : rightCol;
      if (i % 2 == 0 && i > 0)
        yPos += lineHeight + 35; // Add extra spacing to prevent overlap

      char pegLabel[32];
      snprintf(pegLabel, sizeof(pegLabel), "Колышек\n%d:", i + 1);
      ui_color_palette_picker_with_availability((Rectangle){xPos, yPos, 300, 60}, pegLabel,
                                               &state->tempPegColors[i], state->tempPegColors, i, NUM_PEGS);
    }

    // End scissor mode
    EndScissorMode();

    // Draw buttons outside scroll area
    float buttonY = r.y + r.height - 50;
    float buttonWidth = 120;
    float buttonHeight = 35;
    float buttonSpacing = 20;
    float totalButtonWidth = buttonWidth * 2 + buttonSpacing;
    float buttonStartX = r.x + (r.width - totalButtonWidth) / 2; // Center in dialog

    bool apply = GuiButton((Rectangle){buttonStartX, buttonY, buttonWidth, buttonHeight},
                          "Применить");
    bool close = GuiButton((Rectangle){buttonStartX + buttonWidth + buttonSpacing, buttonY, buttonWidth, buttonHeight},
                          "Закрыть");

    if (apply) {
      // Apply window size
      int w = atoi(state->widthBuf);
      int h = atoi(state->heightBuf);
      if (w > 100 && h > 100) {
        state->optionsRef->windowWidth = w;
        state->optionsRef->windowHeight = h;
        SetWindowSize(w, h);
      }

      // Apply emergency chance
      int emergency = atoi(state->emergencyChanceBuf);
      if (emergency >= 0 && emergency <= 100) {
        state->optionsRef->emergencyChance = emergency;
        // Note: The game state will be updated through the main UI slider
        // This ensures consistency between options and game state
      }

      // Apply tick speed
      int tickSpeed = atoi(state->tickSpeedBuf);
      if (tickSpeed >= 1 && tickSpeed <= 256) {
        state->optionsRef->tickSpeed = tickSpeed;
      }

      // Apply seed
      unsigned int seed = (unsigned int)atoi(state->randomSeedBuf);
      state->optionsRef->randomSeed = seed;
      game_set_seed(seed);

      // Apply font settings
      int fontSize = atoi(state->fontSizeBuf);
      int fontSpacing = atoi(state->fontSpacingBuf);
      if (fontSize > 0 && fontSize <= 100) {
        state->optionsRef->fontSize = fontSize;
      }
      if (fontSpacing >= 0 && fontSpacing <= 10) {
        state->optionsRef->fontSpacing = fontSpacing;
      }

      // Apply font configuration globally
      FontConfig fontConfig;
      fontConfig.fontSize = state->optionsRef->fontSize;
      fontConfig.fontSpacing = state->optionsRef->fontSpacing;
      ui_set_font_config(&fontConfig);

      // Update GUI font sizes - two different sizes for different elements
      GuiSetStyle(DEFAULT, TEXT_SIZE,
                  state->optionsRef->fontSize); // Main GUI elements
      GuiSetStyle(LABEL, TEXT_SIZE,
                  state->optionsRef->fontSize - 2); // Labels slightly smaller

      // Apply theme
      state->optionsRef->currentTheme = (ThemeType)state->selectedTheme;
      options_set_theme(state->optionsRef->currentTheme);

      // Update GUI colors to match new theme
      const ThemeColors *theme = options_get_current_theme();
      if (theme) {
        GuiSetStyle(DEFAULT, BACKGROUND_COLOR, ColorToInt(theme->base));
        GuiSetStyle(DEFAULT, BASE_COLOR_NORMAL, ColorToInt(theme->surface));
        GuiSetStyle(DEFAULT, BASE_COLOR_FOCUSED, ColorToInt(theme->surface1));
        GuiSetStyle(DEFAULT, BASE_COLOR_PRESSED, ColorToInt(theme->surface1));
        GuiSetStyle(DEFAULT, BORDER_COLOR_NORMAL, ColorToInt(theme->overlay0));
        GuiSetStyle(DEFAULT, TEXT_COLOR_NORMAL, ColorToInt(theme->text));
        GuiSetStyle(DEFAULT, TEXT_COLOR_FOCUSED, ColorToInt(theme->text));
        GuiSetStyle(DEFAULT, TEXT_COLOR_PRESSED, ColorToInt(theme->text));
      }

      // Apply peg colors
      if (options_colors_unique(state->tempPegColors)) {
        for (int i = 0; i < NUM_PEGS; i++) {
          state->optionsRef->pegColors[i] = state->tempPegColors[i];
        }
      } else {
        // Reset to theme colors if duplicates found
        options_apply_theme_to_pegs(state->optionsRef->pegColors,
                                    (ThemeType)state->optionsRef->currentTheme);
      }
    }

    if (close) {
      state->showOptions = false;
    }
  }

  if (state->showConfirmExit) {
    Rectangle r = center_modal_rect(350, 175);
    GuiPanel(r, "Подтвердить выход");
    ui_draw_text("Вы уверены, что хотите выйти?", (Vector2){r.x + 20, r.y + 50},
                 theme->text);
    ui_draw_text("Весь несохранённый прогресс\nбудет потерян.",
                 (Vector2){r.x + 20, r.y + 70}, theme->subtext0);

    // Center the buttons
    float buttonWidth = 80;
    float buttonHeight = 35;
    float buttonSpacing = 20;
    float totalButtonWidth = buttonWidth * 2 + buttonSpacing;
    float buttonStartX = r.x + (r.width - totalButtonWidth) / 2;
    float buttonY = r.y + r.height - 50;

    if (GuiButton((Rectangle){buttonStartX, buttonY, buttonWidth, buttonHeight},
                  "Да")) {
      if (state->confirmCallback) {
        state->confirmCallback();
      }
      state->showConfirmExit = false;
    }
    if (GuiButton((Rectangle){buttonStartX + buttonWidth + buttonSpacing, buttonY, buttonWidth, buttonHeight},
                  "Нет")) {
      state->showConfirmExit = false;
    }
  }

  if (state->showMessage) {
    Rectangle r = center_modal_rect(350, 150);
    GuiPanel(r, "Сообщение");
    ui_draw_text("Файл настроек:", (Vector2){r.x + 20, r.y + 50},
                 theme->subtext0);
    ui_draw_text(state->messageText, (Vector2){r.x + 20, r.y + 70},
                 state->messageColor);

    // Center the button
    float buttonWidth = 80;
    float buttonHeight = 35;
    float buttonX = r.x + (r.width - buttonWidth) / 2;
    float buttonY = r.y + r.height - 50;

    if (GuiButton((Rectangle){buttonX, buttonY, buttonWidth, buttonHeight},
                  "ОК")) {
      state->showMessage = false;
    }
  }

  if (state->showGameCompleted) {
    Rectangle r = center_modal_rect(400, 200);
    GuiPanel(r, "Поздравляем!");
    ui_draw_text("Все колышки заполнены!", (Vector2){r.x + 20, r.y + 60}, theme->text);
    ui_draw_text("Игра завершена успешно.", (Vector2){r.x + 20, r.y + 90}, theme->subtext0);

    // Center the button
    float buttonWidth = 80;
    float buttonHeight = 35;
    float buttonX = r.x + (r.width - buttonWidth) / 2;
    float buttonY = r.y + r.height - 50;

    if (GuiButton((Rectangle){buttonX, buttonY, buttonWidth, buttonHeight},
                  "ОК")) {
      state->showGameCompleted = false;
    }
  }
}
