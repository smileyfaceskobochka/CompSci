/**
 * @file main.c
 * @brief Главная точка входа приложения "Лабораторная работа №1"
 *
 * Этот файл содержит основную логику приложения:
 * - Инициализация всех подсистем (графика, UI, игра)
 * - Главный игровой цикл с обработкой событий
 * - Управление диалогами и пользовательским интерфейсом
 * - Обработка файловых операций (сохранение/загрузка настроек)
 * - Горячие клавиши и управление игрой
 */

#include <raygui.h>
#include <raylib.h>
#include <stdio.h>

#include "game/game.h"
#include "game/ui_game.h"
#include "options.h"
#include "sys/file_dialog.h"
#include "ui/components.h"
#include "ui/dialogs.h"
#include "user_interface.h"

// Callback функции для подтверждений
void exit_application(void);

/**
 * @brief Главная функция приложения
 *
 * Выполняет полную инициализацию приложения:
 * 1. Настройка параметров приложения
 * 2. Инициализация графического окна
 * 3. Настройка подсистем UI (шрифты, темы, шейдеры)
 * 4. Инициализация компонентов (UI, диалоги, игра)
 * 5. Запуск главного игрового цикла
 *
 * @return Код выхода (0 при успешном завершении)
 */
int main(void) {
  // Настройка параметров приложения
  Options options;
  options_set_defaults(&options);

  // Инициализация графического окна
  InitWindow(options.windowWidth, options.windowHeight,
             "Lab1 - Automata Theory: Pyramids Peg Game");
  SetWindowState(FLAG_WINDOW_RESIZABLE);
  SetTargetFPS(60);

  // Инициализация подсистем пользовательского интерфейса
  ui_init_font(&options);
  ui_setup_theme(&options);
  ui_init_shaders();

  // Инициализация компонентов приложения
  UIState ui;
  ui_init(&ui);

  DialogsState dialogs;
  dialogs_init(&dialogs, &options);

  GameState game;
  game_init(&game);

  GameUI gameui;
  game_ui_init(&gameui);

  // Главный игровой цикл
  while (!WindowShouldClose()) {
    BeginDrawing();

    // Используем цвет фона из текущей темы
    const ThemeColors *theme = options_get_current_theme();
    if (theme) {
      ClearBackground(theme->base);
    } else {
      ClearBackground(RAYWHITE);
    }

    ui_update(&ui); // отрисовка виджетов верхней панели
    ui_handle(&ui); // обработка действий пользователя

    // Обработка открытия файла настроек
    if (ui.actionOpen) {
      char path[512];
      if (open_file_dialog(path, sizeof(path), "Открыть options.yaml",
                           "YAML файлы", "*.yml *.yaml")) {
        if (options_load_from_file(path, &options)) {
          const ThemeColors *theme = options_get_current_theme();
          open_message(&dialogs, "Настройки успешно загружены из файла",
                       theme->green);
          SetWindowSize(options.windowWidth, options.windowHeight);
          // Синхронизируем состояние игры с загруженными настройками
          game_set_emergency_chance(&game, options.emergencyChance);
          game_set_tick_speed(&game, options.tickSpeed);
        } else {
          const ThemeColors *theme = options_get_current_theme();
          open_message(&dialogs, "Ошибка загрузки настроек из файла",
                       theme->red);
        }
      }
      ui.actionOpen = false;
    }
    // Обработка сохранения файла настроек
    if (ui.actionSave) {
      // Обновляем настройки текущими значениями окна и игры
      options.windowWidth = GetScreenWidth();
      options.windowHeight = GetScreenHeight();
      options.emergencyChance = game.emergencyChance;

      char path[512];
      if (save_file_dialog(path, sizeof(path), "Сохранить options.yaml",
                           "options.yaml", "YAML файлы", "*.yml *.yaml")) {
        if (options_save_to_file(path, &options)) {
          const ThemeColors *theme = options_get_current_theme();
          open_message(&dialogs, "Настройки успешно сохранены в файл",
                       theme->green);
        } else {
          const ThemeColors *theme = options_get_current_theme();
          open_message(&dialogs, "Ошибка сохранения настроек в файл",
                       theme->red);
        }
      }
      ui.actionSave = false;
    }
    // Обработка действий выхода и информации
    if (ui.actionExit) {
      open_confirm_exit(&dialogs, exit_application);
      ui.actionExit = false;
    }
    if (ui.actionAboutProgram) {
      open_about_program(&dialogs);
      ui.actionAboutProgram = false;
    }
    if (ui.actionAboutAuthor) {
      open_about_author(&dialogs);
      ui.actionAboutAuthor = false;
    }

    // Диалог настроек через меню
    if (ui.actionOptions) {
      // Синхронизируем текущие значения игры с диалогом настроек
      dialogs_sync_emergency_chance(&dialogs, game.emergencyChance);
      dialogs_sync_window_size(&dialogs, GetScreenWidth(), GetScreenHeight());
      dialogs_sync_tick_speed(&dialogs, options.tickSpeed);
      dialogs_sync_seed(&dialogs, options.randomSeed);
      open_options(&dialogs);
      ui.actionOptions = false;
    }

    // Отрисовываем UI игры только когда нет открытых диалогов
    if (!dialogs_any_open(&dialogs)) {
      game_ui_draw(&gameui, &game, &options);
    }

    // Проверяем завершение игры и показываем диалог
    static bool gameWasCompleted = false;
    if (game.finished && !gameWasCompleted) {
      // Игра только что завершилась
      open_game_completed(&dialogs);
      gameWasCompleted = true;
    } else if (!game.finished && gameWasCompleted) {
      // Игра была сброшена
      gameWasCompleted = false;
    }

    // Горячие клавиши (только когда нет открытых диалогов)
    if (!dialogs.showAboutProgram && !dialogs.showAboutAuthor &&
        !dialogs.showOptions && !dialogs.showConfirmExit &&
        !dialogs.showMessage && !dialogs.showGameCompleted) {

      // ПРОБЕЛ: запуск/остановка симуляции
      if (IsKeyPressed(KEY_SPACE)) {
        if (ui.actionOptions) {
          // Не interferируем с диалогом настроек
        } else {
          // Переключаем состояние выполнения игры
          if (!game.running) {
            game.running = true;
          } else {
            game.running = false;
          }
        }
      }

      // R: сброс игры
      if (IsKeyPressed(KEY_R)) {
        game_reset(&game);
        // Сбрасываем сид в 0 (случайный) при сбросе игры
        options.randomSeed = 0;
        game_set_seed(0);
      }

      // S: пошаговое выполнение
      if (IsKeyPressed(KEY_S)) {
        if (!game.running) {
          game_step(&game);
        }
      }
    }

    // Отрисовываем выпадающие меню верхней панели после игры, но перед диалогами
    ui_render_dropdowns(&ui);

    // Отрисовываем все диалоги
    dialogs_draw(&dialogs, GetScreenWidth(), GetScreenHeight());

    EndDrawing();
  }

  return 0;
}

/**
 * @brief Callback функция для выхода из приложения
 *
 * Вызывается при подтверждении выхода из приложения.
 * Закрывает графическое окно и завершает работу программы.
 */
void exit_application(void) { CloseWindow(); }
