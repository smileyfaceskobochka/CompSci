/**
 * @file components.c
 * @brief Реализация компонентов пользовательского интерфейса
 *
 * Этот файл содержит реализации различных UI компонентов:
 * - Управление конфигурацией шрифтов
 * - Кнопки с пользовательскими стилями
 * - Слайдеры с метками
 * - Выбор цвета с палитрой
 * - Текстовые поля ввода
 * - Панели с заголовками
 */

#include "components.h"
#include "../options.h"
#include <stdio.h>
#include <string.h>

static FontConfig g_fontConfig = {0};

/**
 * @brief Инициализирует конфигурацию шрифта значениями по умолчанию
 * @param config Указатель на структуру конфигурации шрифта
 */
void ui_font_config_init(FontConfig *config) {
  config->fontSize = 20;
  config->fontSpacing = 1;
}

/**
 * @brief Устанавливает глобальную конфигурацию шрифта
 * @param config Указатель на структуру конфигурации шрифта
 */
void ui_set_font_config(const FontConfig *config) { g_fontConfig = *config; }

/**
 * @brief Получает текущую глобальную конфигурацию шрифта
 * @return Структура конфигурации шрифта
 */
FontConfig ui_get_font_config(void) { return g_fontConfig; }

/**
 * @brief Создает кнопку с пользовательскими цветами фона и текста
 * @param bounds Прямоугольник, определяющий позицию и размер кнопки
 * @param text Текст кнопки
 * @param bgColor Цвет фона кнопки
 * @param textColor Цвет текста кнопки
 * @return true если кнопка была нажата, false в противном случае
 */
bool ui_button_ex(Rectangle bounds, const char *text, Color bgColor,
                  Color textColor) {
  // Сохраняем оригинальные цвета стиля
  int originalBg = GuiGetStyle(BUTTON, BASE_COLOR_NORMAL);
  int originalText = GuiGetStyle(BUTTON, TEXT_COLOR_NORMAL);

  // Устанавливаем пользовательские цвета
  GuiSetStyle(BUTTON, BASE_COLOR_NORMAL, ColorToInt(bgColor));
  GuiSetStyle(BUTTON, TEXT_COLOR_NORMAL, ColorToInt(textColor));

  // Рисуем кнопку
  bool result = GuiButton(bounds, text);

  // Восстанавливаем оригинальные цвета
  GuiSetStyle(BUTTON, BASE_COLOR_NORMAL, originalBg);
  GuiSetStyle(BUTTON, TEXT_COLOR_NORMAL, originalText);

  return result;
}

bool ui_slider_with_label(Rectangle bounds, const char *label, float *value,
                          float minValue, float maxValue, const char *format) {
  float labelWidth = bounds.width * 0.3f;
  float sliderWidth = bounds.width * 0.5f;
  float valueWidth = bounds.width * 0.2f;

  // Draw label
  GuiLabel((Rectangle){bounds.x, bounds.y, labelWidth, bounds.height}, label);

  // Draw slider
  bool changed = GuiSliderBar(
      (Rectangle){bounds.x + labelWidth, bounds.y, sliderWidth, bounds.height},
      NULL, NULL, value, minValue, maxValue);

  // Draw value
  char valueText[32];
  snprintf(valueText, sizeof(valueText), format, *value);
  GuiLabel((Rectangle){bounds.x + labelWidth + sliderWidth, bounds.y,
                       valueWidth, bounds.height},
           valueText);

  return changed;
}

bool ui_color_picker(Rectangle bounds, const char *label, Color *color) {
  float labelWidth = bounds.width * 0.3f;
  float colorWidth = bounds.width * 0.7f;

  // Get theme colors for border and use theme accent colors for cycling
  const ThemeColors *theme = options_get_current_theme();

  // Draw label
  GuiLabel((Rectangle){bounds.x, bounds.y, labelWidth, bounds.height}, label);

  // Draw color preview
  Rectangle colorRect = {bounds.x + labelWidth, bounds.y + 2, colorWidth - 4,
                         bounds.height - 4};
  DrawRectangleRec(colorRect, *color);
  DrawRectangleLinesEx(colorRect, 1, theme->overlay0); // Theme-aware border

  // Check for click to open color picker
  if (CheckCollisionPointRec(GetMousePosition(), colorRect) &&
      IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
    // Cycle through theme accent colors instead of generic colors
    static int themeColorIndex = 0;
    Color themeAccentColors[] = {
        theme->blue,     theme->lavender, theme->sapphire, theme->sky,
        theme->teal,     theme->green,    theme->yellow,   theme->peach,
        theme->maroon,   theme->red,      theme->mauve,    theme->pink,
        theme->flamingo, theme->rosewater};
    themeColorIndex = (themeColorIndex + 1) % (sizeof(themeAccentColors) /
                                               sizeof(themeAccentColors[0]));
    *color = themeAccentColors[themeColorIndex];
    return true;
  }

  return false;
}

bool ui_color_palette_picker(Rectangle bounds, const char *label, Color *color) {
  float labelWidth = bounds.width * 0.3f;
  float paletteWidth = bounds.width * 0.7f;

  // Get theme colors for styling
  const ThemeColors *theme = options_get_current_theme();

  // Draw label
  GuiLabel((Rectangle){bounds.x, bounds.y, labelWidth, bounds.height}, label);

  // Define 14 colors for the palette
  Color paletteColors[14] = {
      theme->blue,      theme->lavender,  theme->sapphire,  theme->sky,
      theme->teal,      theme->green,     theme->yellow,    theme->peach,
      theme->maroon,    theme->red,       theme->mauve,     theme->pink,
      theme->flamingo,  theme->rosewater
  };

  // Calculate grid layout (7 columns x 2 rows = 14 colors)
  int cols = 7;
  int rows = 2;
  float colorSize = paletteWidth / cols;
  float colorHeight = bounds.height / rows;

  // Draw color palette grid
  for (int row = 0; row < rows; row++) {
    for (int col = 0; col < cols; col++) {
      int colorIndex = row * cols + col;
      if (colorIndex >= 14) break;

      Rectangle colorRect = {
          bounds.x + labelWidth + col * colorSize,
          bounds.y + row * colorHeight,
          colorSize - 1, // Small gap between colors
          colorHeight - 1
      };

      // Check if this color is available (not used by other pegs)
      // This will be determined by the caller - for now assume all are available
      bool isAvailable = true; // TODO: Pass peg colors array to check availability

      // Draw color swatch with availability indication
      Color displayColor = paletteColors[colorIndex];
      if (!isAvailable) {
        // Dim unavailable colors
        displayColor.r = (unsigned char)(displayColor.r * 0.3f);
        displayColor.g = (unsigned char)(displayColor.g * 0.3f);
        displayColor.b = (unsigned char)(displayColor.b * 0.3f);
        displayColor.a = 180; // Semi-transparent
      }

      DrawRectangleRec(colorRect, displayColor);

      // Highlight selected color
      if (ColorToInt(*color) == ColorToInt(paletteColors[colorIndex])) {
        DrawRectangleLinesEx(colorRect, 2, theme->text); // White border for selected
      } else if (isAvailable) {
        DrawRectangleLinesEx(colorRect, 1, theme->overlay0); // Subtle border for available
      } else {
        DrawRectangleLinesEx(colorRect, 1, theme->red); // Red border for unavailable
      }

      // Check for click on this color (only if available)
      if (isAvailable && CheckCollisionPointRec(GetMousePosition(), colorRect) &&
          IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
        *color = paletteColors[colorIndex];
        return true;
      }
    }
  }

  return false;
}
/**
 * @brief Создает палитру цветов с индикацией доступности
 *
 * Отображает сетку из 14 цветов темы с визуальными индикаторами:
 * - Доступные цвета: яркие, с тонкой рамкой
 * - Недоступные цвета: затемненные, с красной рамкой и "X"
 * - Выбранный цвет: белая рамка
 *
 * @param bounds Прямоугольник для размещения компонента
 * @param label Метка компонента
 * @param color Указатель на текущий выбранный цвет
 * @param allPegColors Массив цветов всех колышков для проверки доступности
 * @param currentPegIndex Индекс текущего колышка (чтобы не считать его цвет недоступным)
 * @param totalPegs Общее количество колышков
 * @return true если цвет был изменен, false в противном случае
 */
bool ui_color_palette_picker_with_availability(Rectangle bounds, const char *label, Color *color, const Color *allPegColors, int currentPegIndex, int totalPegs) {
  float labelWidth = bounds.width * 0.3f;
  float paletteWidth = bounds.width * 0.7f;

  // Get theme colors for styling
  const ThemeColors *theme = options_get_current_theme();

  // Draw label
  GuiLabel((Rectangle){bounds.x, bounds.y, labelWidth, bounds.height}, label);

  // Define 14 colors for the palette
  Color paletteColors[14] = {
      theme->blue,      theme->lavender,  theme->sapphire,  theme->sky,
      theme->teal,      theme->green,     theme->yellow,    theme->peach,
      theme->maroon,    theme->red,       theme->mauve,     theme->pink,
      theme->flamingo,  theme->rosewater
  };

  // Calculate grid layout (7 columns x 2 rows = 14 colors)
  int cols = 7;
  int rows = 2;
  float colorSize = paletteWidth / cols;
  float colorHeight = bounds.height / rows;

  // Draw color palette grid
  for (int row = 0; row < rows; row++) {
    for (int col = 0; col < cols; col++) {
      int colorIndex = row * cols + col;
      if (colorIndex >= 14) break;

      Rectangle colorRect = {
          bounds.x + labelWidth + col * colorSize,
          bounds.y + row * colorHeight,
          colorSize - 1, // Small gap between colors
          colorHeight - 1
      };

      // Check if this color is available (not used by other pegs)
      bool isAvailable = true;
      for (int i = 0; i < totalPegs; i++) {
        if (i != currentPegIndex && ColorToInt(allPegColors[i]) == ColorToInt(paletteColors[colorIndex])) {
          isAvailable = false;
          break;
        }
      }

      // Draw color swatch with availability indication
      Color displayColor = paletteColors[colorIndex];
      if (!isAvailable) {
        // Dim unavailable colors
        displayColor.r = (unsigned char)(displayColor.r * 0.4f);
        displayColor.g = (unsigned char)(displayColor.g * 0.4f);
        displayColor.b = (unsigned char)(displayColor.b * 0.4f);
        displayColor.a = 160; // Semi-transparent
      }

      DrawRectangleRec(colorRect, displayColor);

      // Draw "X" mark on unavailable colors
      if (!isAvailable) {
        // Draw diagonal lines to form an "X"
        float lineThickness = 2.0f;
        Color xColor = theme->red; // Red X for unavailable

        // First diagonal line (top-left to bottom-right)
        Vector2 start1 = {colorRect.x + 3, colorRect.y + 3};
        Vector2 end1 = {colorRect.x + colorRect.width - 3, colorRect.y + colorRect.height - 3};
        DrawLineEx(start1, end1, lineThickness, xColor);

        // Second diagonal line (top-right to bottom-left)
        Vector2 start2 = {colorRect.x + colorRect.width - 3, colorRect.y + 3};
        Vector2 end2 = {colorRect.x + 3, colorRect.y + colorRect.height - 3};
        DrawLineEx(start2, end2, lineThickness, xColor);
      }

      // Highlight selected color
      if (ColorToInt(*color) == ColorToInt(paletteColors[colorIndex])) {
        DrawRectangleLinesEx(colorRect, 2, theme->text); // White border for selected
      } else if (isAvailable) {
        DrawRectangleLinesEx(colorRect, 1, theme->overlay0); // Subtle border for available
      } else {
        DrawRectangleLinesEx(colorRect, 1, theme->red); // Red border for unavailable
      }

      // Check for click on this color (only if available)
      if (isAvailable && CheckCollisionPointRec(GetMousePosition(), colorRect) &&
          IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
        *color = paletteColors[colorIndex];
        return true;
      }
    }
  }

  return false;
}

bool ui_text_input_with_label(Rectangle bounds, const char *label, char *text,
                              int textSize) {
  static int focusedFieldX = -1;
  static int focusedFieldY = -1;

  // Use field position as stable identifier
  int fieldIdX = (int)bounds.x;
  int fieldIdY = (int)bounds.y;

  float labelWidth = bounds.width * 0.4f;
  float inputWidth = bounds.width * 0.6f;

  // Draw label
  GuiLabel((Rectangle){bounds.x, bounds.y, labelWidth, bounds.height}, label);

  // Create input rectangle
  Rectangle inputRect = {bounds.x + labelWidth, bounds.y, inputWidth,
                         bounds.height};

  // Check if this field should be focused
  bool isFocused = (focusedFieldX == fieldIdX && focusedFieldY == fieldIdY);

  // Handle mouse clicks for focus management
  if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) {
    if (CheckCollisionPointRec(GetMousePosition(), inputRect)) {
      // Clicked on this field - focus it
      focusedFieldX = fieldIdX;
      focusedFieldY = fieldIdY;
      isFocused = true;
    } else if (isFocused) {
      // Clicked outside this field while it was focused - unfocus it
      focusedFieldX = -1;
      focusedFieldY = -1;
      isFocused = false;
    }
  }

  // Draw text input
  bool result = GuiTextBox(inputRect, text, textSize, isFocused);

  return result;
}

void ui_panel_with_title(Rectangle bounds, const char *title) {
  GuiPanel(bounds, title);
}

Rectangle ui_center_modal_rect(int width, int height) {
  int screenWidth = GetScreenWidth();
  int screenHeight = GetScreenHeight();
  return (Rectangle){(screenWidth - width) / 2.0f,
                     (screenHeight - height) / 2.0f, (float)width,
                     (float)height};
}

void ui_draw_text(const char *text, Vector2 position, Color color) {
  if (g_fontConfig.fontSize > 0) {
    DrawTextEx(GuiGetFont(), text, position, (float)g_fontConfig.fontSize,
               (float)g_fontConfig.fontSpacing, color);
  } else {
    DrawTextEx(GuiGetFont(), text, position, 20, 1, color);
  }
}

float ui_get_text_width(const char *text) {
  if (g_fontConfig.fontSize > 0) {
    return MeasureTextEx(GuiGetFont(), text, (float)g_fontConfig.fontSize,
                         (float)g_fontConfig.fontSpacing)
        .x;
  } else {
    return MeasureTextEx(GuiGetFont(), text, 20, 1).x;
  }
}
