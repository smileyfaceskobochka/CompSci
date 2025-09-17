#pragma once

#include <raylib.h>
#include <raygui.h>
#include <stdbool.h>

// Font configuration
typedef struct {
    int fontSize;
    int fontSpacing;
} FontConfig;

// Initialize font configuration with defaults
void ui_font_config_init(FontConfig *config);

// Set global font configuration
void ui_set_font_config(const FontConfig *config);

// Get current font configuration
FontConfig ui_get_font_config(void);

// Reusable UI Components

// Enhanced button with customizable styling
bool ui_button_ex(Rectangle bounds, const char *text, Color bgColor, Color textColor);

// Slider with label and value display
bool ui_slider_with_label(Rectangle bounds, const char *label, float *value, float minValue, float maxValue, const char *format);

// Color picker component
bool ui_color_picker(Rectangle bounds, const char *label, Color *color);

// Color palette picker with predefined colors
bool ui_color_palette_picker(Rectangle bounds, const char *label, Color *color);

// Color palette picker with availability indication
bool ui_color_palette_picker_with_availability(Rectangle bounds, const char *label, Color *color, const Color *allPegColors, int currentPegIndex, int totalPegs);

// Text input with label
bool ui_text_input_with_label(Rectangle bounds, const char *label, char *text, int textSize);

// Panel with title
void ui_panel_with_title(Rectangle bounds, const char *title);

// Centered modal rectangle helper
Rectangle ui_center_modal_rect(int width, int height);

// Draw text with current font configuration
void ui_draw_text(const char *text, Vector2 position, Color color);

// Get text width with current font configuration
float ui_get_text_width(const char *text);
