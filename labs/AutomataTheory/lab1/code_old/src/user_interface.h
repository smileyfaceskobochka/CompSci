#pragma once

#include "ui/topbar.h"
#include "ui/components.h"

#include <stdbool.h>

// Font size configuration - easily adjustable
#define UI_FONT_SIZE_SMALL 16
#define UI_FONT_SIZE_MEDIUM 20
#define UI_FONT_SIZE_LARGE 24
#define UI_FONT_SIZE_XLARGE 28

// UI layout constants
#define UI_TOPBAR_HEIGHT 45.0f
#define UI_TOPBAR_PADDING 5.0f
#define UI_BOARD_TOP_MARGIN 45    // Topbar height
#define UI_CONTROLS_HEIGHT 60     // Bottom controls area
#define UI_CONTROLS_BOTTOM_MARGIN 50

#define UI_FONT_SPACING_TIGHT 0
#define UI_FONT_SPACING_NORMAL 1
#define UI_FONT_SPACING_LOOSE 2

// Font loading constants
#define UI_DEFAULT_FONT_SIZE 40

// This struct keeps state of all UI widgets
typedef struct {
  TopBar topbar;

  // Example flags for actions (can expand later)
  bool actionOpen;
  bool actionSave;
  bool actionExit;
  bool actionAboutProgram;
  bool actionAboutAuthor;
  bool actionOptions;
} UIState;

// Initialize all UI elements
void ui_init(UIState *ui);

// Draw/update UI (render + input collection)
void ui_update(UIState *ui);

// Handle triggered actions and set flags
void ui_handle(UIState *ui);

// Render dropdown menus (called after game rendering)
void ui_render_dropdowns(UIState *ui);

// Font and theme initialization
void ui_init_font(Options *options);
void ui_setup_theme(Options *options);

// Shader management
void ui_init_shaders(void);
void ui_cleanup_shaders(void);

Shader ui_get_emergency_shader(void);
Shader ui_get_ring_animation_shader(void);
