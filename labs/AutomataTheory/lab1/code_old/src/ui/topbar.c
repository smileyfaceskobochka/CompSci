#include "topbar.h"
#include "../options.h"

#include <raygui.h>
#include <raylib.h>

#include <stdbool.h>
#include <stdio.h>

#define TOPBAR_EXTRA_PADDING 30       // Button sizing constant
#define TOPBAR_DROPDOWN_EXTRA_WIDTH 40 // Extra width for dropdown menus

void init_topbar(TopBar *topbar, float height, float padding) {
  topbar->buttonCount = 0;
  topbar->height = height;
  topbar->buttonPadding = padding;
}

int add_topbar_button(TopBar *topbar, const char *label) {
  if (topbar->buttonCount >= MAX_TOPBAR_BUTTONS)
    return -1;

  int idx = topbar->buttonCount++;
  topbar->buttons[idx].label = label;
  topbar->buttons[idx].actionCount = 0;
  topbar->buttons[idx].isOpen = false;
  return idx;
}

void add_topbar_action(TopBar *topbar, const char *label, int buttonIndex,
                       bool *flag) {
  if (buttonIndex < 0 || buttonIndex >= topbar->buttonCount)
    return;

  TopBarButton *button = &topbar->buttons[buttonIndex];
  if (button->actionCount >= MAX_ACTIONS_PER_BUTTON)
    return;

  button->actions[button->actionCount++] = (Action){label, flag, false};
}

void update_topbar(TopBar *topbar) {
  // Use theme color for topbar background
  const ThemeColors* theme = options_get_current_theme();
  Color topbarColor = LIGHTGRAY; // fallback
  if (theme) {
    topbarColor = theme->mantle;
  }
  DrawRectangle(0, 0, GetScreenWidth(), topbar->height, topbarColor);

  float x = topbar->buttonPadding;
  float buttonHeight = topbar->height - 2 * topbar->buttonPadding;

  for (int i = 0; i < topbar->buttonCount; i++) {
    TopBarButton *button = &topbar->buttons[i];
    // Use current GUI font size for measurement
    int currentFontSize = GuiGetStyle(DEFAULT, TEXT_SIZE);
    int buttonWidth = MeasureTextEx(GuiGetFont(), button->label, currentFontSize, 0).x + TOPBAR_EXTRA_PADDING;

    // Draw the button
    if (GuiButton((Rectangle){x, topbar->buttonPadding, buttonWidth, buttonHeight},
                  button->label)) {
      button->isOpen = !button->isOpen;
      // close other buttons
      for (int j = 0; j < topbar->buttonCount; j++)
        if (j != i)
          topbar->buttons[j].isOpen = false;
    }

    x += buttonWidth + topbar->buttonPadding;
  }
}

void render_topbar_dropdowns(TopBar *topbar) {
  float x = topbar->buttonPadding;
  float buttonHeight = topbar->height - 2 * topbar->buttonPadding;

  for (int i = 0; i < topbar->buttonCount; i++) {
    TopBarButton *button = &topbar->buttons[i];
    // Use current GUI font size for measurement
    int currentFontSize = GuiGetStyle(DEFAULT, TEXT_SIZE);
    int buttonWidth = MeasureTextEx(GuiGetFont(), button->label, currentFontSize, 0).x + TOPBAR_EXTRA_PADDING;

    // Draw actions if open
    if (button->isOpen) {
      float y = topbar->height;
      for (int a = 0; a < button->actionCount; a++) {
        Action *action = &button->actions[a];
        if (GuiButton((Rectangle){x, y, buttonWidth + TOPBAR_DROPDOWN_EXTRA_WIDTH, buttonHeight}, action->label)) {
          action->triggered = true;
          button->isOpen = false;
        }
        y += buttonHeight + topbar->buttonPadding;
      }
    }

    x += buttonWidth + topbar->buttonPadding;
  }
}

void handle_topbar(TopBar *topbar) {
  for (int i = 0; i < topbar->buttonCount; i++) {
    TopBarButton *button = &topbar->buttons[i];
    for (int a = 0; a < button->actionCount; a++) {
      Action *action = &button->actions[a];
      if (action->triggered) {
        *(action->actionFlag) = true;
        action->triggered = false;
      }
    }
  }
}
