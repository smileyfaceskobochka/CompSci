#pragma once

#define MAX_ACTIONS_PER_BUTTON 10
#define MAX_TOPBAR_BUTTONS 10

#include <stdbool.h>

typedef struct {
  const char *label;
  bool *actionFlag;
  bool triggered;
} Action;

typedef struct {
  const char *label;
  Action actions[MAX_ACTIONS_PER_BUTTON];
  int actionCount;
  bool isOpen;
} TopBarButton;

typedef struct {
  TopBarButton buttons[MAX_TOPBAR_BUTTONS];
  int buttonCount;
  float height;
  float buttonPadding;
} TopBar;

void init_topbar(TopBar *topbar, float height, float padding);

int add_topbar_button(TopBar *topbar, const char *label);

void add_topbar_action(TopBar *topbar, const char *label, int buttonIndex,
                       bool *flag);

void handle_topbar(TopBar *topbar);

void update_topbar(TopBar *topbar);

// Render only the dropdown menus (for separate rendering order)
void render_topbar_dropdowns(TopBar *topbar);
