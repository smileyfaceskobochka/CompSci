#include "options.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define RGBA values for Catppuccin theme
#define COLOR_CATPPUCCIN(r, g, b) ((Color){(r), (g), (b), 0xff})

// Catppuccin Latte (Light) theme colors
static const ThemeColors latteTheme = {
    .base = COLOR_CATPPUCCIN(0xef, 0xe8, 0xdd),       // Surface background
    .mantle = COLOR_CATPPUCCIN(0xe6, 0xe0, 0xd4),    // Secondary surface
    .surface = COLOR_CATPPUCCIN(0xdc, 0xd6, 0xcb),    // Window/container background
    .surface1 = COLOR_CATPPUCCIN(0xd1, 0xcb, 0xc0),   // Elevated container
    .text = COLOR_CATPPUCCIN(0x4c, 0x4f, 0x69),       // Primary text
    .subtext0 = COLOR_CATPPUCCIN(0x6c, 0x6f, 0x85),   // Secondary text
    .subtext1 = COLOR_CATPPUCCIN(0x8c, 0x8f, 0xa1),   // Tertiary text
    .overlay0 = COLOR_CATPPUCCIN(0x9c, 0xa0, 0xb0),   // Overlay text/icons
    .overlay1 = COLOR_CATPPUCCIN(0xac, 0xb0, 0xbe),   // Higher contrast overlay
    // Semantic colors
    .blue = COLOR_CATPPUCCIN(0x1e, 0x66, 0xf5),       // Primary accent
    .lavender = COLOR_CATPPUCCIN(0x72, 0x89, 0xda),   // Secondary accent
    .sapphire = COLOR_CATPPUCCIN(0x20, 0x95, 0xdf),   // Link/focus
    .sky = COLOR_CATPPUCCIN(0x04, 0xa5, 0xe5),        // Info/success secondary
    .teal = COLOR_CATPPUCCIN(0x17, 0x94, 0x9c),       // Info/success
    .green = COLOR_CATPPUCCIN(0x40, 0xa0, 0x2b),      // Success
    .yellow = COLOR_CATPPUCCIN(0xdf, 0x8e, 0x1d),     // Warning
    .peach = COLOR_CATPPUCCIN(0xfe, 0x64, 0x0b),      // Warning secondary
    .maroon = COLOR_CATPPUCCIN(0xe6, 0x45, 0x53),     // Danger
    .red = COLOR_CATPPUCCIN(0xd2, 0x0f, 0x39),        // Error
    .mauve = COLOR_CATPPUCCIN(0x88, 0x38, 0xcb),      // Purple accent
    .pink = COLOR_CATPPUCCIN(0xea, 0x76, 0xcb),        // Pink accent
    .flamingo = COLOR_CATPPUCCIN(0xdd, 0x78, 0x7d),   // Coral accent
    .rosewater = COLOR_CATPPUCCIN(0xdc, 0x8a, 0x78)   // Warm toned accent
};

// Catppuccin Mocha (Dark) theme colors
static const ThemeColors mochaTheme = {
    .base = COLOR_CATPPUCCIN(0x1e, 0x1e, 0x2e),       // Surface background
    .mantle = COLOR_CATPPUCCIN(0x18, 0x18, 0x25),     // Secondary surface
    .surface = COLOR_CATPPUCCIN(0x31, 0x31, 0x44),    // Window/container background
    .surface1 = COLOR_CATPPUCCIN(0x45, 0x45, 0x59),   // Elevated container
    .text = COLOR_CATPPUCCIN(0xcd, 0xd6, 0xf4),        // Primary text
    .subtext0 = COLOR_CATPPUCCIN(0xa6, 0xad, 0xc8),   // Secondary text
    .subtext1 = COLOR_CATPPUCCIN(0x94, 0x9c, 0xbb),   // Tertiary text
    .overlay0 = COLOR_CATPPUCCIN(0x6c, 0x70, 0x85),   // Overlay text/icons
    .overlay1 = COLOR_CATPPUCCIN(0x7f, 0x84, 0x9c),   // Higher contrast overlay
    // Semantic colors
    .blue = COLOR_CATPPUCCIN(0x89, 0xb4, 0xfa),       // Primary accent
    .lavender = COLOR_CATPPUCCIN(0xb4, 0xbe, 0xfe),   // Secondary accent
    .sapphire = COLOR_CATPPUCCIN(0x74, 0xc7, 0xec),   // Link/focus
    .sky = COLOR_CATPPUCCIN(0x89, 0xd6, 0xeb),        // Info/success secondary
    .teal = COLOR_CATPPUCCIN(0x94, 0xe2, 0xd5),       // Info/success
    .green = COLOR_CATPPUCCIN(0xa6, 0xe3, 0xa1),      // Success
    .yellow = COLOR_CATPPUCCIN(0xf9, 0xe2, 0xaf),     // Warning
    .peach = COLOR_CATPPUCCIN(0xfa, 0xb3, 0x87),      // Warning secondary
    .maroon = COLOR_CATPPUCCIN(0xeb, 0xa0, 0xac),     // Danger
    .red = COLOR_CATPPUCCIN(0xf3, 0x8b, 0xa8),        // Error
    .mauve = COLOR_CATPPUCCIN(0xcb, 0xa6, 0xf7),      // Purple accent
    .pink = COLOR_CATPPUCCIN(0xf5, 0xc2, 0xe7),       // Pink accent
    .flamingo = COLOR_CATPPUCCIN(0xf2, 0xcd, 0xcd),   // Coral accent
    .rosewater = COLOR_CATPPUCCIN(0xf5, 0xe0, 0xdc)   // Warm toned accent
};

// Current theme colors
static ThemeColors currentThemeColors;

void options_set_theme(ThemeType theme) {
    switch (theme) {
        case THEME_LIGHT:
            currentThemeColors = latteTheme;
            break;
        case THEME_DARK:
            currentThemeColors = mochaTheme;
            break;
    }
}

const ThemeColors* options_get_current_theme(void) {
    return &currentThemeColors;
}

bool options_colors_unique(const Color pegColors[NUM_PEGS]) {
    for (int i = 0; i < NUM_PEGS; i++) {
        for (int j = i + 1; j < NUM_PEGS; j++) {
            if (pegColors[i].r == pegColors[j].r &&
                pegColors[i].g == pegColors[j].g &&
                pegColors[i].b == pegColors[j].b &&
                pegColors[i].a == pegColors[j].a) {
                return false;
            }
        }
    }
    return true;
}

void options_apply_theme_to_pegs(Color pegColors[NUM_PEGS], ThemeType theme) {
    const ThemeColors* themeColors = (theme == THEME_LIGHT) ? &latteTheme : &mochaTheme;

    // Assign theme colors to pegs in a nice gradient
    pegColors[0] = themeColors->blue;
    pegColors[1] = themeColors->lavender;
    pegColors[2] = themeColors->sapphire;
    pegColors[3] = themeColors->sky;
    pegColors[4] = themeColors->teal;
    pegColors[5] = themeColors->green;
    pegColors[6] = themeColors->yellow;
    pegColors[7] = themeColors->peach;
    pegColors[8] = themeColors->mauve;
    pegColors[9] = themeColors->pink;
}

void options_set_defaults(Options *opts) {
  opts->windowWidth = 900;
  opts->windowHeight = 600;
  opts->emergencyChance = 50;
  opts->fontSize = 20;
  opts->fontSpacing = 1;
  opts->currentTheme = THEME_DARK;  // Default to dark theme
  opts->tickSpeed = 5;  // Default 5 ticks per second
  opts->randomSeed = 0;  // Default seed (0 means random)

  // Set default theme
  options_set_theme(opts->currentTheme);

  // Apply theme colors to pegs
  options_apply_theme_to_pegs(opts->pegColors, opts->currentTheme);
}

static char *trim(char *s) {
  while (isspace((unsigned char)*s))
    s++;
  if (*s == 0)
    return s;
  char *end = s + strlen(s) - 1;
  while (end > s && isspace((unsigned char)*end))
    end--;
  end[1] = '\0';
  return s;
}

bool options_load_from_file(const char *path, Options *opts) {
  FILE *f = fopen(path, "r");
  if (!f)
    return false;
  options_set_defaults(opts);
  char line[256];
  while (fgets(line, sizeof(line), f)) {
    char *p = strchr(line, '#');
    if (p)
      *p = '\0';
    char *colon = strchr(line, ':');
    if (!colon)
      continue;
    *colon = '\0';
    char *key = trim(line);
    char *val = trim(colon + 1);
    if (strcmp(key, "windowWidth") == 0)
      opts->windowWidth = atoi(val);
    else if (strcmp(key, "windowHeight") == 0)
      opts->windowHeight = atoi(val);
    else if (strcmp(key, "emergencyChance") == 0)
      opts->emergencyChance = atoi(val);
    else if (strcmp(key, "fontSize") == 0)
      opts->fontSize = atoi(val);
    else if (strcmp(key, "fontSpacing") == 0)
      opts->fontSpacing = atoi(val);
    else if (strcmp(key, "currentTheme") == 0)
      opts->currentTheme = atoi(val);
    else if (strcmp(key, "tickSpeed") == 0)
      opts->tickSpeed = atoi(val);
    else if (strcmp(key, "randomSeed") == 0)
      opts->randomSeed = (unsigned int)atoi(val);
    else if (strncmp(key, "pegColor", 8) == 0) {
      int pegIndex = atoi(key + 8);
      if (pegIndex >= 0 && pegIndex < NUM_PEGS) {
        unsigned int colorValue;
        if (sscanf(val, "%x", &colorValue) == 1) {
          opts->pegColors[pegIndex] = GetColor(colorValue);
        }
      }
    }
  }
  fclose(f);
  return true;
}

bool options_save_to_file(const char *path, const Options *opts) {
  FILE *f = fopen(path, "w");
  if (!f)
    return false;
  fprintf(f, "# Options file\n");
  fprintf(f, "windowWidth: %d\n", opts->windowWidth);
  fprintf(f, "windowHeight: %d\n", opts->windowHeight);
  fprintf(f, "emergencyChance: %d\n", opts->emergencyChance);
  fprintf(f, "fontSize: %d\n", opts->fontSize);
  fprintf(f, "fontSpacing: %d\n", opts->fontSpacing);
  fprintf(f, "currentTheme: %d\n", opts->currentTheme);
  fprintf(f, "tickSpeed: %d\n", opts->tickSpeed);
  fprintf(f, "randomSeed: %u\n", opts->randomSeed);

  // Save peg colors
  for (int i = 0; i < NUM_PEGS; i++) {
    fprintf(f, "pegColor%d: %08x\n", i, ColorToInt(opts->pegColors[i]));
  }
  
  fclose(f);
  return true;
}
