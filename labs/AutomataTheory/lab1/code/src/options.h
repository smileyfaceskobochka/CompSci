#pragma once

#include <stdbool.h>
#include <raylib.h>

#define NUM_PEGS 10

// Theme definitions
typedef enum {
    THEME_LIGHT,  // Catppuccin Latte
    THEME_DARK    // Catppuccin Mocha
} ThemeType;

typedef struct {
    Color base;           // Base background
    Color mantle;         // Secondary background
    Color surface;        // Surface elements
    Color surface1;       // Surface variant
    Color text;           // Primary text
    Color subtext0;       // Secondary text
    Color subtext1;       // Tertiary text
    Color overlay0;       // Overlay text
    Color overlay1;       // Overlay text variant
    Color blue;           // Blue accent
    Color lavender;       // Lavender accent
    Color sapphire;       // Sapphire accent
    Color sky;            // Sky accent
    Color teal;           // Teal accent
    Color green;          // Green accent
    Color yellow;         // Yellow accent
    Color peach;          // Peach accent
    Color maroon;         // Maroon accent
    Color red;            // Red accent
    Color mauve;          // Mauve accent
    Color pink;           // Pink accent
    Color flamingo;       // Flamingo accent
    Color rosewater;      // Rosewater accent
} ThemeColors;

typedef struct {
  int windowWidth;
  int windowHeight;
  int emergencyChance;
  Color pegColors[NUM_PEGS];
  int fontSize;
  int fontSpacing;
  ThemeType currentTheme;
  int tickSpeed;  // ticks per second (1-256)
  unsigned int randomSeed;  // Seed for random number generator
} Options;

void options_set_defaults(Options *opts);

// Theme functions
void options_set_theme(ThemeType theme);
const ThemeColors* options_get_current_theme(void);
void options_apply_theme_to_pegs(Color pegColors[NUM_PEGS], ThemeType theme);

// Check if all peg colors are unique
bool options_colors_unique(const Color pegColors[NUM_PEGS]);

// Returns true on success
bool options_load_from_file(const char *path, Options *opts);

// Returns true on success
bool options_save_to_file(const char *path, const Options *opts);
