#ifndef UTILS_H
#define UTILS_H

#include <stdio.h>
#include <stdlib.h>

#include <SDL3/SDL.h>

// Damn hex color palete

#define CP_MOCHA_CRUST 0x11111b     // Catppuccin Mocha Crust
#define CP_MOCHA_MANTLE 0x181825    // Catppuccin Mocha Mantle
#define CP_MOCHA_BASE 0x1e1e2e      // Catppuccin Mocha Base
#define CP_MOCHA_SURFACE_0 0x313244 // Catppuccin Mocha Surface 0
#define CP_MOCHA_SURFACE_1 0x45475a // Catppuccin Mocha Surface 1
#define CP_MOCHA_SURFACE_2 0x585b70 // Catppuccin Mocha Surface 2
#define CP_MOCHA_OVERLAY_0 0x6c7086 // Catppuccin Mocha Overlay 0
#define CP_MOCHA_OVERLAY_1 0x7f849c // Catppuccin Mocha Overlay 1
#define CP_MOCHA_OVERLAY_2 0x9399b2 // Catppuccin Mocha Overlay 2
#define CP_MOCHA_SUBTEXT_0 0xa6adc8 // Catppuccin Mocha Subtext 0
#define CP_MOCHA_SUBTEXT_1 0xbac2de // Catppuccin Mocha Subtext 1
#define CP_MOCHA_TEXT 0xcdd6f4      // Catppuccin Mocha Text
#define CP_MOCHA_ROSEWATER 0xf5e0dc // Catppuccin Mocha Lavander
#define CP_MOCHA_FLAMINGO 0xf2cdcd  // Catppuccin Mocha Flamingo
#define CP_MOCHA_PINK 0xf5c2e7      // Catppuccin Mocha Pink
#define CP_MOCHA_MAUVE 0xcba6f7     // Catppuccin Mocha Mauve
#define CP_MOCHA_RED 0xf38ba8       // Catppuccin Mocha Red
#define CP_MOCHA_MAROON 0xeba0ac    // Catppuccin Mocha Maroon
#define CP_MOCHA_PEACH 0xfab387     // Catppuccin Mocha Peach
#define CP_MOCHA_YELLOW 0xf9e2af    // Catppuccin Mocha Yellow
#define CP_MOCHA_GREEN 0xa6e3a1     // Catppuccin Mocha Green
#define CP_MOCHA_TEAL 0x94e2d5      // Catppuccin Mocha Teal
#define CP_MOCHA_SKY 0x89dceb       // Catppuccin Mocha Sky
#define CP_MOCHA_SAPPHIRE 0x74c7ec  // Catppuccin Mocha Sapphire
#define CP_MOCHA_BLUE 0x89b4fa      // Catppuccin Mocha Blue
#define CP_MOCHA_LAVANDER 0xb4befe  // Catppuccin Mocha Lavander

SDL_Color hexa_to_rgba(unsigned int hex, double opacity);

#endif // UTILS_H