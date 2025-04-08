#include "utils.h"
#include <stdio.h>
#include <stdlib.h>

void hex_to_rgba(const char *hex, SDL_Color *color) {
  if (strlen(hex) == 9 && hex[0] == '#') {
    sscanf(hex + 1, "%2hhx%2hhx%2hhx%2hhx", &color->r, &color->g, &color->b, &color->a);
  } else {
    fprintf(stderr, "Invalid hex format. Using default color.\n");
    color->r = color->g = color->b = 0;
    color->a = 255; // Черный цвет по умолчанию
  }
}