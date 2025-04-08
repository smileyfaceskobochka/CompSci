#ifndef WINDOW_H
#define WINDOW_H

#include <SDL3/SDL.h>

typedef struct {
  const char *title;
  int width;
  int height;
  SDL_Color bg_color; // Background color
} WindowConfig;

int create_window(const WindowConfig *config);

#endif // WINDOW_H