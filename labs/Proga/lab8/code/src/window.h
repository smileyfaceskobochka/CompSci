#ifndef WINDOW_H
#define WINDOW_H

#include <SDL3/SDL.h>
#include <stdio.h>

extern SDL_Window* window;
extern SDL_Renderer* renderer;

extern float scale;
extern int offset_x;
extern int offset_y;

typedef struct WindowConfig {
  const char* w_title;
  int w_width;
  int w_height;
} WindowConfig;

int init_window(WindowConfig *config);

void update_loop();

void handle_events(bool *running);

void destroy_window();

#endif