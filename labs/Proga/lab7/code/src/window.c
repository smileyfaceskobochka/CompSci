#include "window.h"

#include <SDL3/SDL.h>
#include <stdio.h>

int create_window(const WindowConfig *config) {
  // Init SDL
  int init_result = SDL_Init(SDL_INIT_VIDEO);
  const char *err = SDL_GetError();
  if (init_result < 0 && strlen(err) > 0) {
    fprintf(stderr, "SDL_Init Error: %s\n", err);
    return 1;
  }

  // Create window
  SDL_Window *window = SDL_CreateWindow(
    config -> title,
    config -> width,
    config -> height,
    SDL_WINDOW_RESIZABLE // or other flags as needed
  );
  if (!window) {
    fprintf(stderr, "SDL_CreateWindow Error: %s\n", SDL_GetError());
    SDL_Quit();
    return 1;
  }

  // Create renderer
  SDL_Renderer *renderer = SDL_CreateRenderer(window, NULL);
  if (!renderer) {
    fprintf(stderr, "SDL Error: %s\n", SDL_GetError());
    SDL_Quit();
    return 1;
  }
  // Set background color
  SDL_SetRenderDrawColor(renderer, 
    config->bg_color.r,
    config->bg_color.g,
    config->bg_color.b,
    config->bg_color.a
  );
  SDL_RenderClear(renderer);
  SDL_RenderPresent(renderer);
  SDL_Delay(5000);

  // Очистка ресурсов
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
  return 0;
}