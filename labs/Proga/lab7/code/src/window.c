#include "window.h"

SDL_Window* window = NULL;
SDL_Renderer* renderer = NULL;

int init_window(WindowConfig *config) {
  int init_res = SDL_Init(SDL_INIT_VIDEO);
  const char *err_msg = SDL_GetError();
  if (init_res != 0 && strlen(err_msg) > 0) {
    fprintf(stderr, "SDL_Init Error: %s\n", err_msg);
    return 1;
  }

  window = SDL_CreateWindow(
    config->w_title,
    config->w_width,
    config->w_height,
    SDL_WINDOW_RESIZABLE
  );

  if (window == NULL) {
    fprintf(stderr, "SDL_CreateWindow Error: %s\n", SDL_GetError());
    SDL_Quit();
    return 1;
  }

  renderer = SDL_CreateRenderer(window, "software"); // direct3d11 direct3d12 direct3d opengl opengles2 vulkan gpu software 
  if (renderer == NULL) {
    fprintf(stderr, "SDL_CreateRenderer Error: %s\n", SDL_GetError());
    SDL_DestroyWindow(window);
    SDL_Quit();
    return 1;
  }

  return 0;
}

void handle_events() {
  SDL_Event event;
  while (SDL_PollEvent(&event)) {
    if (event.type == SDL_EVENT_QUIT) {
      destroy_window();
      SDL_Quit();
      exit(0);
    }
  }
}

void update_loop() {
  while (true) {
    handle_events();

    // Очистка буфера
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255); // Чёрный цвет
    SDL_RenderClear(renderer);

    // Здесь можно добавить отрисовку объектов

    // Презентация кадра
    SDL_RenderPresent(renderer);
  }
}

void destroy_window() {
  if (renderer) {
    SDL_DestroyRenderer(renderer);
    renderer = NULL;
  }
  if (window) {
    SDL_DestroyWindow(window);
    window = NULL;
  }
  fprintf(stdout, "Bye!\n");
  SDL_Quit();
}