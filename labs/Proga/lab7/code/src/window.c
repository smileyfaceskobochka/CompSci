#include "window.h"
#include "utils.h"
#include "render_gilbert.h"

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

  init_gilbert(3, config->w_width, config->w_height);

  return 0;
}

void handle_events(bool *running) {
  SDL_Event event;
  while (SDL_PollEvent(&event)) {
    if (event.type == SDL_EVENT_QUIT) {
      destroy_window();
      SDL_Quit();
      exit(0);
    } else if (event.type == SDL_EVENT_KEY_DOWN) {
      if (event.key.key == SDLK_ESCAPE) {
          *running = false;
      }
    } else if (event.type == SDL_EVENT_WINDOW_RESIZED) {
      int new_width = event.window.data1;
      int new_height = event.window.data2;
      init_gilbert(3, new_width, new_height); // Regenerate curve with new dimensions
      SDL_Log("Window resized to %dx%d", new_width, new_height);
    }
  }
}

void update_loop() {
  bool running = true;
  Uint32 frame_start_time = 0;
  while (running) {
    frame_start_time = SDL_GetTicks();

    handle_events(&running);

    // Buffer clear
    SDL_Color bg_color = hexa_to_rgba(CP_MOCHA_BASE, 1.0);
    SDL_SetRenderDrawColor(renderer, bg_color.r, bg_color.g, bg_color.b, bg_color.a);
    SDL_RenderClear(renderer);
    
    render_gilbert();

    SDL_RenderPresent(renderer);

    Uint32 frame_time = SDL_GetTicks() - frame_start_time;
    if (frame_time < 16) { // 1000ms / 60 ≈ 16ms
      SDL_Delay(16 - frame_time);
    }
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