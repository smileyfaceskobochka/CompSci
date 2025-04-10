#include "window.h"
#include "utils.h"
#include "render_gilbert.h"

SDL_Window *window = NULL;
SDL_Renderer *renderer = NULL;
float scale = 1.0f;
int offset_x = 0;
int offset_y = 0;

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

  init_gilbert(gilbert_depth, config->w_width, config->w_height);

  return 0;
}

void handle_events(bool *running) {
  SDL_Event event;
  while (SDL_PollEvent(&event)) {
    switch (event.type) {

// #region MARK: KEY_DOWN
// >---------------------KEY_DOWN EVENTS-----------------------<

    case SDL_EVENT_KEY_DOWN: {
      SDL_Keycode key = event.key.key;
      switch (key) {
      case SDLK_ESCAPE: {
        *running = false;
      }
      break;

      case SDLK_Z: { // Зум-ин (увеличение)
        scale += 0.1f;
        if (scale > 8.0f)
          scale = 8.0f;
        fprintf(stdout, "Zoom in | x%.2f%%\n", scale * 100);
      }
      break;

      case SDLK_X: { // Зум-аут (уменьшение)
        scale -= 0.1f;
        if (scale < 0.1f) scale = 0.1f;
        fprintf(stdout, "Zoom out | x%.2f%%\n", scale * 100);
      }
      break;
      
      // Перемещение
      case SDLK_UP: {
        offset_y += 10;
        fprintf(stdout, "Move up | Y%d\n", offset_y);
      }
      break;

      case SDLK_DOWN: {
        offset_y -= 10;
        fprintf(stdout, "Move down | Y%d\n", offset_y);
      }
      break;

      case SDLK_LEFT: {
        offset_x += 10;
        fprintf(stdout, "Move left | X%d\n", offset_x);
      }
      break;

      case SDLK_RIGHT:{
        offset_x -= 10;
        fprintf(stdout, "Move right | X%d\n", offset_x);
      }
      break;

      // Изменение глубины
      case SDLK_W: {
        gilbert_depth++;
        if (gilbert_depth > 10)
          gilbert_depth = 10;
        int w, h;
        SDL_GetWindowSize(window, &w, &h);
        fprintf(stdout, "Increase depth | %d\n", gilbert_depth);
        init_gilbert(gilbert_depth, w, h);
      }
      break;

      case SDLK_S: {
        gilbert_depth--;
        if (gilbert_depth < 1)
          gilbert_depth = 1;
        int w, h;
        SDL_GetWindowSize(window, &w, &h);
        fprintf(stdout, "Decrease depth | %d\n", gilbert_depth);
        init_gilbert(gilbert_depth, w, h);
      }
      break;
      
      default:
      break;
      }
      break;
    }
// #endregion MARK: KEY_DOWN



// #region MARK: OTHER_EVENTS
// >--------------------------OTHER_EVENTS--------------------------<
    case SDL_EVENT_WINDOW_RESIZED: {
      int w, h;
      SDL_GetWindowSize(window, &w, &h);
      init_gilbert(gilbert_depth, w, h);
      break;
    }

    case SDL_EVENT_QUIT: {
      *running = false;
    }
    break;

    default:
      break;
    }
// #endregion MARK: OTHER_EVENTS
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

    render_gilbert(scale, offset_x, offset_y);

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