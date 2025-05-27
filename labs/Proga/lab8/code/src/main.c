#include <SDL3/SDL.h>

#include <stdio.h>
#include <string.h>

#include "utils.h"
#include "window.h"

int main() {
  WindowConfig cfg = {.w_title = "sort", .w_height = 600, .w_width = 800};

  if (init_window(&cfg) != 0) {
    fprintf(stderr, "Failed to initialize window\n");
    return 1;
  }

  update_loop();

  return 0;
}
