#include <SDL3/SDL.h>

#include <stdio.h>
#include <string.h>

#include "utils.h"
#include "args_parser.h"
#include "window.h"

int main(int argc, char *argv[]) {
  WindowConfig config = {
    .w_title = "Кривая Гилберта // W/S - увел./уменш. глубину // Z/X - увел./уменьш. масштаб",
    .w_width = 800,
    .w_height = 600
  };

  parse_arguments(argc, argv, &config);
  
  if (init_window(&config) != 0) {
    fprintf(stderr, "Failed to initialize window\n");
    return 1;
  }

  update_loop();

  return 0;
}