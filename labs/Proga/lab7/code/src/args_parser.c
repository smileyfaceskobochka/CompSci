#include "args_parser.h"
#include <string.h>
#include <stdio.h>

int parse_arguments(int argc, char *argv[], WindowConfig *config) {
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-n") == 0 || strcmp(argv[i], "--name") == 0) {
      if (i + 1 < argc && argv[i + 1][0] != '-') {
        config->title = argv[++i];
      } else {
        fprintf(stderr, "Error: Missing value for %s\n", argv[i]);
        return 1;
      }
    } else if (strcmp(argv[i], "-s") == 0 || strcmp(argv[i], "--size") == 0) {
      if (i + 1 < argc && argv[i + 1][0] != '-') {
        if (sscanf(argv[++i], "%dx%d", &config->width, &config->height) != 2 || config->width <= 160 || config->height <= 120) {
          fprintf(stderr, "Invalid size. Using 800x600.\n");
          config->width = 800;
          config->height = 600;
        }
      } else {
        fprintf(stderr, "Error: Missing value for %s\n", argv[i]);
        return 1;
      }
    } else {
      fprintf(stderr, "Unknown option: %s\n", argv[i]);
      return 1;
    }
  }
  return 0;
}