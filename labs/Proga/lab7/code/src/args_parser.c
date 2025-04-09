#include "args_parser.h"
#include <string.h>
#include <stdio.h>

int parse_arguments(int argc, char *argv[], WindowConfig *config) {
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-n") == 0 || strcmp(argv[i], "--name") == 0) {
      if (i + 1 < argc && argv[i + 1][0] != '-') {
        config->w_title = argv[++i];
      } else {
        fprintf(stderr, "Error: Missing value for %s\n", argv[i]);
        return 1;
      }
    } else if (strcmp(argv[i], "-s") == 0 || strcmp(argv[i], "--size") == 0) {
      if (i + 1 < argc && argv[i + 1][0] != '-') {
        if (sscanf(argv[++i], "%dx%d", &config->w_width, &config->w_height) != 2 || config->w_width <= 160 || config->w_height <= 120) {
          fprintf(stderr, "Invalid size. Using 800x600.\n");
          config->w_width = 800;
          config->w_height = 600;
        }
      } else {
        fprintf(stderr, "Error: Missing value for %s\n", argv[i]);
        return 1;
      }
    } else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
      printf("Usage: %s [options]\n", argv[0]);
      printf("Options:\n");
      printf("  -n, --name <title>   Set the window title\n");
      printf("  -s, --size <WIDTHxHEIGHT>   Set the window size (minimum 160x120)\n");
      printf("  -h, --help          Show this help message\n");
      return 0;

    } else {
      fprintf(stderr, "Unknown option: %s\n", argv[i]);
      return 1;
    }
  }
  return 0;
}