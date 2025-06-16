#ifndef ARGS_PARSER_H
#define ARGS_PARSER_H
#include "window.h"
#include <SDL3/SDL.h>
// Парсинг аргументов командной строки
int parse_arguments(int argc, char *argv[], WindowConfig *config);

#endif // ARGS_PARSER_H