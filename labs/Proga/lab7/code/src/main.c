#include <stdio.h>
#include "window.h"
#include "args_parser.h"
#include "utils.h"

int main(int argc, char *argv[]) {
  // Конфигурация по умолчанию
  WindowConfig config = {
    .title = "Default Window",
    .width = 800,
    .height = 600
  };

  // Парсинг аргументов
  if (parse_arguments(argc, argv, &config) != 0) {
    return 1;
  }

  // Установка цвета фона
  hex_to_rgba("#1e1e2eff", &config.bg_color);

  // Запуск приложения
  return create_window(&config);
}