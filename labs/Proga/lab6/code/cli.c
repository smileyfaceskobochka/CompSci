#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cdll.h"
#include <readline/history.h>
#include <readline/readline.h>

void print_help() {
  printf("Доступные команды:\n");
  printf("  append <data>  - Добавить элемент в список\n");
  printf("  display        - Показать содержимое списка\n");
  printf("  delete <data>  - Удалить элемент из списка\n");
  printf("  clear          - Очистить экран\n");
  printf("  help           - Показать это сообщение\n");
  printf("  exit           - Выйти из программы\n");
}

char *commands[] = {"append", "display", "delete", "clear",
                    "help",   "exit",    NULL};

char *command_generator(const char *text, int state) {
  static int index, len;
  if (!state) {
    index = 0;
    len = strlen(text);
  }

  char *name;
  while ((name = commands[index++])) {
    if (strncmp(name, text, len) == 0)
      return strdup(name);
  }
  return NULL;
}

char **cli_completion(const char *text, int start, int end) {
  rl_attempted_completion_over = 1;
  return rl_completion_matches(text, command_generator);
}

int parse_command(const char *command) {
  if (strncmp(command, "append ", 7) == 0)
    return 1;
  if (strcmp(command, "display") == 0)
    return 2;
  if (strncmp(command, "delete ", 7) == 0)
    return 3;
  if (strcmp(command, "clear") == 0)
    return 5;
  if (strcmp(command, "help") == 0)
    return 6;
  if (strcmp(command, "exit") == 0)
    return 4;
  return 0;
}

void clear_screen() { printf("\033[H\033[J"); }

int main() {
  CDLLists list = {NULL};
  char *input;

  rl_attempted_completion_function = cli_completion;

  printf("CDLL CLI. Введите 'help' для списка команд.\n");

  while ((input = readline(">> ")) != NULL) {
    if (strlen(input) == 0) {
      free(input);
      continue;
    }

    add_history(input);

    int cmd = parse_command(input);
    switch (cmd) {
    case 1: { // append
      char data[256];
      if (sscanf(input + 7, "%255s", data) == 1) {
        append(&list, data);
      } else {
        printf("Ошибка формата: append <data>\n");
      }
      break;
    }

    case 2: // display
      display(&list);
      break;

    case 3: { // delete
      char data[256];
      if (sscanf(input + 7, "%255s", data) == 1) {
        deleteNode(&list, data);
      } else {
        printf("Ошибка формата: delete <data>\n");
      }
      break;
    }

    case 4: // exit
      freeList(&list);
      free(input);
      return 0;

    case 5: // clear
      clear_screen();
      break;

    case 6: // help
      print_help();
      break;

    default:
      printf("Неизвестная команда. Напишите 'help'.\n");
    }
    free(input);
  }

  freeList(&list);
  return 0;
}