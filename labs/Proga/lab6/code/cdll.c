#include "cdll.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Node *createNode(const char *data) {
  // Создаем новый узел и выделяем память для данных
  Node *newNode = (Node *)malloc(sizeof(Node));
  if (!newNode) {
    fprintf(stderr, "Memory allocation failed! :( \n");
    exit(EXIT_FAILURE);
  }
  newNode->data = (char *)malloc(strlen(data) + 1);
  if (!newNode->data) {
    fprintf(stderr, "Memory allocation for data failed! :( \n");
    free(newNode);
    exit(EXIT_FAILURE);
  }
  strcpy(newNode->data, data);
  newNode->next = newNode;
  newNode->prev = newNode;
  return newNode;
}

void append(CDLLists *list, const char *data) {
  Node *newNode = createNode(data);
  if (!list->head) {
    list->head = newNode;
  } else {
    Node *tail = list->head->prev;
    tail->next = newNode;
    newNode->prev = tail;
    newNode->next = list->head;
    list->head->prev = newNode;
  }
}

void display(const CDLLists *list) {
  if (!list->head) {
    printf("Список пустой :(.\n");
    return;
  }

  // Проходим по списку и выводим данные каждого узла
  Node *current = list->head;
  do {
    printf("%s <-> ", current->data);
    current = current->next;
  } while (current != list->head);
  printf("(head)\n");
}

void deleteNode(CDLLists *list, const char *data) {
  if (!list->head)
    return;

  Node *current = list->head;

  do {
    if (strcmp(current->data, data) == 0) {
      if (current->next == current) {
        free(current->data);
        free(current);
        list->head = NULL;
      } else {
        current->prev->next = current->next;
        current->next->prev = current->prev;
        if (current == list->head) {
          list->head = current->next;
        }
        free(current->data);
        free(current);
      }
      return;
    }
    current = current->next;
  } while (current != list->head);

  printf("Такого элемента нет в списке: '%s'\n", data);
}

void freeList(CDLLists *list) {
  if (!list->head)
    return;

  Node *current = list->head;
  Node *temp;
  do {
    temp = current;
    current = current->next;
    free(temp->data);
    free(temp);
  } while (current != list->head);

  list->head = NULL;
}