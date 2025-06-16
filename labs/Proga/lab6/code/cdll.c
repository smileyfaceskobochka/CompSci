#include "cdll.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Node *createNode(const char *data) {
  Node *newNode = (Node *)malloc(sizeof(Node));
  if (!newNode) {
    fprintf(stderr, "Mem alloc failed for newNode! :( \n");
    exit(EXIT_FAILURE);
  }
  newNode->data = (char *)malloc(strlen(data) + 1);
  if (!newNode->data) {
    fprintf(stderr, "Mem alloc failed for data! :( \n");
    free(newNode);
    exit(EXIT_FAILURE);
  }
  strcpy(newNode->data, data);
  newNode->next = newNode;
  newNode->prev = newNode;
  return newNode;
}

void append(CDLLists *list, const char *data) {
  if (!list->head) {
    list->head = createNode(data);
    return;
  }

  Node *current = list->head;
  do {
    if (strcmp(current->data, data) == 0) {
      printf("Элемент '%s' уже существует в списке.\n", data);
      return;
    }
    current = current->next;
  } while (current != list->head);

  Node *newNode = createNode(data);
  Node *tail = list->head->prev;
  tail->next = newNode;
  newNode->prev = tail;
  newNode->next = list->head;
  list->head->prev = newNode;
}

void display(const CDLLists *list) {
  if (!list->head) {
    printf("Список пустой :(.\n");
    return;
  }

  Node *current = list->head;
  do {
    printf("%s <-> ", current->data);
    current = current->next;
  } while (current != list->head);
  printf("(head)\n");
}

void deleteNode(CDLLists *list, const char *data) {
  if (!list->head) {
    printf("Список пуст.\n");
    return;
  }

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
      printf("Элемент '%s' удалён.\n", data);
      return;
    }
    current = current->next;
  } while (current != list->head);

  printf("Элемент '%s' не найден.\n", data);
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