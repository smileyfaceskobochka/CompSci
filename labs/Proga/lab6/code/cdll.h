#ifndef CDLL_H
#define CDLL_H

typedef struct Node {
  char *data;
  struct Node *next;
  struct Node *prev;
} Node;

typedef struct {
  Node *head;
} CDLLists;

Node *createNode(const char *data);

void append(CDLLists *list, const char *data);

void display(const CDLLists *list);

void deleteNode(CDLLists *list, const char *data);

void freeList(CDLLists *list);

#endif // CDLL_H