#ifndef CDLL_H
#define CDLL_H

// Узел кольцевого двусвязного списка
typedef struct Node {
  char *data;        // Данные узла
  struct Node *next; // Указатель на следующий узел
  struct Node *prev; // Указатель на предыдущий узел
} Node;

// Структура для управления списком
typedef struct {
  Node *head; // Указатель на голову списка
} CDLLists;

// Создает новый узел с данными
Node *createNode(const char *data);

// Добавляет новый элемент в конец списка
void append(CDLLists *list, const char *data);

// Выводит содержимое списка
void display(const CDLLists *list);

// Удаляет элемент из списка
void deleteNode(CDLLists *list, const char *data);

// Освобождает память, занятую списком
void freeList(CDLLists *list);

#endif // CDLL_H