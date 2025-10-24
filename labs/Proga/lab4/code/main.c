#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Структура узла списка
typedef struct Node {
  char value;
  int next;
  int prev;
} Node;

// Структура списка
typedef struct LList {
  int size;
  int head;
  int tail;
  Node nodes[100];
} LList;

// Глобальная переменная для списка
LList list;

// Создает новый список
void createList() {
  list.size = 0;
  list.head = -1;
  list.tail = -1;
}

// Добавляет символ в список
void append(char value) {
  if (list.size >= 100) {
    printf("Переполнение списка.\n");
    return;
  }
  int index = list.size;
  list.nodes[index].value = value;
  list.nodes[index].prev = list.tail;
  list.nodes[index].next = -1;
  if (list.tail != -1)
    list.nodes[list.tail].next = index;
  list.tail = index;
  if (list.head == -1)
    list.head = index;
  list.size++;
}

// Удаляет узел из списка
void deleteNode(int index) {
  if (index == -1 || list.size == 0)
    return;
  if (list.nodes[index].prev != -1)
    list.nodes[list.nodes[index].prev].next = list.nodes[index].next;
  if (list.nodes[index].next != -1)
    list.nodes[list.nodes[index].next].prev = list.nodes[index].prev;
  if (list.head == index)
    list.head = list.nodes[index].next;
  if (list.tail == index)
    list.tail = list.nodes[index].prev;
  list.size--;
}

// Печатает список
void printList() {
  int current = list.head;
  while (current != -1) {
    printf("%c", list.nodes[current].value);
    current = list.nodes[current].next;
  }
  printf("\n");
}

int main() {
  createList();
  char ch, prevCh = '\0';

  printf("Введите последовательность лат. символов оканчивающююся '.',\
 'Ch' - для удаления предыдущего символа.\n");

  ch = getchar();
  while (ch != '.') {
    if (prevCh == 'C' && ch == 'h') {
      deleteNode(list.tail);
      prevCh = '\0';
    } else {
      if (prevCh != '\0' && prevCh != 'C') {
        append(prevCh);
      }
      prevCh = ch;
    }
    ch = getchar();
  }
  if (prevCh != '\0' && prevCh != 'C' && prevCh != 'h') {
    append(prevCh);
  }
  printList();

  return 0;
}