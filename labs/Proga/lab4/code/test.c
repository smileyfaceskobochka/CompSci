#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Структура узла списка
typedef struct Node {
    char value;
    struct Node* next;
    struct Node* prev;
} Node;

// Структура списка
typedef struct LList {
    size_t size;
    Node* head;
    Node* tail;
} LList;

// Создает новый узел
Node* createNode(char value) {
    Node* node = (Node*)malloc(sizeof(Node));
    node->value = value;
    node->prev = node->next = NULL;
    return node;
}

// Создает новый список
LList* createList() {
    LList* list = (LList*)malloc(sizeof(LList));
    list->size = 0;
    list->head = list->tail = NULL;
    return list;
}

// Удаляет список
void deleteList(LList* list) {
    Node* tmp = list->head;
    Node* next = NULL;
    while (tmp) {
        next = tmp->next;
        free(tmp);
        tmp = next;
    }
    free(list);
}

// Добавляет символ в список
void append(LList* list, char value) {
    Node* node = createNode(value);
    node->prev = list->tail;
    if (list->tail)
        list->tail->next = node;
    list->tail = node;
    if (list->head == NULL)
        list->head = node;
    list->size++;
}

// Удаляет узел из списка
void deleteNode(LList* list, Node* node) {
    if (node == NULL) return;
    if (node->prev)
        node->prev->next = node->next;
    if (node->next)
        node->next->prev = node->prev;
    if (list->head == node)
        list->head = node->next;
    if (list->tail == node)
        list->tail = node->prev;
    list->size--;
    free(node);
}

// Печатает список
void printList(LList* list) {
    Node* current = list->head;
    while (current) {
        printf("%c", current->value);
        current = current->next;
    }
    printf("\n");
}

int main() {
    LList* list = createList();
    char ch;
    char exitBuffer[6] = {0};
    int index = 0;

    printf("Введите последовательность лат. символов оканчивающююся '.',\
 '&' - для удаления предыдущего символа, 'exit' - для выхода.\n");

    int exit = 1;

    while (exit) {
        ch = getchar();

        exitBuffer[index % 5] = ch;
        index++;

        // Проверка на "exit"
        if (index >= 5) {
            if (exitBuffer[(index - 5) % 5] == 'e' &&
                exitBuffer[(index - 4) % 5] == 'x' &&
                exitBuffer[(index - 3) % 5] == 'i' &&
                exitBuffer[(index - 2) % 5] == 't') {
                
                printf("Вы ввели 'exit'. Нажмите любую клавишу для выхода.\n");
                getchar();
                exit = 0;
            }
        }

        // Завершение ввода на '.'
        if (ch == '.') {
            printList(list);
            deleteList(list);
            list = createList(); // Очистка и создание нового списка
            continue;
        }

        if (ch == '&') {
            deleteNode(list, list->tail); // Удаление последнего символа
        } else {
            append(list, ch); // Добавление символа
        }
    }
    deleteList(list);
    return 0;
}