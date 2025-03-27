#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cdll.h"

Node* createNode(const char* data) {
    Node* newNode = (Node*)malloc(sizeof(Node));
    if (!newNode) {
        fprintf(stderr, "Memory allocation failed!\n");
        exit(EXIT_FAILURE);
    }
    newNode->data = (char*)malloc(strlen(data) + 1);
    if (!newNode->data) {
        fprintf(stderr, "Memory allocation for data failed!\n");
        free(newNode);
        exit(EXIT_FAILURE);
    }
    strcpy(newNode->data, data);
    newNode->next = newNode;
    newNode->prev = newNode;
    return newNode;
}

// Добавление элемента в список
void append(CDLLists* list, const char* data) {
    Node* newNode = createNode(data);
    if (!list->head) {
        list->head = newNode;
    } else {
        Node* tail = list->head->prev;
        tail->next = newNode;
        newNode->prev = tail;
        newNode->next = list->head;
        list->head->prev = newNode;
    }
}

// Отображение элементов списка
void display(const CDLLists* list) {
    if (!list->head) {
        printf("Список пустой :(.\n");
        return;
    }
    
    Node* current = list->head;
    do {
        printf("%s <-> ", current->data);
        current = current->next;
    } while (current != list->head);
    printf("(head)\n");
}

// Удаление элемента и освобождение памяти
void deleteNode(CDLLists* list, const char* data) {
    if (!list->head) return;

    Node* current = list->head;

    do {
        if (strcmp(current->data, data) == 0) {
            if (current->next == current) { // Only one node
                free(current->data); // Free the allocated string
                free(current); // Free the node
                list->head = NULL;
            } else {
                current->prev->next = current->next;
                current->next->prev = current->prev;
                if (current == list->head) {
                    list->head = current->next; // Move head if necessary
                }
                free(current->data); // Free the allocated string
                free(current); // Free the node
            }
            return;
        }
        current = current->next;
    } while (current != list->head);
}

// Освобождение всего списка
void freeList(CDLLists* list) {
    if (!list->head) return;

    Node* current = list->head;
    Node* temp;
    do {
        temp = current;
        current = current->next;
        free(temp->data); // Free the allocated string
        free(temp); // Free the node
    } while (current != list->head);

    list->head = NULL;
}