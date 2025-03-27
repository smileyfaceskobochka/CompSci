#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cdll.h"

#define COMMAND_SIZE 100

int parseCommand(const char* command) {
    if (strncmp(command, "append", 6) == 0) return 1;
    if (strcmp(command, "display") == 0) return 2;
    if (strncmp(command, "delete", 6) == 0) return 3;
    if (strcmp(command, "exit") == 0) return 4;
    return 0; // Unknown command
}

int main() {
    CDLLists list;
    list.head = NULL;
    char command[COMMAND_SIZE];
    char data[100];

    printf("Welcome to Circular Doubly Linked List CLI. Type 'exit' to quit.\n");

    while (1) {
        printf("Enter command (append <data>, display, delete <data>): ");
        fgets(command, COMMAND_SIZE, stdin);
        command[strcspn(command, "\n")] = '\0'; // Remove newline character

        int cmd = parseCommand(command);
        switch (cmd) {
            case 1: // Append
                sscanf(command + 7, "%s", data); // Assumes command is "append <data>"
                append(&list, data);
                printf("Appended: %s\n", data);
                break;
            case 2: // Display
                display(&list);
                break;
            case 3: // Delete
                sscanf(command + 7, "%s", data); // Assumes command is "delete <data>"
                deleteNode(&list, data);
                printf("Deleted: %s\n", data);
                break;
            case 4: // Exit
                freeList(&list);
                return 0;
            default: // Unknown command
                printf("Unknown command.\n");
                break;
        }
    }

    freeList(&list);
    return 0;
}