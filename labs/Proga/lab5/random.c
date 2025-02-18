#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

const char *path = "./data/gen_input.txt";

unsigned int hash_string(const char *str) {
    unsigned int hash = 228;
    int c;
    while ((c = *str++)) {
        hash = ((hash << 5) + hash) + c; // hash * 33 + c
    }
    return hash;
}

int main() {
    FILE *file;
    char seed_string[256];

    int n; // n of rand nums

    printf("Enter seed string: ");
    fgets(seed_string, sizeof(seed_string), stdin);

    printf("Enter n of random numbers: ");
    scanf("%d", &n);

    // Remove newline character from the end of the input string
    seed_string[strcspn(seed_string, "\n")] = '\0';

    // Generate a seed from the string
    unsigned int seed = hash_string(seed_string);

    // Seed the random number generator
    srand(seed);

    // Open the file for writing
    file = fopen(path, "w");

    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    // Write the number of random numbers to the file
    fprintf(file, "%d\n", n);

    // Write the random numbers to the file
    for (int i = 0; i < n; i++) {
        fprintf(file, "%d ", rand());
    }

    fclose(file);
    printf("%d nums written to %s\n - seed_s: %s\n - seed: %d", n, path, seed_string, seed);

    return 0;
}