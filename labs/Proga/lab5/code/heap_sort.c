#include <stdio.h>
#include <stdlib.h>

const char *in = "./data/gen_input.txt";
const char *out = "./data/output.txt";

int cmp_ascending(int *array, int index, int extreme_index, int size) {
    if (index < size && array[index] > array[extreme_index]) {
        return index;
    }
    return extreme_index;
}

int cmp_descending(int *array, int index, int extreme_index, int size) {
    if (index < size && array[index] < array[extreme_index]) {
        return index;
    }
    return extreme_index;
}

void heapify(int *array, int size, int root_index, int (*cmp)(int *, int, int, int)) {
    int largest_index = root_index;
    int left_child_index = 2 * root_index + 1;
    int right_child_index = 2 * root_index + 2;

    largest_index = cmp(array, left_child_index, largest_index, size);
    largest_index = cmp(array, right_child_index, largest_index, size);

    if (largest_index != root_index) {
        int temp = array[root_index];
        array[root_index] = array[largest_index];
        array[largest_index] = temp;

        heapify(array, size, largest_index, cmp);
    }
}

void heap_sort(int *array, int size, int (*cmp)(int *, int, int, int)) {
    for (int i = size / 2 - 1; i >= 0; i--) {
        heapify(array, size, i, cmp);
    }

    for (int i = size - 1; i > 0; i--) {
        int temp = array[0];
        array[0] = array[i];
        array[i] = temp;

        heapify(array, i, 0, cmp);
    }
}

int main() {
    FILE *file = fopen(in, "r");
    if (file == NULL) {
        return 1;
    }

    int n;
    fscanf(file, "%d", &n);

    int *arr = malloc(sizeof(int) * n);
    if (arr == NULL) {
        fclose(file);
        return 1;
    }
    for (int i = 0; i < n; i++) {
        fscanf(file, "%d", &arr[i]);
    }

    fclose(file);

    // heap_sort(arr, n, cmp_ascending);
    heap_sort(arr, n, cmp_descending);

    FILE *output = fopen(out, "w");
    if (output == NULL) {
        free(arr);
        return 1;
    }

    for (int i = 0; i < n; i++) {
        fprintf(output, "%d ", arr[i]);
    }

    fclose(output);
    free(arr);

    return 0;
}