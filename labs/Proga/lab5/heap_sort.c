#include <stdio.h>
#include <stdlib.h>

const char *in = "./data/gen_input.txt";
const char *out = "./data/output.txt";

//MARK: - Compare
int ascending(int *arr, int i, int largest, int n) {
    if (i < n && arr[i] > arr[largest]) {
        return i;
    }
    return largest;
}

int descending(int *arr, int i, int largest, int n) {
    if (i < n && arr[i] < arr[largest]) {
        return i;
    }
    return largest;
}

// MARK: - Heapify

void heapify(int *arr, int n, int i, int (*cmp)(int *, int, int, int)) {
    int largest = i;
    int left = 2 * i + 1;
    int right = 2 * i + 2;

    largest = cmp(arr, left, largest, n);

    largest = cmp(arr, right, largest, n);

    if (largest != i) {
        int temp = arr[i];
        arr[i] = arr[largest];
        arr[largest] = temp;

        heapify(arr, n, largest, cmp);
    }
}

// MARK: - Heap Sort

void heap_sort(int *arr, int n, int (*cmp)(int *, int, int, int)) {
    for (int i = n / 2 - 1; i >= 0; i--) {
        heapify(arr, n, i, cmp);
    }

    for (int i = n - 1; i > 0; i--) {
        int temp = arr[0];
        arr[0] = arr[i];
        arr[i] = temp;

        heapify(arr, i, 0, cmp);
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

    // heap_sort(arr, n, ascending);
    heap_sort(arr, n, descending);

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

    system("code ./data/gen_input.txt");
    system("code ./data/output.txt");

    return 0;
}