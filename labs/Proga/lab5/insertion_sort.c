#include <stdio.h>
#include <stdlib.h>

int *ascending(int *arr, int key, int j) {
  while (j >= 0 && arr[j] > key) {
    arr[j + 1] = arr[j];
    j = j - 1;
  }
  arr[j + 1] = key;

  return arr;
}

int *descending(int *arr, int key, int j) {
  while (j >= 0 && arr[j] < key) {
    arr[j + 1] = arr[j];
    j = j - 1;
  }
  arr[j + 1] = key;

  return arr;
}

void insertionSort(int *arr, int n, int *(*cmp)(int*, int, int)) {
  for (int i = 1; i < n; i++) {
    int key = arr[i];
    int j = i - 1;

    cmp(arr, key, j);
  }
}

int main() {
  FILE *inputFile = fopen("data/gen_input.txt", "r");
  FILE *outputFile = fopen("data/output.txt", "w");

  if (inputFile == NULL || outputFile == NULL) {
    printf("Error opening file!\n");
    return 1;
  }

  int n;
  fscanf(inputFile, "%d", &n);

  int *arr = (int *)malloc(n * sizeof(int));
  for (int i = 0; i < n; i++) {
    fscanf(inputFile, "%d", &arr[i]);
  }

  fclose(inputFile);

  insertionSort(arr, n, ascending);

  for (int i = 0; i < n; i++) {
    fprintf(outputFile, "%d ", arr[i]);
  }

  fclose(outputFile);
  free(arr);

  return 0;
}