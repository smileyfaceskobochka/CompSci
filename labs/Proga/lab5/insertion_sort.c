#include <stdio.h>
#include <stdlib.h>

int ascending(int *arr, int key, int j) {
  while (j >= 0 && arr[j] > key) {
      j--;
  }
  return j + 1; // Индекс для вставки
}

int descending(int *arr, int key, int j) {
  while (j >= 0 && arr[j] < key) {
      j--;
  }
  return j + 1;
}

void insertionSort(int *arr, int n, int (*cmp)(int *, int, int)) {
  for (int i = 1; i < n; i++) {
      int key = arr[i];
      int j = i - 1;
      int insertIndex = cmp(arr, key, j);

      // Сдвигаем элементы вправо
      while (j >= insertIndex) {
          arr[j + 1] = arr[j];
          j--;
      }
      arr[j + 1] = key;
  }
}

int main() {
  FILE *inputFile = fopen("data/input.txt", "r");
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

  // insertionSort(arr, n, ascending);
  insertionSort(arr, n, descending);

  for (int i = 0; i < n; i++) {
    fprintf(outputFile, "%d ", arr[i]);
  }

  fclose(outputFile);
  free(arr);

  return 0;
}