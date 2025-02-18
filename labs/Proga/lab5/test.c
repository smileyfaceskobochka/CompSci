#include <stdio.h>
#include <stdlib.h>

// По возрастанию
int bol(int a, int b, int *array, int i, int j){
    if (array != NULL && i != j){
        int temp = array[i];
        array[i] = array[j];
        array[j] = temp;
    }
    return a < b;
}

// По убыванию
int mensh(int a, int b, int *array, int i, int j){
    if (array != NULL && i != j){
        int temp = array[i];
        array[i] = array[j];
        array[j] = temp;
    }
    return a > b;
}

// Функция для выбора медианы из трёх
int medianOfThree(int Q[], int low, int high) {
    int mid = low + (high - low) / 2;
    
    if (Q[low] > Q[mid]) {
        int temp = Q[low];
        Q[low] = Q[mid];
        Q[mid] = temp;
    }
    if (Q[low] > Q[high]) {
        int temp = Q[low];
        Q[low] = Q[high];
        Q[high] = temp;
    }
    if (Q[mid] > Q[high]) {
        int temp = Q[mid];
        Q[mid] = Q[high];
        Q[high] = temp;
    }
    
    return mid; // Возвращаем индекс медианы
}

// Разделение
int partition(int Q[], int low, int high, int (*comparee)(int, int, int *, int, int)){
    // Выбираем опорный элемент как медиану из трёх
    int medianIndex = medianOfThree(Q, low, high);
    int temp = Q[medianIndex];
    Q[medianIndex] = Q[high];
    Q[high] = temp;
    
    int opora = Q[high];  // Опорный элемент
    int i = low - 1;      // Индекс меньшего элемента
    for (int j = low; j <= high - 1; j++){
        if (comparee(Q[j], opora, Q, i + 1, j)){
            i++;
        }
    }
    temp = Q[i + 1];
    Q[i + 1] = Q[high];
    Q[high] = temp;
    return i + 1;
}

// Рекурсивная сортировка
void quickSort(int Q[], int low, int high, int (*comparee)(int, int, int *, int, int)){
    if (low < high){
        int pi = partition(Q, low, high, comparee);
        quickSort(Q, low, pi - 1, comparee);
        quickSort(Q, pi + 1, high, comparee);
    }
}

int main() {
    FILE *inputF = fopen("data/gen_input.txt", "r");
    FILE *outputF = fopen("data/output.txt", "w");

    int ABC=1; 

    int n;
    fscanf(inputF, "%d", &n);
    int *Q = (int *)malloc(n * sizeof(int));

    // Считываем массив чисел
    for (int i = 0; i < n; i++){
        fscanf(inputF, "%d", &Q[i]);
    }
    fclose(inputF);

    int (*comparee)(int, int, int *, int, int);
    if (ABC == 1){
        comparee = bol;  
    }else{comparee = mensh;}

    // Сортируем массив
    quickSort(Q, 0, n - 1, comparee);

    for (int i = 0; i < n; i++){
        fprintf(outputF, "%d ", Q[i]);
    }
    fclose(outputF);

    free(Q);
    return 0;
}
