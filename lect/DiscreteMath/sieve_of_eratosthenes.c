#include <stdio.h>
#include <math.h>
#include <stdlib.h>

void sieve_of_eratosthenes(int n) {
    int *arr = (int *)calloc(n + 1, sizeof(int)); // выделяем память и инициализируем массив нулями

    arr[0] = arr[1] = 1; // 0 и 1 не являются простыми числами

    for (int i = 2; i <= round(sqrt(n)); i++) {
        if (arr[i] == 0) {
            for (int j = i * i; j <= n; j += i) {
                arr[j] = 1;
            }
        }
    }

    // Выводим простые числа
    for (int i = 2; i <= n; i++) {
        if (arr[i] == 0) {
            printf("%d ", i);
        }
    }
    printf("\n");

    free(arr); // освобождаем выделенную память
}

int main() {
    int n;
    printf("Введите число n: ");
    scanf("%d", &n);

    sieve_of_eratosthenes(n);

    return 0;
}