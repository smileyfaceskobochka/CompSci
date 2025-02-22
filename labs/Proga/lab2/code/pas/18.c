#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main() {
    int n;
    scanf("%d", &n);

    int div3 = 0; // Счетчик элементов, делящихся нацело на 3
    int evenSum = 0; // Сумма элементов с четными значениями
    int evenCount = 0; // Счетчик элементов с четными значениями

    int *arr = malloc(n * sizeof(int)); // Исходный массив
    int *narr = malloc((n + 2) * sizeof(int)); // Новый массив, увеличенный на 2 элемента

    for (int i = 0; i < n; i++) {
        int x;
        scanf("%d", &x);

        if (x % 3 == 0) {
            div3++; // Увеличиваем счетчик элементов, делящихся на 3
        }

        if (x % 2 == 0) {
            evenSum += x; // Добавляем значение к сумме четных элементов
            evenCount++; // Увеличиваем счетчик четных элементов
        }

        arr[i] = x; // Сохраняем элемент в исходный массив
    }

    int evenAvg = 0; // Среднее арифметическое четных элементов
    if (evenCount > 0) {
        evenAvg = round((double)evenSum / evenCount); // Вычисляем и округляем среднее арифметическое
    }

    // Вставляем количество элементов, делящихся на 3, на первое место
    narr[0] = div3;

    // Копируем элементы исходного массива
    for (int i = 0; i < n; i++) {
        narr[i + 1] = arr[i];
    }

    // Вставляем среднее арифметическое элементов с четными значениями на последнее место
    narr[n + 1] = evenAvg;

    // Выводим новый массив
    for (int i = 0; i < n + 2; i++) {
        printf("%d ", narr[i]);
    }
    printf("\n");

    free(arr); // Освобождаем память, выделенную для исходного массива
    free(narr); // Освобождаем память, выделенную для нового массива

    return 0;
}