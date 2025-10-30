#include <stdio.h>

// Функция для перевода числа в 2СС
int toBin(int n, int i) {
    int binrev[64];
    int j = 0;

    do {
        // Массив в котором представление записывается в обратном порядке
        binrev[j] = n % 2;
        n /= 2;
        j++;
    } while (n > 0);

    if (i < 0 || i >= j) {
        printf("Индекс %d выходит из диапазона представления числа в 2СС.\n", i);
        return -1;
    } // Вывод с ошибкой
    return binrev[i];
}

int main() {    
    int n, i;
    printf("Введите число n и индекс i бита, который хотите узнать: ");
    scanf("%d %d", &n, &i);
    printf("Бит под индексом %d: %d\n", i, toBin(n, i));
    return 0;
}
