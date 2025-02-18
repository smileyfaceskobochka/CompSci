#include <stdio.h>
#include <stdlib.h>

int n;

// Функция для нахождения обратного по модулю числа a по модулю m
int modMultInverse(int a, int m) {
    int originalModulus = m; // Сохраняем оригинальное значение m
    int y = 0;               // Коэффициент для y (временная переменная)
    int x = 1;               // Коэффициент для x (временная переменная)

    // Если m равно 1, обратного элемента не существует, возвращаем 0
    if (m == 1) return 0;

    // Алгоритм Евклида для нахождения обратного элемента
    while (a > 1) {
        // div - целая часть от деления a на m
        int intDiv = a / m;
        int tempMod = m; // Временная переменная для хранения m

        // Обновляем m и a
        m = a % m;     // Остаток от деления a на m
        a = tempMod;   // Обновляем a на старое значение m
        int tempY = y; // Временная переменная для хранения y

        // Обновляем y и x
        y = x - intDiv * y; // Обновляем y
        x = tempY;          // Обновляем x
    }

    // Если x отрицательное, значение m для получения положительного результата
    if (x < 0) x += originalModulus;

    return x; // Возвращаем обратное по модулю значение
}

// Функция для преобразования числа из СОК в десятичную СС (ост, базисы) 
int residueToDec(int *residues, int *moduli) {
    int prod = 1; // Произведение модулей
    int res = 0;  // Результат в десятичной системе

    // Вычисление произведения модулей
    for (int i = 0; i < n; i++) {
        prod *= moduli[i];
    }

    // Вычисление результата в десятичной системе
    for (int i = 0; i < n; i++) {
        int partProd = prod / moduli[i];               // Частичное произведение
        int inv = modMultInverse(partProd, moduli[i]); // Обратное по модулю число
        res += residues[i] * partProd * inv;
    }
    return res % prod; // Результат по модулю product
}

// Функция для сложения чисел в системе остаточных классов (базисы, ост1, ост2)
int calcSum(int *moduliArr, int *residues1Arr, int *residues2Arr) {
    int *residueSum = (int *)malloc(n * sizeof(int));

    for (int i = 0; i < n; i++) {
        residueSum[i] = (residues1Arr[i] + residues2Arr[i]) % moduliArr[i];
    }
    int result = residueToDec(residueSum, moduliArr);
    free(residueSum); // Освобождаем память
    return result;
}

// Функция для записи чисел в массивы (массив)
int* writeToArr() {
    int *arr = (int *)malloc(n * sizeof(int));
    for (int i = 0; i < n; i++) {
        scanf("%d", &arr[i]);
    }
    return arr;
}

int exitWithErr(int *moduli, int *residues0, int *residues1) {
    int range = 1; // находим диапазон +1
    for (int i = 0; i < n; i++) {
        range *= moduli[i];
    }

    // Преобразуем остатки в десятичное число
    int decimalValue0 = residueToDec(residues0, moduli);
    int decimalValue1 = residueToDec(residues1, moduli);

    int totValue = decimalValue0 + decimalValue1;
    if (totValue < 0 || totValue >= range) {
        return -1; // Если сумма выходит за пределы диапазона, возвращаем -1
    }
    return 0;
}

int main() {
    scanf("%d", &n);

    int *moduli = writeToArr();
    int *residues0 = writeToArr();
    int *residues1 = writeToArr();

    int resVal = calcSum(moduli, residues0, residues1);
    if (exitWithErr(moduli, residues0, residues1) == -1) {
        printf("-1\n");
    } else {
        printf("%d\n", resVal);
    }

    // Освобождаем выделенную память
    free(moduli);
    free(residues0);
    free(residues1);

    return 0;
}
