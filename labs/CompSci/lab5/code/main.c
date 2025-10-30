#include <stdio.h>
#include <stdlib.h>

char* zero(float x, int n, int m) {
    char* res = malloc(n + 1);
    if (!res) return NULL;
    union {
        float f;
        unsigned int i;
    } u;
    u.f = x;
    // Знак
    int sign = (u.i >> 31);
    res[0] = sign;
    int e = n - 1 - m;
    // Извлечение экспоненты
    int raw_exp = (u.i >> 23) & 0xFF; // 8 бит для экспоненты
    int exp_bias = (1 << (e - 1)) - 1;
    int exp = raw_exp - 127 + exp_bias;
    // Обработка переполнения/недополнения экспоненты
    if (raw_exp == 0) { // Денормализованное число
        exp = 0;
    } else if (raw_exp == 0xFF) { // Бесконечность / не число
        exp = (1 << e) - 1;
    }
    // Экспонента
    for (int i = 0; i < e; i++) {
        res[1 + i] = ((exp & (1 << (e - 1 - i))) != 0) ? '1' : '0';
    }
    // Мантисса
    unsigned int mantissa = u.i & 0x7FFFFF; // Последние 23 бита
    if (raw_exp != 0) { // Добавляем скрытый бит для нормализованных чисел
        mantissa |= (1 << 23);
    }
    for (int i = 0; i < m; i++) {
        res[1 + e + i] = ((mantissa & (1 << (23 - 1 - i))) != 0) ? '1' : '0';
    }
    res[n] = '\0';
    return res;
} //+

char* one(float x, int n, int m) {
    char* res = malloc(n + 1);
    if (!res) return NULL;
    union {
        float f;
        unsigned int i;
    } u;
    u.f = x;
    // Знак
    int sign = (u.i >> 31);
    res[0] = sign;
    // Мантисса
    unsigned int mantissa = u.i & 0x7FFFFF;
    if ((u.i >> 23) & 0xFF) {
        mantissa |= (1 << 23);
    }
    for (int i = 0; i < m; i++) {
        res[1 + i] = ((mantissa & (1 << (23 - i))) != 0) ? '1' : '0';
    }
    // Порядок
    int e = n - m;
    int exp = (u.i >> 23) & 0xFF;
    int p = exp - 127;
    res[m + 1] = (p + 1 < 0) ? '1' : '0';
    for (int i = m + 2; i < n; i++) {
        res[i] = (((abs(p + 1) >> (n - i - 1)) & 1) != 0) ? '1' : '0';
    }
    res[n] = '\0';
    return res;
}//+

char* two(float x, int n, int m) {
    char* res = malloc(n + 1);
    if (!res) return NULL;
    union {
        float f;
        unsigned int i;
    } u;
    u.f = x;
    // Знак
    int sign = (u.i >> 31);
    res[0] = sign;
    // Мантисса
    unsigned int mantissa = u.i & 0x7FFFFF;
    if ((u.i >> 23) & 0xFF) {
        mantissa |= (1 << 23);
    }
    for (int i = 0; i < m + 1; i++) {
        res[1 + i] = ((mantissa & (1 << (23 - i))) != 0) ? '1' : '0';
    }
    // Хар-ка
    int e = n - m - 1;
    int exp = (u.i >> 23) & 0xFF;
    int p = exp - 127;
    int charc = p + 1 + (1 << (e - 1));
    for (int i = m; i < n; i++) {
        res[i] = (((charc >> (n - i - 1)) & 1) != 0) ? '1' : '0';
    }
    res[n] = '\0';
    return res;
}

int main() {
    float x;
    int n, m;
    scanf("%f %d %d", &x, &n, &m);
    printf("0. %s\n", zero(x, n, m));
    printf("1. %s\n", one(x, n, m));
    printf("2. %s\n", two(x, n, m));
    return 0;
}