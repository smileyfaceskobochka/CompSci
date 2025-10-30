#include <stdio.h>
#include <stdlib.h>

void print_error(const char* message, int x, int n) {
    printf("ошибка: %s (x = %d, n = %d)\n", message, x, n);
}

void zero(unsigned int x, int n) {
    printf("0. ");
    if (x >= (1 << n)) {
        print_error("число не входит в диапазон представления для сетки", x, n);
        return;
    }
    for (int i = n - 1; i >= 0; i--) {
        putchar((x & (1 << i)) != 0 ? '1' : '0');
    }
    putchar('\n');
}

void one(int x, int n) {
    printf("1. ");
    if (x <= -(1 << (n - 1)) || x >= (1 << (n - 1))) {
        print_error("число не входит в диапазон представления\
    в прямом коде для сетки", x, n);
        return;
    }
    if (x < 0) {
        putchar('1');
        x = -x;
    } else {
        putchar('0');
    }
    for (int i = n - 2; i >= 0; i--) {
        putchar((x & (1 << i)) != 0 ? '1' : '0');
    }
    printf(" (прямой код)\n");
}

char* two(int x, int n) {
    char *arr = (char*)malloc(sizeof(char) * (n + 1));
    if (x < -(1 << (n - 1)) || x >= (1 << (n - 1))) {
        print_error("число выходит из диапазона представления\
    в дополнительном коде для сетки", x, n);
        free(arr);
        return NULL;
    }
    int mask = 1 << (n - 1);
    for (int i = 0; i < n; i++) {
        arr[i] = (x & mask) != 0 ? '1' : '0';
        mask >>= 1;
    }
    arr[n] = '\0';
    return arr;
}

void three(int x, int n) {
    printf("3. ");
    if (x > (1 << (n - 1)) - 1 || x < -((1 << (n - 1)) - 1)) {
        print_error("число не входит в диапазон представления для сетки", x, n);
        return;
    }
    if (x < 0) {
        x = abs(x);
        x = ~x;
    }
    for (int i = n - 1; i >= 0; i--) {
        putchar(((x >> i) & 1) ? '1' : '0');
    }
    printf(" (обратный код)\n");
}

int four(int x, int y, int n) {
    char* str1 = two(x, n); 
    char* str2 = two(y, n);
    if (str1 == NULL || str2 == NULL){
        return -1;
    }
    int dist = 0;
    for (int i = 0; i < n; i++) {
        if (str1[i] != str2[i]) {
            dist++;
        }
    }
    free(str1);
    free(str2);
    return dist;
}

int main() {
    int x, y, n;
    scanf("%d %d %d", &x, &y, &n);
    zero(x, n);
    one(x, n);
    char* two_result = two(x, n);
    if (two_result != NULL) {
        printf("2. %s (дополнительный код)\n", two_result);
        free(two_result);
    }
    three(x, n);
    int ham_dist = four(x, y, n);
    if (ham_dist >= 0) {
        printf("4. %d (расстояние по Хэммингу)\n", ham_dist);
    }
    return 0;
}
