#include <stdio.h>

void err() {printf("???\n");}

void gen(int n) {
    int bits = 0;
    while ((1 << bits) < n) {bits++;}
    for (int i = 0; i < n; i++) {
        for (int j = bits - 1; j >= 0; j--) {
            int bit = (i >> j) & 1;
            printf("%d", bit);
        }
        printf(" ");
    }
    printf("\n");
    return;
}

int main() {
    int n;
    scanf("%d", &n);
    if (n <= 0) {
        err();
        return 1;
    }
    gen(n);
    return 0;
}
