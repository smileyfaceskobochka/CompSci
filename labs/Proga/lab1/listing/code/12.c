#include <stdio.h>

int Perfecto(int n) {
    int sum = 0;
    for (int i = 1; i <= n / 2; i++) {
        if (n % i == 0) {
            sum += i;
        }
    }
    return sum == n;
}

int main() {
    int n;
    scanf("%d", &n);
    if (Perfecto(n)) {
        printf("YES\n");
    } else {
        printf("NO\n");
    }
    return 0;
}