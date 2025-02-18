#include <stdio.h>

int main() {
    long n;
    int m;
    long sum = 0;

    scanf("%ld", &n);
    scanf("%d", &m);

    for (int i = 0; i < m; i++) {
        sum += n % 10;
        n /= 10;
    }
    
    printf("%d\n", sum);
    return 0;
}