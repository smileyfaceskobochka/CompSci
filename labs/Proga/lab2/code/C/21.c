#include <stdio.h>
#include <stdlib.h>

int main(){
    int n;
    scanf("%d", &n);
    
    if (n < 3) {
        printf("No");
        return 0;
    }

    int *arr = (int*)malloc(sizeof(int)*n);

    for (int i = 0; i < n; i++) {
        scanf("%d", &arr[i]);
    }

    int sawLike = 1;

    for (int i = 1; i < n - 1; i++) {
        // не условие
        if (!((arr[i] > arr[i - 1] && arr[i] > arr[i + 1]) || (arr[i] < arr[i - 1] && arr[i] < arr[i + 1]))) {
            sawLike = 0;
            break;
        }
    }

    if (sawLike) {
        printf("Yes");
    } else {
        printf("No");
    }

    free(arr);
    return 0;
}