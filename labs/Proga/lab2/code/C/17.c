#include <stdio.h>
#include <stdlib.h>

int main(){
    int n, k;
    scanf("%d %d\n", &n, &k);

    int *arr = (int*)malloc(sizeof(int)*n);

    for (int i = 0; i < n; i++){
        scanf("%d", &arr[i]);
    }

    int cnt = 1;

    for (int i = 0; i < n; i++){
        int sum = 0;

        for(int j = i; j < n; j++){
            sum += arr[j];

            if ((sum <= k) && (j - i + 1 > cnt)){
                cnt = j - i + 1;
            }
        }
    }
    printf("%d", cnt);
    free(arr);
    return 0;
}