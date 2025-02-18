#include <stdio.h>
#include <stdlib.h>

int main(){
    int n;
    scanf("%d", &n);

    int *arr = (int*)malloc(sizeof(int)*n);

    for (int i = 0; i < n; i++){
        scanf("%d", &arr[i]);
    }

    int cnt, res, ind = 0;

    for (int i = 0; i < n; i++){
        if (arr[i] == 1){
            cnt++;
            if (res <= cnt){
                ind = i-cnt+1;
                res = cnt;
            }
        } else cnt = 0;
    } 
    printf("%d %d", ind, res);
    free(arr);
    return 0;
}