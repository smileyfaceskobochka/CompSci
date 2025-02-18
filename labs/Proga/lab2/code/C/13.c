#include <stdio.h>
#include <stdlib.h>

int main(){
    int n;
    scanf("%d", &n);
    int *arr = (int*)malloc(sizeof(int)*n);
    int *narr = (int*)malloc(sizeof(int)*n);

    for(int i = 0; i < n; i++){
        scanf("%d", &arr[i]);
    }

    for(int i = 0; i < n; i++){
        if(arr[i]==0){
            if(i == 0){
                narr[0] = arr[n-1] + arr[n-2];
            } else if (i == 1){
                narr[1] = arr[0] + arr[n-1];
            } else narr[i] = arr[i-2] + arr[i-1];
        } else narr[i] = arr[i];
    }

    for(int i = 0; i < n; i++){
        printf("%d ", narr[i]);
    }
    free(arr);
    free(narr);
    return 0;
}