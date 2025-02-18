#include <stdio.h>

int main(){
    int n, min, index, count = 0;
    while (scanf("%d",&n)==1 && n!=0){
        if (n < min){
            min=n;
            index=count;
        }
        count++;
    }
    printf("%d", index);
    return 0;
}