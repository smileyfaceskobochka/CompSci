#include <stdio.h>

int main() {
    int n;
    scanf("%d", &n);
    
    int en = 0;
    int fr = 0;
    int de = 0;

    for(int i=0; i < n; i++){
        int a;
        scanf("%d", &a);
        if(a == 1){
            en++;
        } if(a == 2){
            fr++;
        } if(a == 3){
            de++;
        }
    }
    printf("%d\n", en);
    printf("%d\n", fr);
    printf("%d\n", de);
    return 0;
}