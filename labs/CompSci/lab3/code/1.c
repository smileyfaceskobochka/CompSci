#include <stdio.h>

int cntBin(int n){
    int i = 0; //Счетчик нулей в 2-ой записи числа n
    do {
        if (n % 2 == 0){
            i++;
        }
        n /= 2;
    } while (n > 0);
    printf("%d", i); //Вывод i
}

int main(){
    int n; 
    scanf("%d", &n); //Ввод числа n
    if(n < 0){
        return -1;
    } //Вывод ошибки
    cntBin(n);
    return 0;
}