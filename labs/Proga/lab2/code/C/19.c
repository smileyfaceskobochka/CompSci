#include <stdio.h>
#include <stdlib.h>

int main(){
    int n, m;
    scanf("%d %d\n", &n, &m);

    int **arr = (int**)malloc(sizeof(int*)*n);
    for (int i = 0; i < n; i++) {
        arr[i] = (int*)malloc(m * sizeof(int));
    }

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            scanf("%d", &arr[i][j]);
        }
    }

    double sum = 0;
    for (int j = 0; j < m; j++) {
        sum += arr[0][j];
    }
    double min_avg = sum / m;

    for (int i = 1; i < n; i++) {
        sum = 0;
        for (int j = 0; j < m; j++) {
            sum += arr[i][j];
        }
        double avg = sum / m;
        if (avg < min_avg) {
            min_avg = avg;
        }
    }

    printf("%.2f\n", min_avg);

    for (int i = 0; i < n; i++) {
        free(arr[i]);
    }
    free(arr);
}