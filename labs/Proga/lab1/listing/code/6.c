#include <stdio.h>

int main() 
{
    long long x1, y1, x2, y2;
    long long S = 0;

    scanf("%lld %lld %lld %lld", &x1, &y1, &x2, &y2);
    
    //bool statement ? T : F
    long long min_x = (x1 < x2) ? x1 : x2;
    long long max_x = (x1 > x2) ? x1 : x2;
    long long min_y = (y1 < y2) ? y1 : y2;
    long long max_y = (y1 > y2) ? y1 : y2;

    long long peresech_min_x = (min_x < 0) ? 0 : min_x;
    long long peresech_max_x = (max_x < 0) ? 0 : max_x;
    long long peresech_min_y = (min_y < 0) ? 0 : min_y;
    long long peresech_max_y = (max_y < 0) ? 0 : max_y;

    long long width = peresech_max_x - peresech_min_x;
    long long height = peresech_max_y - peresech_min_y;

    long long area = 0;
    if (width > 0 && height > 0) {
        area = width * height;
    }
    printf("%lld\n", area);
    return 0;
}