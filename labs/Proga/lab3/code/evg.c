#include <stdio.h>
#include <math.h>
#include <windows.h>

// Новая функция, под которой вычисляется интеграл
double f(double x) {
    return pow(x, 3) - 2 * pow(x, 2) + 3 * x + 4;
}

// Первообразная функции
double pervf(double x) {
    return 0.25 * pow(x, 4) - (2.0 / 3.0) * pow(x, 3) + 1.5 * pow(x, 2) + 4 * x;
}

// Метод средних прямоугольников
double integ(double a, double b, int n) {
    double dx = (b - a) / n; // разбиение отрезка [a, b] на n равных частей
    double sum = 0.0;

    for (int i = 0; i < n; i++) {
        double x_mid = a + (i + 0.5) * dx;
        double y_mid = f(x_mid);
        sum += y_mid * dx;
    }

    return sum; // суммарная площадь прямоугольников
}

// Функция для вычисления абсолютной погрешности
double AbsPogr(double a, double b, int n) {
    double integ_num = integ(a, b, n);
    double integ_ex = fabs(pervf(b) - pervf(a));

    return fabs(integ_ex - integ_num);
}

// Функция для вычисления относительной погрешности
double OtnosPogr(double a, double b, int n) {
    double integ1 = integ(a, b, n);
    double integ2 = integ(a, b, 2 * n);

    if (integ1 != 0.0) {
        return AbsPogr(a, b, n) / fabs(pervf(b) - pervf(a)) * 100;
    } else return 0.0;
}

// Метод Ньютона
double newtons_method(double initial_guess, double precision) {
    double x = initial_guess;
    while (fabs(f(x)) > precision) {
        x = x - f(x) / (3 * pow(x, 2) - 4 * x + 3); // производная функции
    }
    return x;
}

int main() {
    double a, b; // параметры интегрирования
    int n; // число шагов

    printf("Введите нижний предел интегрирования (a), верхний предел интегрирования (b) и число шагов (n): ");
    scanf("%lf%lf%d", &a, &b, &n);

    // Проверка и замена границы интегрирования, если она меньше корня уравнения
    double root = newtons_method(0.1, 0.00001);
    if (a < root) {
        printf("Нижняя граница интегрирования меньше корня уравнения. Обрезаем ее до значения корня.\n");
        a = root;
        Sleep(3000);
    }

    system("cls");

    char casse; // переменная для выбора пользователем действия

    do {
        // Вывод меню
        printf("Меню\n");
        printf("1. Задать границы интегрирования и шаг заново\n");
        printf("2. Вычислить площадь под графиком функции\n");
        printf("3. Вычислить абсолютную и относительную погрешности\n");
        printf("4. Вывести корень уравнения\n");
        printf("5. Выйти\n");
        printf("Текущая верхняя граница интегрирования: %lf\n", a);
        printf("Текущая нижняя граница интегрирования: %lf\n", b);
        printf("Текущий шаг интегрирования: %d\n", n);
        printf("Текущее значение интеграла: %.6lf\n", integ(a, b, n));
        printf("Выберите действие: ");

        scanf(" %c", &casse);

        // Выбор действия пользователя
        switch (casse) {
            case '1':
                system("cls");
                printf("Введите новые значения границ интегрирования (a и b) и число шагов (n): ");
                scanf("%lf%lf%d", &a, &b, &n);

                // Проверка и замена границы интегрирования, если она меньше корня уравнения
                root = newtons_method(0.1, 0.00001);
                if (a < root) {
                    printf("Нижняя граница интегрирования меньше корня уравнения. Обрезаем ее до значения корня.\n");
                    a = root;
                    Sleep(3000);
                }
                system("cls");
                break;

            case '2':
                system("cls");
                printf("Площадь под графиком функции: %.6lf\n", integ(a, b, n));
                printf("Загрузка...");
                Sleep(3000); // Задержка 3 секунды
                system("cls");
                break;
            case '3':
                system("cls");
                printf("Абсолютная погрешность: %.6lf\n", AbsPogr(a, b, n));
                printf("Относительная погрешность: %.6lf\n", OtnosPogr(a, b, n));
                printf("Загрузка...");
                Sleep(3000); // Задержка 3 секунды
                system("cls");
                break;

            case '4':
                system("cls");
                root = newtons_method(0.1, 0.00001);
                printf("Корень уравнения: %.6lf\n", root);
                printf("Загрузка...");
                Sleep(3000); // Задержка 3 секунды
                system("cls");
                printf("Меню\n");
                break;

            case '5':
                printf("Выход\n");
                return 0;

            default:
                system("cls");
                printf("Некорректный выбор\n");
                Sleep(3000);
                system("cls");
        }
    } while (casse != '5');

    return 0;
}
