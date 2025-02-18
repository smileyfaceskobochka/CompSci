#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <math.h>

#define MAX_ITEMS 5

double curve(double x) {
    return 2 * pow(x, 3) + 2 * pow(x, 2) + 3 * x + 1;
}

double antiderivative(double x) { // Первообразная
    return 0.5 * pow(x, 4) + (2.0 / 3.0) * pow(x, 3) + 1.5 * pow(x, 2) + x;
}

double integrate(double a, double b, int n) {
    double width = (b - a) / n;
    double area = 0.0;
    for (int i = 0; i < n; i++) {
        double x = a + (i + 0.5) * width;
        double y = curve(x);
        if (y > 0) {
            area += y * width;
        }
    }
    return fabs(area);
}

double integrate_exact(double a, double b) {
    return fabs(antiderivative(b) - antiderivative(a));
}

void estimate_error(double a, double b, int n, double *abs_error, double *rel_error) {
    double numerical = integrate(a, b, n);
    double exact = integrate_exact(a, b);
    *abs_error = fabs(exact - numerical);
    *rel_error = (exact != 0) ? (*abs_error / exact) * 100.0 : 0.0;
}

// Общие вспомогательные функции
void wait_and_return() {
    printf("Нажмите любую клавишу, чтобы вернуться в меню...\n");
    _getch();
}

double find_x() {
    double left = -1, right = 0, centre;
    while (right - left > 1e-10) {
        centre = (left + right) / 2;
        if (curve(centre) * curve(right) < 0) left = centre;
        else right = centre;
    }
    double foundX = (left + right) / 2;
    return foundX;
}

void input_limits(double *a, double *b) {
    printf("Введите пределы интегрирования (a b): ");
    scanf("%lf %lf", a, b);
    if (*a < find_x()) *a = find_x();
    if (*b < find_x()) *b = find_x();
}

void input_rectangles(int *n) {
    printf("Введите количество прямоугольников: ");
    scanf("%d", n);
    if (*n <= 1) {
        printf("Количество прямоугольников должно быть положительным.\n");
        *n = 0;
    }
}

// Меню
void print_menu(int highlight, double a, double b, int n) {
    const char *choices[MAX_ITEMS] = {
        "Ввести верхний и нижний пределы интегрирования",
        "Ввести количество прямоугольников",
        "Вычислить площадь",
        "Найти погрешность",
        "Завершить"};
    system("cls");
    for (int i = 0; i < MAX_ITEMS; ++i) {
        if (highlight == i) printf("> %s (a: %lf, b: %lf, n: %d)\n", choices[i],
a, b, n);
        else printf("%s\n", choices[i]);
    }
}

int main() {
    int highlight = 0;
    int choice = -1;
    double a = 0, b = 0;
    int n = 0;
    int limits = 0, rect = 0, exit = 1;

    while (exit) {
        print_menu(highlight, a, b, n);
        int c = _getch();
        // Скролл меню стрелками и выбор Enter
        switch (c) {
        case 224:
            switch (_getch()) {
            case 72: // Стрелка вверх
                highlight = (highlight == 0) ? MAX_ITEMS - 1 : highlight - 1;
                break;
            case 80: // Стрелка вниз
                highlight = (highlight == MAX_ITEMS - 1) ? 0 : highlight + 1;
                break;
            }
            break;
        case 13: // Enter
            choice = highlight;
            break;
        default:
            continue;
        }

        if (choice >= 0) {
            switch (choice) {
            case 0: // Ввести пределы
                input_limits(&a, &b);
                limits = 1;
                wait_and_return();
                break;
            case 1: // Ввести количество прямоугольников
                input_rectangles(&n);
                rect = (n >= 0);
                wait_and_return();
                break;
            case 2: // Вычислить площадь
                if (!limits) { // Проверка пределов
                    printf("Необходимо сначала задать пределы интегрирования.\n");
                }
                else if (!rect) { // Проверка количества прямоугольников
                    printf("Необходимо сначала задать количество прямоугольников.\n");
                }
                else {
                    double area = integrate(a, b, n);
                    printf("Площадь: %.6lf\n", area);
                }
                wait_and_return();
                limits, rect = 0;
                break;

            case 3: // Найти погрешность
                if (!limits) {
                    printf("Необходимо сначала задать пределы интегрирования.\n");
                }
                else if (!rect) {
                    printf("Необходимо сначала задать количество прямоугольников.\n");
                }
                else {
                    double abs_error, rel_error;
                    estimate_error(a, b, n, &abs_error, &rel_error);
                    printf("Абсолютная погрешность: %.6lf\n", abs_error);
                    printf("Относительная погрешность: %.6lf%%\n", rel_error);
                    printf("-----------------------------------------------\n");
                    printf("Точный: %.6lf\nМетод прямоугольников: %.6lf\n",\
integrate_exact(a, b), integrate(a, b, n));
                }
                wait_and_return();
                limits, rect = 0;
                break;

            case 4: // Завершить
                printf("Выход...\n");
                exit = 0;
            }
            choice = -1; // Сброс выбора
        }
    }
    return 0;
}
