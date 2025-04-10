#include "render_gilbert.h"
#include "window.h"
#include "utils.h"
#include <math.h>

Point *gilbert_points = NULL;
int point_count = 0;
int current_point = 0;
int gilbert_depth = 3;

static int current_index = 0;

static void add_point(float x, float y) {
    if (current_index < point_count) {
        gilbert_points[current_index].x = (int)x;
        gilbert_points[current_index].y = (int)y;
        current_index++;
    }
}
/**
 * Рекурсивная генерация кривой Гилберта
 *
 * \param level – текущий уровень рекурсии;
 * \param x, y – начальная точка области;
 * \param xi, xj – компоненты вектора для горизонтального направления;
 * \param yi, yj – компоненты вектора для вертикального направления.
 */
static void gen_hilbert(int level, float x, float y, float xi, float xj, float yi, float yj) {
    if (level <= 0) {
        // Вычисляем центр текущей ячейки
        float mid_x = x + (xi + yi) / 2;
        float mid_y = y + (xj + yj) / 2;
        add_point(mid_x, mid_y);
    } else {
        // Первая четверть: поворот влево
        gen_hilbert(level - 1, x, y, yi / 2, yj / 2, xi / 2, xj / 2);
        // Вторая четверть: движение вниз
        gen_hilbert(level - 1, x + xi / 2, y + xj / 2, xi / 2, xj / 2, yi / 2, yj / 2);
        // Третья четверть: движение вправо
        gen_hilbert(level - 1, x + xi / 2 + yi / 2, y + xj / 2 + yj / 2, xi / 2, xj / 2, yi / 2, yj / 2);
        // Четвертая четверть: отражение и поворот
        gen_hilbert(level - 1, x + xi / 2 + yi, y + xj / 2 + yj, -yi / 2, -yj / 2, -xi / 2, -xj / 2);
    }
}

void init_gilbert(int depth, int width, int height) {
    if (gilbert_points) {
        free(gilbert_points);
        gilbert_points = NULL;
    }
    current_index = 0;
    
    // Определяем число ячеек: n = 2^depth, общее число точек = n * n
    int n = 1 << depth;
    point_count = n * n;
    gilbert_points = malloc(point_count * sizeof(Point));
    
    // Используем квадратную область, которая вписывается в окно, с центральным выравниванием
    float size = fminf((float)width, (float)height);
    float margin_x = ((float)width - size) / 2;
    float margin_y = ((float)height - size) / 2;
    
    // Векторы для начальной области: полная ширина и высота соответствуют выбранной области
    // Начинаем с верхнего левого угла области
    gen_hilbert(depth, margin_x, margin_y, size, 0, 0, size);
}

void render_gilbert(float scale, int offset_x, int offset_y) {
    // Отрисовка квадратной сетки
    // int n = (int)sqrt(point_count);
    // if (n <= 0) return;
    
    // Определяем размер ячейки для сетки
    // SDL_Surface* surface = SDL_GetWindowSurface(window);
    // int grid_size = surface->w / n;

    // SDL_Color cell_color = hexa_to_rgba(CP_MOCHA_SURFACE_0, 0.7);
    // SDL_SetRenderDrawColor(renderer, cell_color.r, cell_color.g, cell_color.b, cell_color.a);
    
    // // Вертикальные линии
    // for (int i = 0; i <= n; i++) {
    //     SDL_RenderLine(renderer, i * grid_size * scale + offset_x, offset_y, i * grid_size * scale + offset_x, n * grid_size * scale + offset_y);
    // }

    // // Горизонтальные линии
    // for (int i = 0; i <= n; i++) {
    //     SDL_RenderLine(renderer, offset_x, i * grid_size * scale + offset_y, n * grid_size * scale + offset_x, i * grid_size * scale + offset_y);
    // }
    
    // Отрисовка кривой Гилберта
    if (point_count < 2) return;

    SDL_Color curve_color = hexa_to_rgba(CP_MOCHA_RED, 1.0);
    SDL_SetRenderDrawColor(renderer, curve_color.r, curve_color.g, curve_color.b, curve_color.a);
    
    for (int i = 0; i < point_count - 1; i++) {
        int x1 = gilbert_points[i].x * scale + offset_x;
        int y1 = gilbert_points[i].y * scale + offset_y;
        int x2 = gilbert_points[i + 1].x * scale + offset_x;
        int y2 = gilbert_points[i + 1].y * scale + offset_y;
        SDL_RenderLine(renderer, x1, y1, x2, y2);
    }
}
