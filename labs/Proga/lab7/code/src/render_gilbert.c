#include "render_gilbert.h"
#include "utils.h"
#include "window.h"
#include <math.h>
#include <stdlib.h>

/* Глобальный массив точек кривой и параметры */
Point *gilbert_points = NULL;
int point_count = 0;
int gilbert_depth = 3;
static int current_index = 0;

/* Параметры рабочей области (центрированного квадрата, где строится кривая) */
float g_margin_x = 0.0f; // отступ по горизонтали
float g_margin_y = 0.0f; // отступ по вертикали
float g_size = 0.0f;     // размер стороны квадрата

/**
 * @brief Добавляет точку в массив точек.
 *
 * Точка записывается как центр ячейки, вычисленный рекурсивным алгоритмом.
 *
 * @param x Координата X точки.
 * @param y Координата Y точки.
 */
static void add_point(float x, float y) {
  if (current_index < point_count) {
    gilbert_points[current_index].x = (int)x;
    gilbert_points[current_index].y = (int)y;
    current_index++;
  }
}

/**
 * @brief Рекурсивная генерация точки кривой Гилберта.
 *
 * @param level Уровень рекурсии.
 * @param x Начальная координата X рабочей области.
 * @param y Начальная координата Y рабочей области.
 * @param xi Горизонтальный шаг по X.
 * @param xj Горизонтальный шаг по Y.
 * @param yi Вертикальный шаг по X.
 * @param yj Вертикальный шаг по Y.
 */
void gen_hilbert(int level, float x, float y, float xi, float xj, float yi,
                 float yj) {
  if (level <= 0) {
    // Вычисляем центр ячейки
    float mid_x = x + (xi + yi) / 2.0f;
    float mid_y = y + (xj + yj) / 2.0f;
    add_point(mid_x, mid_y);
  } else {
    gen_hilbert(level - 1, x, y, yi / 2.0f, yj / 2.0f, xi / 2.0f, xj / 2.0f);
    gen_hilbert(level - 1, x + xi / 2.0f, y + xj / 2.0f, xi / 2.0f, xj / 2.0f,
                yi / 2.0f, yj / 2.0f);
    gen_hilbert(level - 1, x + xi / 2.0f + yi / 2.0f, y + xj / 2.0f + yj / 2.0f,
                xi / 2.0f, xj / 2.0f, yi / 2.0f, yj / 2.0f);
    gen_hilbert(level - 1, x + xi / 2.0f + yi, y + xj / 2.0f + yj, -yi / 2.0f,
                -yj / 2.0f, -xi / 2.0f, -xj / 2.0f);
  }
}

/**
 * @brief Инициализация кривой Гилберта.
 *
 * Вычисляет рабочую область (центрированный квадрат) для генерации кривой,
 * выделяет память для точек и генерирует кривую.
 *
 * @param depth Уровень рекурсии (детализации).
 * @param width Ширина окна.
 * @param height Высота окна.
 */
void init_gilbert(int depth, int width, int height) {
  if (gilbert_points) {
    free(gilbert_points);
    gilbert_points = NULL;
  }
  current_index = 0;
  gilbert_depth = depth;

  // Число ячеек: n = 2^depth, общее число точек = n*n
  int n = 1 << depth;
  point_count = n * n;
  gilbert_points = malloc(point_count * sizeof(Point));

  // Определяем рабочую область — квадрат, вписывающийся в окно (центрированный)
  g_size = fminf((float)width, (float)height);
  g_margin_x = ((float)width - g_size) / 2.0f;
  g_margin_y = ((float)height - g_size) / 2.0f;

  // Генерируем кривую в рабочей области
  gen_hilbert(depth, g_margin_x, g_margin_y, g_size, 0, 0, g_size);
}

/**
 * @brief Отрисовывает квадратную сетку и кривую Гилберта.
 *
 * Сетка строится в пределах рабочей области, где каждая ячейка имеет размер
 * g_size/n. Точки кривой, генерируемые функцией gen_hilbert, находятся в
 * центрах ячеек. Применяется масштабирование и смещение.
 *
 * @param scale Коэффициент масштабирования.
 * @param offset_x Горизонтальное смещение.
 * @param offset_y Вертикальное смещение.
 */
void render_gilbert(float scale, int offset_x, int offset_y) {
  if (g_size <= 0 || point_count < 2)
    return;

  int n = 1 << gilbert_depth;   // Количество ячеек по стороне
  float cell_size = g_size / n; // Размер ячейки

  // Рисуем квадратную сетку
  SDL_Color grid_color = hexa_to_rgba(CP_MOCHA_SURFACE_1, 1.0);
  SDL_SetRenderDrawColor(renderer, grid_color.r, grid_color.g, grid_color.b,
                         grid_color.a);

  // Вертикальные линии
  for (int i = 0; i <= n; i++) {
    float x = g_margin_x + i * cell_size;
    int rx = (int)(x * scale + offset_x);
    int ry_start = (int)(g_margin_y * scale + offset_y);
    int ry_end = (int)((g_margin_y + g_size) * scale + offset_y);
    SDL_RenderLine(renderer, rx, ry_start, rx, ry_end);
  }

  // Горизонтальные линии
  for (int i = 0; i <= n; i++) {
    float y = g_margin_y + i * cell_size;
    int ry = (int)(y * scale + offset_y);
    int rx_start = (int)(g_margin_x * scale + offset_x);
    int rx_end = (int)((g_margin_x + g_size) * scale + offset_x);
    SDL_RenderLine(renderer, rx_start, ry, rx_end, ry);
  }

  // Рисуем кривую Гилберта (соединяем центры ячеек)
  SDL_Color curve_color = hexa_to_rgba(CP_MOCHA_RED, 1.0);
  SDL_SetRenderDrawColor(renderer, curve_color.r, curve_color.g, curve_color.b,
                         curve_color.a);

  for (int i = 0; i < point_count - 1; i++) {
    int x1 = (int)(gilbert_points[i].x * scale + offset_x);
    int y1 = (int)(gilbert_points[i].y * scale + offset_y);
    int x2 = (int)(gilbert_points[i + 1].x * scale + offset_x);
    int y2 = (int)(gilbert_points[i + 1].y * scale + offset_y);
    SDL_RenderLine(renderer, x1, y1, x2, y2);
  }
}
