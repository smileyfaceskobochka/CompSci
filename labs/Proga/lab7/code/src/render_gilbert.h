#ifndef RENDER_GILBERT_H
#define RENDER_GILBERT_H

#include <SDL3/SDL.h>
#include "utils.h"

/**
 * @brief Структура точки с целочисленными координатами.
 */
typedef struct {
  int x; ///< координата X
  int y; ///< координата Y
} Point;

/**
 * Глобальный массив точек, представляющих кривую Гилберта.
 */
extern Point *gilbert_points;

/**
 * Общее число точек кривой (n*n, где n = 2^depth).
 */
extern int point_count;

/**
 * Текущий уровень рекурсии (детализации) кривой Гилберта.
 */
extern int gilbert_depth;

/**
 * Рабочая область (центрированный квадрат), в которой генерируется кривая.
 */
extern float g_margin_x; ///< Отступ от левого края до квадрата
extern float g_margin_y; ///< Отступ от верхнего края до квадрата
extern float g_size;     ///< Размер стороны квадрата

/**
 * @brief Рекурсивно генерирует точки кривой Гилберта.
 *
 * @param level Уровень рекурсии.
 * @param x Начальная координата X рабочей области.
 * @param y Начальная координата Y рабочей области.
 * @param xi Горизонтальный шаг по X.
 * @param xj Горизонтальный шаг по Y.
 * @param yi Вертикальный шаг по X.
 * @param yj Вертикальный шаг по Y.
 */
void gen_hilbert(int level, float x, float y, float xi, float xj, float yi, float yj);

/**
 * @brief Инициализирует кривую Гилберта.
 *
 * Вычисляет центрированный квадрат (рабочую область) для генерации кривой,
 * выделяет память для точек и генерирует кривую с помощью рекурсии.
 *
 * @param depth Уровень рекурсии (детализации).
 * @param width Ширина окна.
 * @param height Высота окна.
 */
void init_gilbert(int depth, int width, int height);

/**
 * @brief Отрисовывает квадратную сетку и кривую Гилберта.
 *
 * Точки кривой находятся в центрах ячеек сетки, которая
 * рассчитывается исходя из рабочей области. Применяются масштаб (scale)
 * и смещение (offset_x, offset_y).
 *
 * @param scale Коэффициент масштабирования.
 * @param offset_x Горизонтальное смещение.
 * @param offset_y Вертикальное смещение.
 */
void render_gilbert(float scale, int offset_x, int offset_y);

#endif // RENDER_GILBERT_H
