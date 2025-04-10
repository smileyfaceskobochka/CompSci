#ifndef RENDER_GILBERT_H
#define RENDER_GILBERT_H

#include <SDL3/SDL.h>
#include "utils.h"

typedef struct {
    int x, y; // Coordinates of the point
} Point;

extern Point* gilbert_points;
extern int point_count;
extern int current_point;
extern int gilbert_depth;

void gen_gilbert(int level, float x, float y, float xi, float xj, float yi, float yj);
void init_gilbert(int depth, int width, int height);
void render_gilbert(float scale, int offset_x, int offset_y);

#endif