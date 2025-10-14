#pragma once

#include <QtGui/QColor>

class Pyramid {
public:
    int rings = 0;
    QColor color;

    Pyramid();
    Pyramid(QColor c);
};
