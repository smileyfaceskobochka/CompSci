#pragma once

#include "pyramid.h"
#include <QtCore/QPair>
#include <vector>

class Simulation {
public:
  std::vector<Pyramid> pyramids;
  std::vector<QPair<int, int>> arrayO; // <pyramid_index, rings>
  std::vector<QPair<int, int>> arrayP; // <pyramid_index, rings>
  std::vector<QPair<int, int>> arrayF; // <pyramid_index, rings>

  int emergencyChance = 0; // 0-100
  bool isRunning = false;
  bool isPaused = false;

  Simulation();
  void reset();
  void setInitialState(const std::vector<int> &initialRings);
  void setColors(const std::vector<QColor> &colors);
  bool step();
  bool isFinished() const;
  void syncPyramidRings();
};
