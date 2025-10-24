#include "simulation.h"
#include <QtCore/QRandomGenerator>
#include <array>

// Constants
const int NUM_PYRAMIDS = 10;
const int MAX_RINGS = 12;
const int NUM_COLORS = 14;

// 16-color palette (4-bit)
const std::array<QColor, 16> COLOR_PALETTE = {
    QColor(0, 0, 0),       // Black
    QColor(128, 0, 0),     // Maroon
    QColor(0, 128, 0),     // Green
    QColor(128, 128, 0),   // Olive
    QColor(0, 0, 128),     // Navy
    QColor(128, 0, 128),   // Purple
    QColor(0, 128, 128),   // Teal
    QColor(192, 192, 192), // Silver
    QColor(128, 128, 128), // Gray
    QColor(255, 0, 0),     // Red
    QColor(0, 255, 0),     // Lime
    QColor(255, 255, 0),   // Yellow
    QColor(0, 0, 255),     // Blue
    QColor(255, 0, 255),   // Fuchsia
    QColor(0, 255, 255),   // Aqua
    QColor(255, 255, 255)  // White
};

Simulation::Simulation() {
  // Initialize pyramids with unique colors
  for (int i = 0; i < NUM_PYRAMIDS; ++i) {
    pyramids.emplace_back(COLOR_PALETTE[i % NUM_COLORS]);
  }

  // Set default initial values (not all zeros)
  std::vector<int> defaultRings = {2, 1, 3, 0, 4, 1, 2, 3, 0, 1};
  setInitialState(defaultRings);
}

void Simulation::reset() {
  arrayP.clear();
  arrayF.clear();
  for (auto &pair : arrayO) {
    arrayP.push_back(pair);
  }
}

void Simulation::setInitialState(const std::vector<int> &initialRings) {
  arrayO.clear();
  for (int i = 0; i < NUM_PYRAMIDS && i < static_cast<int>(initialRings.size());
       ++i) {
    arrayO.emplace_back(i, initialRings[i]);
  }
  reset();
}

void Simulation::setColors(const std::vector<QColor> &colors) {
  for (int i = 0; i < NUM_PYRAMIDS && i < static_cast<int>(colors.size());
       ++i) {
    pyramids[i].color = colors[i];
  }
}

bool Simulation::step() {
  if (arrayP.empty())
    return false;

  // Если есть отложенная авария, выполняем её
  if (emergencyPending) {
    if (emergencyTarget >= 0 &&
        emergencyTarget < static_cast<int>(arrayP.size())) {
      arrayP[emergencyTarget].second -= 3;
      if (arrayP[emergencyTarget].second < 0)
        arrayP[emergencyTarget].second = 0;
    }
    emergencyPending = false;
    emergencyTarget = -1;
    return !arrayP.empty();
  }

  // Generator 1: Add ring to random pyramid in P
  if (!arrayP.empty()) {
    int idx =
        QRandomGenerator::global()->bounded(static_cast<int>(arrayP.size()));
    arrayP[idx].second++;
    if (arrayP[idx].second >= MAX_RINGS) {
      arrayF.push_back(arrayP[idx]);
      arrayP.erase(arrayP.begin() + idx);
    }
  }

  // Generator 2: Check emergency
  if (emergencyChance >= 100) {
    // При шансе 100% планируем аварию на следующий тик
    std::vector<int> candidates;
    for (int i = 0; i < static_cast<int>(arrayP.size()); ++i) {
      if (arrayP[i].second >= 3) {
        candidates.push_back(i);
      }
    }
    if (!candidates.empty()) {
      int idx = QRandomGenerator::global()->bounded(
          static_cast<int>(candidates.size()));
      emergencyPending = true;
      emergencyTarget = candidates[idx];
    }
  } else if (QRandomGenerator::global()->bounded(100) < emergencyChance) {
    // При шансе < 100% авария происходит в текущем тике
    std::vector<int> candidates;
    for (int i = 0; i < static_cast<int>(arrayP.size()); ++i) {
      if (arrayP[i].second >= 3) {
        candidates.push_back(i);
      }
    }
    if (!candidates.empty()) {
      int idx = QRandomGenerator::global()->bounded(
          static_cast<int>(candidates.size()));
      arrayP[candidates[idx]].second -= 3;
      if (arrayP[candidates[idx]].second < 0)
        arrayP[candidates[idx]].second = 0;
    }
  }

  return !arrayP.empty();
}

void Simulation::syncPyramidRings() {
  for (auto &pyramid : pyramids) {
    pyramid.rings = 0;
  }
  for (const auto &pair : arrayP) {
    if (pair.first >= 0 && pair.first < static_cast<int>(pyramids.size())) {
      pyramids[pair.first].rings = pair.second;
    }
  }
  for (const auto &pair : arrayF) {
    if (pair.first >= 0 && pair.first < static_cast<int>(pyramids.size())) {
      pyramids[pair.first].rings = pair.second;
    }
  }
}

bool Simulation::isFinished() const {
  return arrayP.empty() && arrayF.size() == NUM_PYRAMIDS;
}
