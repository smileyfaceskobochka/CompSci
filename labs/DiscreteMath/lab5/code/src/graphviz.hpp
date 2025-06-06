// src/graphviz.hpp
#ifndef GRAPHVIZ_HPP
#define GRAPHVIZ_HPP

#include <string>
#include <utility>
#include <vector>

// Генерация DOT-файла по списку дуг edges
bool generate_dot(int n, const std::vector<std::pair<int, int>> &edges,
                  const std::string &dot_name);

bool dot_to_png(const std::string &dot_name);

#endif // GRAPHVIZ_HPP
