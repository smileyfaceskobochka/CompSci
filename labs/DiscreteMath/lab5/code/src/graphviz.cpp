// src/graphviz.cpp
#include "graphviz.hpp"
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>

bool generate_dot(int n, const std::vector<std::pair<int, int>> &edges,
                  const std::string &dot_name) {
  std::ofstream fout(dot_name);
  if (!fout.is_open()) {
    std::perror(("Не удалось открыть " + dot_name).c_str());
    return false;
  }
  fout << "digraph G {\n"
       << "  rankdir=LR;\n"
       << "  node [shape=circle, fontsize=12];\n";
  for (size_t k = 0; k < edges.size(); ++k) {
    int src = edges[k].first;
    int dst = edges[k].second;
    fout << "  \"" << (src + 1) << "\" -> \"" << (dst + 1) << "\" [label=\"e"
         << (k + 1) << "\"];\n";
  }
  fout << "}\n";
  return true;
}

bool dot_to_png(const std::string &dot_name) {
  if (dot_name.size() < 4 || dot_name.substr(dot_name.size() - 4) != ".dot") {
    std::cerr << "viz: имя файла должно оканчиваться на .dot\n";
    return false;
  }

  if (std::system("dot -V >/dev/null 2>&1") != 0) {
    std::cerr << "viz: команда 'dot' не найдена в PATH\n";
    return false;
  }

  std::string base = dot_name.substr(0, dot_name.size() - 4);
  std::string png = base + ".png";
  std::ostringstream cmd;
  cmd << "dot -Tpng -o \"" << png << "\" \"" << dot_name << "\"";
  int ret = std::system(cmd.str().c_str());
  if (ret != 0) {
    std::cerr << "viz: ошибка dot (код " << ret << ")\n";
    return false;
  }
  std::cout << "Сохранено: " << dot_name << " → " << png << "\n";
  return true;
}
