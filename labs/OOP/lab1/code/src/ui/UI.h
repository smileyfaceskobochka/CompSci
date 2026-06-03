#pragma once
#include <vector>
#include <string>
#include <memory>
#include "core/FormulaTeam.h"

class UI {
private:
    std::vector<std::shared_ptr<FormulaTeam>> teams;
    std::vector<std::string> logs;

    void addLog(const std::string &text);

public:
    explicit UI(std::vector<std::shared_ptr<FormulaTeam>> initial_teams);
    void run();
};
