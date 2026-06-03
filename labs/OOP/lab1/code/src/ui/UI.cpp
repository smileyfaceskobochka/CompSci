#include "UI.h"
#include <algorithm>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <cmath>
#include "core/F1Team.h"
#include "core/F2Team.h"
#include "core/FormulaETeam.h"

#include <ftxui/component/component.hpp>
#include <ftxui/component/event.hpp>
#include <ftxui/component/screen_interactive.hpp>
#include <ftxui/dom/elements.hpp>
#include <ftxui/dom/table.hpp>
#include <ftxui/screen/screen.hpp>

using namespace ftxui;

UI::UI(std::vector<std::shared_ptr<FormulaTeam>> initial_teams)
    : teams(std::move(initial_teams)) {}

void UI::addLog(const std::string &text) {
  logs.push_back(text);
  if (logs.size() > 50) {
    logs.erase(logs.begin());
  }
}

// Generate pseudo-random telemetry graph (BTOP sparkline style)
std::string generateSparkline(int seed, int size = 14) {
  std::vector<std::string> blocks = {" ", "▂", "▃", "▄", "▅", "▆", "▇", "█"};
  std::string s;
  for (int i = 0; i < size; ++i) {
    // Generate wave pattern
    double angle = (seed + i) * 0.8;
    int val = static_cast<int>((std::sin(angle) + 1.0) * 3.9);
    if (val < 0) val = 0;
    if (val > 7) val = 7;
    s += blocks[val];
  }
  return s;
}

void UI::run() {
  auto screen = ScreenInteractive::TerminalOutput();

  // Color options
  Color color_f1 = Color::RGB(255, 24, 0);     // Red
  Color color_f2 = Color::RGB(255, 215, 0);    // Gold
  Color color_fe = Color::RGB(0, 191, 255);    // Cyber Blue
  Color color_accent = Color::RGB(0, 255, 127); // Neon Green

  // Helper: Exit button styling
  auto ExitButtonStyle = []() {
    auto option = ButtonOption::Simple();
    option.transform = [](const EntryState &s) {
      auto element = text(s.label);
      if (s.focused) {
        element = text("▕ " + s.label + " ▏") | bold | color(Color::Red) | inverted;
      } else {
        element = text("  " + s.label + "  ") | bold | color(Color::Red);
      }
      return element;
    };
    return option;
  };

  // Helper: Points preview
  auto getPointsPreview = [&](int team_idx, const std::string &input_str) -> std::string {
    try {
      int pos = std::stoi(input_str);
      if (pos >= 1 && pos <= 20) {
        return "EST POINTS: +" +
               std::to_string(teams[team_idx]->calculatePoints(pos));
      }
    } catch (...) {
    }
    return "EST POINTS: +0";
  };

  // Tab titles styled like BTOP tabs
  int active_tab = 0;
  std::vector<std::string> tab_titles = {
      " [1] DASHBOARD ",
      " [2] STANDINGS ",
      " [3] TELEMETRY ",
      " [4] COMPARE ",
      " [5] OOP DOCS ",
      " [6] V-TABLE CASTS "
  };

  // Custom horizontal Menu acts as our navigation toggle bar
  auto tab_menu =
      Menu(&tab_titles, &active_tab, MenuOption::HorizontalAnimated());

  // Main UI Shell Exit Button
  auto exit_btn = Button(" QUIT ", screen.ExitLoopClosure(), ExitButtonStyle());

  // --- Tab 1: Dashboard (BTOP Grid Card Style) ---
  auto dashboard_renderer = Renderer([&]() {
    // Generate left side "SYSTEM METRICS" panel
    int total_wins = 0;
    int total_podiums = 0;
    int total_points = 0;
    int driver_count = 0;
    for (const auto &t : teams) {
      total_wins += t->getRaceWins();
      total_podiums += t->getPodiums();
      total_points += t->getChampionshipPoints();
      driver_count += t->getDrivers().size();
    }

    auto system_metrics = vbox({
        text(" SYSTEM MONITOR ") | bold | color(color_accent) | hcenter,
        separatorDouble(),
        hbox(text(" TEAMS COUNT  : "), text(std::to_string(teams.size())) | bold | color(Color::Cyan)),
        hbox(text(" DRIVERS ACTIVE: "), text(std::to_string(driver_count)) | bold | color(Color::Cyan)),
        hbox(text(" TOTAL WINS    : "), text(std::to_string(total_wins)) | bold | color(Color::Yellow)),
        hbox(text(" TOTAL PODIUMS : "), text(std::to_string(total_podiums)) | bold | color(Color::Magenta)),
        hbox(text(" TOTAL POINTS  : "), text(std::to_string(total_points)) | bold | color(color_accent)),
    }) | borderDouble;

    // Build the grid rows for different series
    Elements f1_cards;
    Elements f2_cards;
    Elements fe_cards;

    int t_seed = 0;
    for (const auto &t : teams) {
      std::string series = t->getSeriesName();
      std::string info_line;
      Color series_color = Color::White;

      Element specific_meter = text("");

      if (auto *f1 = dynamic_cast<F1Team *>(t.get())) {
        series_color = color_f1;
        info_line = "Engine: " + f1->getPowerUnit();
        // Budget gauge
        double percentage = (f1->getBudgetCapMln() / 150.0);
        if (percentage > 1.0) percentage = 1.0;
        specific_meter = hbox({
            text("Budget: "),
            gauge(percentage) | size(WIDTH, EQUAL, 10) | color(Color::Cyan),
            text(" " + std::to_string(static_cast<int>(f1->getBudgetCapMln())) + "M") | dim
        });
      } else if (auto *f2 = dynamic_cast<F2Team *>(t.get())) {
        series_color = color_f2;
        info_line = "Chassis: " + f2->getChassisModel();
        // Graduates display
        specific_meter = hbox({
            text("Graduates: "),
            text(std::to_string(f2->getF1Graduates())) | bold | color(Color::Yellow)
        });
      } else if (auto *fe = dynamic_cast<FormulaETeam *>(t.get())) {
        series_color = color_fe;
        info_line = "Energy: " + fe->getEnergyPartner();
        // Sustainability score gauge
        double percentage = (fe->getSustainabilityScore() / 100.0);
        specific_meter = hbox({
            text("Green ESG: "),
            gauge(percentage) | size(WIDTH, EQUAL, 10) | color(Color::Green),
            text(" " + std::to_string(fe->getSustainabilityScore()) + "%") | dim
        });
      }

      auto card = vbox({
          hbox({
              text(t->getTeamName()) | bold | color(Color::White),
              filler(),
              text(std::to_string(t->getChampionshipPoints()) + " PTS") | bold | color(series_color)
          }),
          text(info_line) | dim,
          specific_meter,
          hbox({
              text("Wins: " + std::to_string(t->getRaceWins())) | dim,
              filler(),
              text("Podiums: " + std::to_string(t->getPodiums())) | dim
          })
      }) | borderLight | size(WIDTH, EQUAL, 32);

      if (series == "Formula 1") f1_cards.push_back(card);
      else if (series == "Formula 2") f2_cards.push_back(card);
      else fe_cards.push_back(card);
    }

    auto f1_section = window(text(" FORMULA 1 ") | bold | color(color_f1), hbox(f1_cards) | hscroll_indicator | frame);
    auto f2_section = window(text(" FORMULA 2 ") | bold | color(color_f2), hbox(f2_cards) | hscroll_indicator | frame);
    auto fe_section = window(text(" FORMULA E ") | bold | color(color_fe), hbox(fe_cards) | hscroll_indicator | frame);

    return hbox({
        system_metrics | size(WIDTH, EQUAL, 28),
        separator(),
        vbox({
            f1_section | flex,
            f2_section | flex,
            fe_section | flex,
        }) | flex
    });
  });

  // --- Tab 2: Championship Standings (BTOP styled System Lists) ---
  auto championship_renderer = Renderer([&]() {
    // Constructors table
    std::vector<std::shared_ptr<FormulaTeam>> sorted_teams = teams;
    std::sort(sorted_teams.begin(), sorted_teams.end(),
              [](const std::shared_ptr<FormulaTeam> &a, const std::shared_ptr<FormulaTeam> &b) { return *a > *b; });

    std::vector<std::vector<std::string>> constructors_data = {
        {"POS", "TEAM NAME", "SERIES", "WINS", "PODIUMS", "POINTS"}
    };
    int rank = 1;
    for (const auto &t : sorted_teams) {
      constructors_data.push_back({
          std::to_string(rank++),
          t->getTeamName(),
          t->getSeriesName(),
          std::to_string(t->getRaceWins()),
          std::to_string(t->getPodiums()),
          std::to_string(t->getChampionshipPoints()) + " PTS"
      });
    }

    auto constructors_table = Table(constructors_data);
    constructors_table.SelectAll().Border(LIGHT);
    constructors_table.SelectAll().SeparatorVertical(LIGHT);
    constructors_table.SelectRow(0).Decorate(bold);
    constructors_table.SelectRow(0).Decorate(color(Color::Green));
    for (size_t r = 1; r < constructors_data.size(); ++r) {
      constructors_table.SelectCell(r, 5).Decorate(bold | color(color_accent));
    }

    // Drivers Table
    std::vector<std::pair<FormulaDriver, std::string>> allDrivers;
    for (const auto &t : teams) {
      for (const auto &d : t->getDrivers()) {
        allDrivers.push_back({d, t->getTeamName()});
      }
    }
    std::sort(allDrivers.begin(), allDrivers.end(),
              [](const std::pair<FormulaDriver, std::string> &a,
                 const std::pair<FormulaDriver, std::string> &b) {
                return a.first.getPoints() > b.first.getPoints();
              });

    std::vector<std::vector<std::string>> drivers_data = {
        {"POS", "DRIVER", "CAR #", "TEAM", "POINTS"}
    };
    int drv_rank = 1;
    for (const auto &pair : allDrivers) {
      drivers_data.push_back({
          std::to_string(drv_rank++),
          pair.first.getFullName(),
          "#" + std::to_string(pair.first.getRacingNumber()),
          pair.second,
          std::to_string(pair.first.getPoints()) + " PTS"
      });
    }

    auto drivers_table = Table(drivers_data);
    drivers_table.SelectAll().Border(LIGHT);
    drivers_table.SelectAll().SeparatorVertical(LIGHT);
    drivers_table.SelectRow(0).Decorate(bold);
    drivers_table.SelectRow(0).Decorate(color(Color::Yellow));
    for (size_t r = 1; r < drivers_data.size(); ++r) {
      drivers_table.SelectCell(r, 4).Decorate(bold | color(Color::Yellow));
    }

    return hbox({
        vbox({
            text(" CONSTRUCTORS CHAMPIONSHIP ") | bold | color(Color::Green) | hcenter,
            separator(),
            constructors_table.Render() | flex,
        }) | flex,
        separator(),
        vbox({
            text(" DRIVER STANDINGS ") | bold | color(Color::Yellow) | hcenter,
            separator(),
            drivers_table.Render() | flex,
        }) | flex,
    });
  });

  // --- Tab 3: Live Race Simulator (Telemetry Input & Event Logs) ---
  std::vector<std::string> team_names;
  for (const auto &t : teams) {
    team_names.push_back(t->getTeamName());
  }

  int selected_sim_team_idx = 0;
  std::string sim_pos_str = "1";
  std::string sim_validation_error = "";
  int selected_sim_driver_idx = 0;

  std::vector<std::string> sim_driver_names;

  auto update_sim_drivers = [&]() {
    sim_driver_names.clear();
    const auto &drivers = teams[selected_sim_team_idx]->getDrivers();
    for (const auto &d : drivers) {
      sim_driver_names.push_back(d.getFullName() + " (#" +
                                 std::to_string(d.getRacingNumber()) + ")");
    }
    if (selected_sim_driver_idx >= (int)drivers.size()) {
      selected_sim_driver_idx = 0;
    }
  };

  update_sim_drivers();

  auto sim_team_menu = Menu(&team_names, &selected_sim_team_idx);
  auto sim_driver_menu = Menu(&sim_driver_names, &selected_sim_driver_idx);
  auto sim_pos_input = Input(&sim_pos_str, "1-20");

  auto submit_btn = Button(
      " WRITE TELEMETRY PACKET ",
      [&]() {
        sim_validation_error = "";
        int pos = -1;
        try {
          pos = std::stoi(sim_pos_str);
        } catch (...) {
          sim_validation_error = "ERROR: INVALID INTEGER POSITION!";
          return;
        }
        if (pos < 1 || pos > 20) {
          sim_validation_error = "ERROR: OUT OF RANGE (1-20)!";
          return;
        }

        std::shared_ptr<FormulaTeam> team = teams[selected_sim_team_idx];
        int pts = team->calculatePoints(pos);

        // Add points
        *team += pts;
        if (pos == 1) {
          ++(*team);
        } else if (pos <= 3) {
          team->addPodium();
        }

        // Add points to driver
        auto &drivers = team->getDriversRef();
        if (selected_sim_driver_idx >= 0 &&
            selected_sim_driver_idx < (int)drivers.size()) {
          drivers[selected_sim_driver_idx].addPoints(pts);

          std::string log_msg = "[TELEMETRY EVENT] " + team->getTeamName() +
                                " | " +
                                drivers[selected_sim_driver_idx].getFullName() +
                                " finished P" + std::to_string(pos) +
                                " (+" + std::to_string(pts) + " PTS)";
          addLog(log_msg);
        }
      },
      ButtonOption::Animated());

  auto simulator_controls = Container::Vertical({
      sim_team_menu,
      sim_driver_menu,
      sim_pos_input,
      submit_btn,
  });

  int last_selected_team_idx = -1;

  auto simulator_renderer = Renderer(simulator_controls, [&]() {
    if (selected_sim_team_idx != last_selected_team_idx) {
      update_sim_drivers();
      last_selected_team_idx = selected_sim_team_idx;
    }

    return vbox({
        text(" TELEMETRY SIMULATION CONTROLS ") | bold | color(Color::Red) | hcenter,
        separatorDouble(),
        hbox({
            vbox({
                window(text(" 1. SELECT TEAM ") | bold | color(Color::Cyan),
                       sim_team_menu->Render() | frame |
                           size(HEIGHT, EQUAL, 5) | vscroll_indicator),
                window(text(" 2. SELECT DRIVER ") | bold | color(Color::Cyan), sim_driver_menu->Render()),
                window(text(" 3. FINISH POSITION (1-20) ") | bold | color(Color::Cyan),
                       vbox({sim_pos_input->Render() | borderLight, separator(),
                             (sim_validation_error.empty()
                                  ? text(getPointsPreview(selected_sim_team_idx,
                                                          sim_pos_str)) |
                                        dim | color(Color::Yellow)
                                  : text(sim_validation_error) | bold |
                                        color(Color::Red))})),
                separator(),
                submit_btn->Render() | hcenter,
            }) | size(WIDTH, EQUAL, 45),
            separator(),
            vbox({
                text(" TELEMETRY SYSTEM MANUAL ") | bold | color(Color::Yellow),
                separator(),
                paragraph("• Use the select boxes on the left to queue an active racing driver and telemetry event packet."),
                paragraph("• Input a precise finish rank between 1 and 20. Points are immediately calculated and synchronized."),
                paragraph("• Click 'WRITE TELEMETRY PACKET' to commit to logs and update core statistics arrays."),
                separatorDouble(),
                text(" POINTS MATRIX DIAGNOSTICS ") | bold | color(Color::Cyan),
                separator(),
                paragraph("  - Formula 1: Top 10 (25, 18, 15, 12, 10, 8, 6, 4, 2, 1)"),
                paragraph("  - Formula 2: Top 10 (10, 8, 6, 5, 4, 3, 2, 1, 0, 0)"),
                paragraph("  - Formula E: Top 10 (25, 18, 15, 12, 10, 8, 6, 4, 2, 1)"),
            }) | flex,
        }) | flex,
    });
  });

  // --- Tab 4: Team Comparisons (Side-by-side Telemetry Cards) ---
  int compare_team_a_idx = 0;
  int compare_team_b_idx = 1;

  auto compare_selector_a = Menu(&team_names, &compare_team_a_idx);
  auto compare_selector_b = Menu(&team_names, &compare_team_b_idx);

  auto compare_pane = Renderer([&]() {
    std::shared_ptr<FormulaTeam> ta = teams[compare_team_a_idx];
    std::shared_ptr<FormulaTeam> tb = teams[compare_team_b_idx];

    auto render_card = [](std::shared_ptr<FormulaTeam> t, Color color_theme) {
      std::vector<Element> lines;
      lines.push_back(text(t->getTeamName()) | bold | color(color_theme) | hcenter);
      lines.push_back(separatorDouble());
      lines.push_back(hbox({text("SERIES   : "), text(t->getSeriesName()) | bold}));
      lines.push_back(hbox({text("PRINCIPAL: "), text(t->getPrincipalName())}));
      lines.push_back(hbox({text("HQ BASE  : "), text(t->getHeadquarters())}));
      lines.push_back(hbox({text("POINTS   : "), text(std::to_string(t->getChampionshipPoints())) | bold | color(color_theme)}));
      lines.push_back(hbox({text("WINS     : "), text(std::to_string(t->getRaceWins())) | color(Color::Yellow)}));
      lines.push_back(hbox({text("PODIUMS  : "), text(std::to_string(t->getPodiums())) | color(Color::Magenta)}));

      // Extra sub-fields
      if (auto *f1 = dynamic_cast<F1Team *>(t.get())) {
        lines.push_back(hbox({text("ENG SUPPL: "), text(f1->getPowerUnit()) | color(Color::Cyan)}));
        lines.push_back(hbox({text("BUDGET   : "), text("$" + std::to_string(static_cast<int>(f1->getBudgetCapMln())) + "M") | color(Color::Cyan)}));
      } else if (auto *f2 = dynamic_cast<F2Team *>(t.get())) {
        lines.push_back(hbox({text("CHASSIS  : "), text(f2->getChassisModel()) | color(Color::Cyan)}));
        lines.push_back(hbox({text("GRADUATES: "), text(std::to_string(f2->getF1Graduates())) | color(Color::Yellow)}));
      } else if (auto *fe = dynamic_cast<FormulaETeam *>(t.get())) {
        lines.push_back(hbox({text("PARTNER  : "), text(fe->getEnergyPartner()) | color(Color::Cyan)}));
        lines.push_back(hbox({text("ESG RATE : "), text(std::to_string(fe->getSustainabilityScore()) + "%") | color(Color::Green)}));
      }

      // Show drivers
      lines.push_back(separator());
      lines.push_back(text(" ACTIVE DRIVERS ") | bold | color(color_theme));
      for (const auto &d : t->getDrivers()) {
        lines.push_back(text(" • " + d.getFullName() + " (#" +
                             std::to_string(d.getRacingNumber()) + ") - " +
                             std::to_string(d.getPoints()) + " PTS"));
      }

      return window(text(" DIAGNOSTIC CARD ") | bold, vbox(lines)) | flex;
    };

    std::string comparison_result;
    if (*ta == *tb) {
      comparison_result = "ERROR: COMPARISON TARGETS IDENTICAL";
    } else if (*ta > *tb) {
      comparison_result = ta->getTeamName() + " LEADS IN CHAMPIONSHIP STANDINGS";
    } else {
      comparison_result = tb->getTeamName() + " LEADS IN CHAMPIONSHIP STANDINGS";
    }

    return vbox({
        hbox({
            render_card(ta, Color::Cyan),
            separator(),
            render_card(tb, Color::Magenta),
        }) | flex,
        separator(),
        text(comparison_result) | bold | color(Color::Green) | hcenter,
    });
  });

  auto compare_controls = Container::Horizontal({
      compare_selector_a,
      compare_selector_b,
  });

  auto compare_renderer = Renderer(compare_controls, [&]() {
    return vbox({
        text(" RADAR DIAGNOSTIC CARD COMPARISON ") | bold | color(Color::Magenta) | hcenter,
        separatorDouble(),
        hbox({
            vbox({
                text(" TARGET A ") | bold | color(Color::Cyan) | hcenter,
                separator(),
                compare_selector_a->Render() | frame | size(HEIGHT, EQUAL, 8) |
                    vscroll_indicator,
            }) | size(WIDTH, EQUAL, 25) |
                border,
            separator(),
            compare_pane->Render() | flex,
            separator(),
            vbox({
                text(" TARGET B ") | bold | color(Color::Magenta) | hcenter,
                separator(),
                compare_selector_b->Render() | frame | size(HEIGHT, EQUAL, 8) |
                    vscroll_indicator,
            }) | size(WIDTH, EQUAL, 25) |
                border,
        }) | flex,
    });
  });

  // --- Tab 5: OOP Explainer (Terminal Code Frame) ---
  int selected_oop_topic = 0;
  std::vector<std::string> oop_topics = {
      " 1. ENCAPSULATION ", " 2. INHERITANCE ",
      " 3. POLYMORPHISM ",  " 4. OPERATORS ",
      " 5. ABSTRACTS ",     " 6. CTORS ",
      " 7. STATIC CAST ",   " 8. DYNAMIC CAST "};
  auto oop_menu = Menu(&oop_topics, &selected_oop_topic);

  auto oop_desc = Renderer([&]() {
    std::vector<Element> lines;
    if (selected_oop_topic == 0) {
      lines.push_back(text(" ENCAPSULATION ") | bold | color(Color::Yellow));
      lines.push_back(separatorDouble());
      lines.push_back(paragraph("Hiding internal state representation (private) and forcing access control via explicit interfaces (getters / setters)."));
      lines.push_back(separator());
      lines.push_back(text(" SOURCE SNIPPET: ") | bold);
      lines.push_back(text(" class FormulaDriver {"));
      lines.push_back(text(" private:"));
      lines.push_back(text("   std::string firstName;"));
      lines.push_back(text("   int points;"));
      lines.push_back(text(" public:"));
      lines.push_back(text("   std::string getFirstName() const;"));
      lines.push_back(text("   void addPoints(int pts);"));
      lines.push_back(text(" };"));
    } else if (selected_oop_topic == 1) {
      lines.push_back(text(" INHERITANCE ") | bold | color(Color::Yellow));
      lines.push_back(separatorDouble());
      lines.push_back(paragraph("Deriving new objects from pre-existing ones. F1Team, F2Team, and FormulaETeam acquire standard base capabilities directly from FormulaTeam."));
      lines.push_back(separator());
      lines.push_back(text(" SOURCE SNIPPET: ") | bold);
      lines.push_back(text(" class F1Team : public FormulaTeam {"));
      lines.push_back(text(" private:"));
      lines.push_back(text("   double budgetCapMln;"));
      lines.push_back(text(" };"));
    } else if (selected_oop_topic == 2) {
      lines.push_back(text(" RUNTIME POLYMORPHISM ") | bold | color(Color::Yellow));
      lines.push_back(separatorDouble());
      lines.push_back(paragraph("Manipulating derived class references using parent pointers. The vtable performs runtime resolution to execute the correct overridden methods."));
      lines.push_back(separator());
      lines.push_back(text(" SOURCE SNIPPET: ") | bold);
      lines.push_back(text(" FormulaTeam* team = new F1Team(...);"));
      lines.push_back(text(" team->getSummary(); // Invokes overridden F1Team method at runtime."));
    } else if (selected_oop_topic == 3) {
      lines.push_back(text(" OPERATOR OVERLOADING ") | bold | color(Color::Yellow));
      lines.push_back(separatorDouble());
      lines.push_back(paragraph("Injecting custom operational behavior onto native compiler operators for specialized user classes."));
      lines.push_back(separator());
      lines.push_back(text(" SOURCE SNIPPET: ") | bold);
      lines.push_back(text(" FormulaTeam &FormulaTeam::operator+=(int pts) {"));
      lines.push_back(text("   addPoints(pts); return *this;"));
      lines.push_back(text(" }"));
    } else if (selected_oop_topic == 4) {
      lines.push_back(text(" ABSTRACT CLASSES ") | bold | color(Color::Yellow));
      lines.push_back(separatorDouble());
      lines.push_back(paragraph("Classes declaring pure virtual functions (= 0) cannot be instantiated directly. They enforce structural contracts onto subclasses."));
      lines.push_back(separator());
      lines.push_back(text(" SOURCE SNIPPET: ") | bold);
      lines.push_back(text(" virtual void displayInfo() const = 0;"));
    } else if (selected_oop_topic == 5) {
      lines.push_back(text(" CONSTRUCTORS ") | bold | color(Color::Yellow));
      lines.push_back(separatorDouble());
      lines.push_back(paragraph("Custom blocks of code executed at block initialization. Demonstrates parameterization and delegating constructors."));
      lines.push_back(separator());
      lines.push_back(text(" SOURCE SNIPPET: ") | bold);
      lines.push_back(text(" F1Team::F1Team(...) : FormulaTeam(...), powerUnit(...) {}"));
    } else if (selected_oop_topic == 6) {
      lines.push_back(text(" STATIC CAST ") | bold | color(Color::Yellow));
      lines.push_back(separatorDouble());
      lines.push_back(paragraph("Compile-time conversion operation. Instructs the compiler to assume safe downcasting without executing runtime validation checks. High speed but dangerous."));
      lines.push_back(separator());
      lines.push_back(text(" SOURCE SNIPPET: ") | bold);
      lines.push_back(text(" F1Team* f1 = static_cast<F1Team*>(base_team_ptr);"));
    } else if (selected_oop_topic == 7) {
      lines.push_back(text(" DYNAMIC CAST ") | bold | color(Color::Yellow));
      lines.push_back(separatorDouble());
      lines.push_back(paragraph("Runtime conversion operation. Queries RTTI flags to ensure safety. Returns nullptr (for pointers) or throws std::bad_cast (for references) on mismatch."));
      lines.push_back(separator());
      lines.push_back(text(" SOURCE SNIPPET: ") | bold);
      lines.push_back(text(" if (auto* f1 = dynamic_cast<F1Team*>(base_ptr)) { f1->getPowerUnitStatus(); }"));
    }
    return window(text(" TECHNICAL CONTEXT ") | bold, vbox(lines)) | flex;
  });

  auto oop_controls = Container::Horizontal({
      oop_menu,
      oop_desc,
  });

  auto oop_renderer = Renderer(oop_controls, [&]() {
    return hbox(
        {vbox({text(" OOP PRINCIPLES ") | bold | color(Color::Blue) | hcenter,
               separatorDouble(), oop_menu->Render() | flex}) |
             size(WIDTH, EQUAL, 25),
         separator(), oop_desc->Render() | flex});
  });

  // --- Tab 6: V-TABLE CASTS (Dynamic Memory Layout Downcasting Inspector) ---
  std::vector<std::unique_ptr<FormulaTeam>> poly_teams;

  poly_teams.push_back(
      std::make_unique<F1Team>("McLaren F1 Team", "Andrea Stella", "Woking, UK",
                               "Mercedes", 135.0, 2, 291, 5, 12));
  poly_teams.push_back(std::make_unique<F1Team>("Ferrari", "Fred Vasseur",
                                                "Maranello, Italy", "Ferrari",
                                                135.0, 3, 249, 3, 8));
  poly_teams.push_back(std::make_unique<F2Team>(
      "Prema Racing", "René Rosin", "Grisignano, Italy", "Dallara F2 2024", 0,
      true, 89, 2, 5));
  poly_teams.push_back(std::make_unique<FormulaETeam>(
      "Jaguar TCS Racing", "James Barclay", "Silverstone, UK", "Castrol", 90,
      38.0, 201, 6, 12));

  std::vector<std::string> poly_team_names;
  for (size_t i = 0; i < poly_teams.size(); ++i) {
    poly_team_names.push_back(poly_teams[i]->getTeamName());
  }

  int selected_poly_idx = 0;
  auto poly_menu = Menu(&poly_team_names, &selected_poly_idx);

  auto poly_info_btn = Button(
      " GET SUMMARY ",
      [&]() {
        addLog("SUMMARY: " + poly_teams[selected_poly_idx]->getSummary());
      },
      ButtonOption::Animated());

  auto poly_static_cast_btn = Button(
      " STATIC CAST ",
      [&]() {
        std::string series = poly_teams[selected_poly_idx]->getSeriesName();
        if (series == "Formula 1") {
          F1Team *f1 = static_cast<F1Team *>(poly_teams[selected_poly_idx].get());
          addLog("[STATIC] F1Team Cast OK | PU: " + f1->getPowerUnitStatus());
        } else if (series == "Formula 2") {
          F2Team *f2 = static_cast<F2Team *>(poly_teams[selected_poly_idx].get());
          addLog("[STATIC] F2Team Cast OK | Feeder: " + f2->getFeederStatus());
        } else {
          FormulaETeam *fe = static_cast<FormulaETeam *>(poly_teams[selected_poly_idx].get());
          addLog("[STATIC] FormulaETeam Cast OK | Battery: " + fe->getBatteryEfficiencyRating());
        }
      },
      ButtonOption::Animated());

  auto poly_dynamic_cast_btn = Button(
      " DYNAMIC CAST ",
      [&]() {
        addLog("--- RUNNING RTTI MEMORY QUERY ---");
        if (auto *f1 = dynamic_cast<F1Team *>(poly_teams[selected_poly_idx].get())) {
          addLog("[DYNAMIC] ✓ SUCCESS: Target resolved as F1Team. PU: " + f1->getPowerUnitStatus());
        } else if (auto *f2 = dynamic_cast<F2Team *>(poly_teams[selected_poly_idx].get())) {
          addLog("[DYNAMIC] ✓ SUCCESS: Target resolved as F2Team. Feeder: " + f2->getFeederStatus());
        } else if (auto *fe = dynamic_cast<FormulaETeam *>(poly_teams[selected_poly_idx].get())) {
          addLog("[DYNAMIC] ✓ SUCCESS: Target resolved as FormulaETeam. Battery: " + fe->getBatteryEfficiencyRating());
        } else {
          addLog("[DYNAMIC] ✗ FAILURE: dynamic_cast returned nullptr!");
        }
      },
      ButtonOption::Animated());

  auto poly_controls = Container::Vertical({
      poly_menu,
      poly_info_btn,
      poly_static_cast_btn,
      poly_dynamic_cast_btn,
  });

  auto poly_renderer = Renderer(poly_controls, [&]() {
    Elements rows;
    rows.push_back(
        text(" RUNTIME POLYMORPHIC POINTER ARRAY (std::vector<unique_ptr>) ") |
        bold | color(Color::Cyan) | hcenter);
    rows.push_back(separatorDouble());

    for (size_t i = 0; i < poly_teams.size(); ++i) {
      auto &t = poly_teams[i];
      std::string series = t->getSeriesName();
      std::string summary = t->getSummary();

      auto entry = hbox({
          text(" [" + std::to_string(i + 1) + "] ") | bold,
          text(series) | color(series == "Formula 1"   ? color_f1
                               : series == "Formula 2" ? color_f2
                                                       : color_fe),
          separator(),
          text(summary) | flex,
      });

      rows.push_back((int)i == selected_poly_idx ? entry | inverted : entry);
    }

    rows.push_back(separator());
    rows.push_back(hbox({
        poly_menu->Render() | size(WIDTH, EQUAL, 40) | frame |
            size(HEIGHT, EQUAL, 4) | vscroll_indicator,
        separator(),
        vbox({
            poly_info_btn->Render(),
            poly_static_cast_btn->Render(),
            poly_dynamic_cast_btn->Render(),
        }),
    }));

    return vbox(rows) | flex;
  });

  // Tab Containers
  auto tab_container = Container::Tab(
      {dashboard_renderer, championship_renderer, simulator_renderer,
       compare_renderer, oop_renderer, poly_renderer},
      &active_tab);

  // Horizontal Header Container containing Menu and Exit Button
  auto header_container = Container::Horizontal({
      tab_menu,
      exit_btn,
  });

  // Render the header horizontally like BTOP monitor top bars
  auto header_renderer = Renderer(header_container, [&]() {
    return hbox({
        text("  BTOP MOTORSPORT MONITOR v1.0.0  ") | bold | color(color_accent) | center,
        separatorDouble(),
        tab_menu->Render() | flex,
        separatorDouble(),
        exit_btn->Render() | center,
    });
  });

  auto main_container = Container::Vertical({
      header_container,
      tab_container,
  });

  // Keyboard navigation helpers using Tab / Shift+Tab to cycle focus
  auto getFocusableComponents = [&](int tab) {
    std::vector<Component> list = {tab_menu, exit_btn};
    if (tab == 2) {
      list.push_back(sim_team_menu);
      list.push_back(sim_driver_menu);
      list.push_back(sim_pos_input);
      list.push_back(submit_btn);
    } else if (tab == 3) {
      list.push_back(compare_selector_a);
      list.push_back(compare_selector_b);
    } else if (tab == 4) {
      list.push_back(oop_menu);
    } else if (tab == 5) {
      list.push_back(poly_menu);
      list.push_back(poly_info_btn);
      list.push_back(poly_static_cast_btn);
      list.push_back(poly_dynamic_cast_btn);
    }
    return list;
  };

  auto findFocusedComponent = [](const std::vector<Component> &list) {
    for (int i = 0; i < (int)list.size(); ++i) {
      if (list[i]->Focused()) {
        return i;
      }
    }
    return -1;
  };

  auto cycle_focus = [&](int direction) {
    auto list = getFocusableComponents(active_tab);
    int curr = findFocusedComponent(list);
    if (curr == -1) {
      list[0]->TakeFocus();
      return;
    }
    int next = (curr + direction + (int)list.size()) % (int)list.size();
    list[next]->TakeFocus();
  };

  // Intercept Tab / Shift+Tab (TabReverse) to perform cycle focus, and numbers 1-6 for tabs
  auto main_container_with_events =
      main_container | CatchEvent([&](Event event) {
        if (event == Event::Tab) {
          cycle_focus(1);
          return true;
        }
        if (event == Event::TabReverse) {
          cycle_focus(-1);
          return true;
        }
        if (event.is_character()) {
          std::string c = event.character();
          if (c >= "1" && c <= "6") {
            active_tab = c[0] - '1';
            return true;
          }
          if (c == "q" || c == "Q") {
            screen.ExitLoopClosure()();
            return true;
          }
        }
        return false;
      });

  auto main_renderer = Renderer(main_container_with_events, [&]() {
    std::vector<Element> log_elements;
    log_elements.push_back(text(" TELEMETRY WRITER JOURNAL ") | bold | color(Color::Cyan));
    log_elements.push_back(separatorDouble());
    if (logs.empty()) {
      log_elements.push_back(text("NO DISPATCH EVENT. COMMENCE A RACE SIMULATION TO GENERATE STATS FEED."));
    } else {
      int start = std::max(0, (int)logs.size() - 4);
      for (int i = start; i < (int)logs.size(); ++i) {
        log_elements.push_back(text(logs[i]) | color(Color::Yellow));
      }
    }

    return vbox({
        header_renderer->Render() | borderDouble | size(HEIGHT, EQUAL, 3) | color(color_accent),
        separator(),
        tab_container->Render() | flex,
        separator(),
        vbox(log_elements) | borderDouble | size(HEIGHT, LESS_THAN, 7) | color(Color::Cyan),
    });
  });

  screen.Loop(main_renderer);
}
