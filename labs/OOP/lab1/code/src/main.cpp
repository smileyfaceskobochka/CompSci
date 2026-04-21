#include <algorithm>
#include <iomanip>
#include <iostream>
#include <limits>
#include <string>
#include <vector>

#include "core/F1Team.h"
#include "core/F2Team.h"
#include "core/FormulaETeam.h"
#include "core/FormulaTeam.h"

void clearInput() {
  std::cin.clear();
  std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
}

void pause() {
  std::cout << "\nPress Enter to continue...";
  std::cin.get();
}

void printHeader(const std::string &title) {
  std::cout << "\n\033[1;36m"
            << "═══════════════════════════════════════════════\n"
            << "  " << title << "\n"
            << "═══════════════════════════════════════════════\033[0m\n\n";
}

// Team initialization

std::vector<FormulaTeam *> createTeams() {
  std::vector<FormulaTeam *> teams;

  // F1 teams
  F1Team *mercedes = new F1Team("Mercedes-AMG Petronas", "Toto Wolff",
                                "Brackley, UK", "Mercedes");
  mercedes->addDriver(Driver("George", "Russell", 63, 110));
  mercedes->addDriver(Driver("Kimi", "Antonelli", 12, 15));
  teams.push_back(mercedes);

  F1Team *ferrari =
      new F1Team("Scuderia Ferrari", "Fred Vasseur", "Maranello, Italy",
                 "Ferrari", 135.0, 3, 195, 8, 24);
  ferrari->addDriver(Driver("Lewis", "Hamilton", 44, 185));
  ferrari->addDriver(Driver("Charles", "Leclerc", 16, 170));
  teams.push_back(ferrari);

  F1Team *redbull =
      new F1Team("Red Bull Racing", "Christian Horner", "Milton Keynes, UK",
                 "Ford RBPT", 135.0, 1, 860, 21, 41);
  redbull->addDriver(Driver("Max", "Verstappen", 1, 395));
  redbull->addDriver(Driver("Isack", "Hadjar", 20, 60));
  teams.push_back(redbull);

  F1Team *mclaren = new F1Team("McLaren F1 Team", "Andrea Stella", "Woking, UK",
                               "Mercedes", 135.0, 2, 666, 6, 20);
  mclaren->addDriver(Driver("Lando", "Norris", 4, 305));
  mclaren->addDriver(Driver("Oscar", "Piastri", 81, 260));
  teams.push_back(mclaren);

  F1Team *aston = new F1Team("Aston Martin Aramco", "Mike Krack",
                             "Silverstone, UK", "Honda");
  aston->addDriver(Driver("Fernando", "Alonso", 14, 95));
  aston->addDriver(Driver("Lance", "Stroll", 18, 48));
  teams.push_back(aston);

  F1Team *audi = new F1Team("Audi F1 Team", "Mattia Binotto",
                            "Hinwil, Switzerland", "Audi");
  audi->addDriver(Driver("Nico", "Hulkenberg", 27, 34));
  audi->addDriver(Driver("Gabriel", "Bortoleto", 5, 5));
  teams.push_back(audi);

  F1Team *cadillac = new F1Team("Cadillac Racing", "Michael Andretti",
                                "Fishers, USA", "Cadillac");
  cadillac->addDriver(Driver("Sergio", "Perez", 11, 82));
  cadillac->addDriver(Driver("Valtteri", "Bottas", 77, 12));
  teams.push_back(cadillac);

  // F2 teams
  F2Team *prema = new F2Team("Prema Racing", "René Rosin", "Grisignano, Italy",
                             "Dallara F2 2024");
  prema->addDriver(Driver("Ugo", "Ugochukwu", 3, 45));
  prema->addDriver(Driver("Arvid", "Lindblad", 4, 62));
  teams.push_back(prema);

  F2Team *art = new F2Team("ART Grand Prix", "Sébastien Philippe",
                           "Villeneuve-la-Garenne, France", "Dallara F2 2024",
                           14, true, 340, 9, 22);
  art->addDriver(Driver("James", "Wharton", 1, 30));
  art->addDriver(Driver("Tuukka", "Taponen", 2, 28));
  teams.push_back(art);

  // Formula E teams
  FormulaETeam *jaguar = new FormulaETeam("Jaguar TCS Racing", "James Barclay",
                                          "Silverstone, UK", "Castrol");
  jaguar->addDriver(Driver("Mitch", "Evans", 9, 155));
  jaguar->addDriver(Driver("Nick", "Cassidy", 37, 142));
  teams.push_back(jaguar);

  FormulaETeam *nissan = new FormulaETeam(
      "Nissan Formula E Team", "Tommaso Volpe", "Yokohama, Japan",
      "Nissan Energy", 88, 38.0, 210, 5, 14);
  nissan->addDriver(Driver("Oliver", "Rowland", 22, 110));
  nissan->addDriver(Driver("Norman", "Nato", 17, 45));
  teams.push_back(nissan);

  return teams;
}

void displayAllTeams(const std::vector<FormulaTeam *> &teams) {
  printHeader("ALL TEAMS");
  for (const auto *t : teams) {
    t->displayInfo();
    std::cout << "\n";
  }
}

void simulateRace(std::vector<FormulaTeam *> &teams) {
  printHeader("SIMULATE RACE RESULTS");
  std::cout << "Teams:\n";
  for (size_t i = 0; i < teams.size(); ++i)
    std::cout << "  " << (i + 1) << ". " << teams[i]->getTeamName() << " ["
              << teams[i]->getSeriesName() << "]\n";

  std::cout << "\nEnter team number (1-" << teams.size() << "): ";
  int idx;
  std::cin >> idx;
  if (idx < 1 || idx > static_cast<int>(teams.size())) {
    std::cout << "Invalid index.\n";
    clearInput();
    return;
  }
  FormulaTeam *team = teams[idx - 1];

  std::cout << "Enter finish position (1-20): ";
  int pos;
  std::cin >> pos;
  clearInput();

  int pts = team->calculatePoints(pos);
  *team += pts;
  if (pos == 1)
    ++(*team);
  else if (pos <= 3)
    team->addPodium();

  std::cout << "\nyes " << team->getTeamName() << " P" << pos << " -> +" << pts
            << " pts"
            << " | Total: " << team->getChampionshipPoints() << "\n";
}

void showStandings(const std::vector<FormulaTeam *> &teams) {
  printHeader("CHAMPIONSHIP STANDINGS");

  // Sort by points
  std::vector<FormulaTeam *> sorted = teams;
  std::sort(sorted.begin(), sorted.end(),
            [](const FormulaTeam *a, const FormulaTeam *b) { return *a > *b; });

  std::cout << std::left << std::setw(4) << "Pos" << std::setw(30) << "Team"
            << std::setw(12) << "Series"
            << "Points\n"
            << std::string(58, '-') << "\n";

  int pos = 1;
  for (const auto *t : sorted) {
    std::cout << std::setw(4) << pos++ << std::setw(30) << t->getTeamName()
              << std::setw(12) << t->getSeriesName()
              << t->getChampionshipPoints() << "\n";
  }
}

void showDriverStandings(const std::vector<FormulaTeam *> &teams) {
  printHeader("DRIVERS CHAMPIONSHIP STANDINGS");

  std::vector<std::pair<Driver, std::string>> allDrivers;
  for (const auto *t : teams) {
    for (const auto &d : t->getDrivers()) {
      allDrivers.push_back({d, t->getTeamName()});
    }
  }

  std::sort(allDrivers.begin(), allDrivers.end(),
            [](const std::pair<Driver, std::string> &a,
               const std::pair<Driver, std::string> &b) {
              return a.first.getPoints() > b.first.getPoints();
            });

  std::cout << std::left << std::setw(4) << "Pos" << std::setw(30) << "Driver"
            << std::setw(30) << "Team"
            << "Points\n"
            << std::string(70, '-') << "\n";

  int pos = 1;
  for (const auto &pair : allDrivers) {
    std::cout << std::setw(4) << pos++ << std::setw(30)
              << pair.first.getFullName() + " #" +
                     std::to_string(pair.first.getRacingNumber())
              << std::setw(30) << pair.second << pair.first.getPoints() << "\n";
  }
}

void compareTeams(const std::vector<FormulaTeam *> &teams) {
  printHeader("COMPARE TWO TEAMS");
  for (size_t i = 0; i < teams.size(); ++i)
    std::cout << "  " << (i + 1) << ". " << teams[i]->getTeamName() << "\n";

  std::cout << "\nTeam A number: ";
  int a;
  std::cin >> a;
  std::cout << "Team B number: ";
  int b;
  std::cin >> b;
  clearInput();

  if (a < 1 || b < 1 || a > (int)teams.size() || b > (int)teams.size()) {
    std::cout << "Invalid index.\n";
    return;
  }

  FormulaTeam *ta = teams[a - 1];
  FormulaTeam *tb = teams[b - 1];

  std::cout << "\n";
  std::cout << *ta << "\n\n";
  std::cout << *tb << "\n\n";

  if (*ta == *tb)
    std::cout << "-> Same team!\n";
  else if (*ta > *tb)
    std::cout << "-> " << ta->getTeamName() << " leads the standings.\n";
  else
    std::cout << "-> " << tb->getTeamName() << " leads the standings.\n";
}

void demonstrateOOP(const std::vector<FormulaTeam *> &teams) {
  printHeader("OOP PRINCIPLES DEMONSTRATION");

  // 1. Polymorphism
  std::cout << "\033[1;33m1. POLYMORPHISM\033[0m\n";
  std::cout << "Calling getSummary() through vector<FormulaTeam*>:\n\n";
  for (const auto *t : teams)
    std::cout << "  " << t->getSummary() << "\n";

  // 2. Encapsulation
  std::cout << "\n\033[1;33m2. ENCAPSULATION\033[0m\n";
  std::cout << "Data is private/protected — access only via getters/setters:\n";
  FormulaTeam *demo = teams[2]; // Red Bull
  std::cout << "  getTeamName()           -> " << demo->getTeamName() << "\n";
  std::cout << "  getChampionshipPoints() -> " << demo->getChampionshipPoints()
            << "\n";
  std::cout << "  getRaceWins()           -> " << demo->getRaceWins() << "\n";
  std::cout << "  getWinRate(23 races)    -> " << std::fixed
            << std::setprecision(1) << demo->getWinRate(23) << "%\n";

  // 3. Inheritance + dynamic_cast
  std::cout << "\n\033[1;33m3. INHERITANCE + dynamic_cast\033[0m\n";
  std::cout << "Find first team of each type via dynamic_cast:\n\n";

  F1Team *foundF1 = nullptr;
  F2Team *foundF2 = nullptr;
  FormulaETeam *foundFE = nullptr;

  for (auto *t : teams) {
    if (!foundF1)
      foundF1 = dynamic_cast<F1Team *>(t);
    if (!foundF2)
      foundF2 = dynamic_cast<F2Team *>(t);
    if (!foundFE)
      foundFE = dynamic_cast<FormulaETeam *>(t);
  }

  if (foundF1) {
    std::cout << "  [F1Team] \"" << foundF1->getTeamName() << "\"\n";
    std::cout << "    getPowerUnitStatus()      -> "
              << foundF1->getPowerUnitStatus() << "\n";
    std::cout << "    calculateBudgetAlloc(30%) -> $" << std::fixed
              << std::setprecision(1) << foundF1->calculateBudgetAllocation(30)
              << "M\n";
  }
  if (foundF2) {
    std::cout << "  [F2Team] \"" << foundF2->getTeamName() << "\"\n";
    std::cout << "    getFeederStatus()  -> " << foundF2->getFeederStatus()
              << "\n";
    std::cout << "    getF1Graduates()   -> " << foundF2->getF1Graduates()
              << "\n";
  }
  if (foundFE) {
    std::cout << "  [FormulaETeam] \"" << foundFE->getTeamName() << "\"\n";
    std::cout << "    getBatteryEfficiencyRating() -> "
              << foundFE->getBatteryEfficiencyRating() << "\n";
    std::cout << "    getSustainabilityScore()     -> "
              << foundFE->getSustainabilityScore() << "\n";
  }

  // 4. Operator overloading
  std::cout << "\n\033[1;33m4. OPERATOR OVERLOADING\033[0m\n";

  F1Team tmpA("Alpha Team", "Alice", "London, UK", "Mercedes", 100.0, 5, 50, 2,
              5);
  F1Team tmpB("Beta Team", "Bob", "Paris, France", "Ferrari", 100.0, 6, 30, 1,
              3);

  std::cout << "  Alpha pts before +=25: " << tmpA.getChampionshipPoints()
            << "\n";
  tmpA += 25;
  std::cout << "  Alpha pts after  +=25: " << tmpA.getChampionshipPoints()
            << "\n";

  std::cout << "  Alpha wins before ++: " << tmpA.getRaceWins() << "\n";
  ++tmpA;
  std::cout << "  Alpha wins after  ++: " << tmpA.getRaceWins() << "\n";

  std::cout << "  Alpha > Beta ? " << (tmpA > tmpB ? "Yes" : "No") << "\n";
  std::cout << "  Alpha == Alpha? " << (tmpA == tmpA ? "Yes" : "No") << "\n";
  std::cout << "  operator<< output:\n" << tmpA << "\n";

  // 5. Abstract class enforcement
  std::cout << "\n\033[1;33m5. ABSTRACT BASE CLASS\033[0m\n";
  std::cout
      << "  FormulaTeam is abstract — 'new FormulaTeam()' would not compile.\n";
  std::cout << "  Pure virtual methods enforced: displayInfo(), "
               "getSeriesName(), calculatePoints()\n";
  std::cout << "  All 3 derived classes implement them -> compile succeeds +\n";

  // 6. Demonstrate constructors (F1Team example)
  std::cout << "\n\033[1;33m6. CONSTRUCTORS\033[0m\n";
  F1Team c1;
  F1Team c2("Test Team", "Test Principal", "Test HQ", "BMW");
  F1Team c3("Full Team", "Full Principal", "Full HQ", "Audi", 120.0, 4, 88, 3,
            9);

  std::cout << "  Default ctor  -> name: \"" << c1.getTeamName()
            << "\", pts: " << c1.getChampionshipPoints() << "\n";
  std::cout << "  4-arg ctor    -> name: \"" << c2.getTeamName()
            << "\", PU:  " << c2.getPowerUnit() << "\n";
  std::cout << "  Full ctor     -> name: \"" << c3.getTeamName()
            << "\", pts: " << c3.getChampionshipPoints()
            << ", wins: " << c3.getRaceWins() << "\n";
}

void showSummaries(const std::vector<FormulaTeam *> &teams) {
  printHeader("TEAM SUMMARIES");
  for (const auto *t : teams)
    std::cout << t->getSummary() << "\n";
}

// Main

int main() {
  std::cout << "\033[1;35m"
            << "╔═══════════════════════════════════════════════╗\n"
            << "║   FORMULA RACING MANAGEMENT SYSTEM v1.0       ║\n"
            << "║   OOP Demonstration - C++ Assignment          ║\n"
            << "╚═══════════════════════════════════════════════╝\n"
            << "\033[0m\n";

  std::vector<FormulaTeam *> teams = createTeams();
  std::cout << "+ Initialized " << teams.size()
            << " teams across F1 / F2 / Formula E\n";

  int choice = -1;
  while (choice != 0) {
    std::cout << "\n\033[1m"
              << "─── MAIN MENU ───────────────────────────────────\n"
              << "  1. Display All Teams\n"
              << "  2. Simulate Race Result\n"
              << "  3. Constructors Championship\n"
              << "  4. Compare Two Teams\n"
              << "  5. Demonstrate OOP Principles\n"
              << "  6. Team Summaries\n"
              << "  7. Drivers Championship\n"
              << "  0. Exit\n"
              << "─────────────────────────────────────────────────\n"
              << "\033[0m"
              << "Enter choice: ";
    std::cin >> choice;
    clearInput();

    switch (choice) {
    case 1:
      displayAllTeams(teams);
      break;
    case 2:
      simulateRace(teams);
      break;
    case 3:
      showStandings(teams);
      break;
    case 4:
      compareTeams(teams);
      break;
    case 5:
      demonstrateOOP(teams);
      break;
    case 6:
      showSummaries(teams);
      break;
    case 7:
      showDriverStandings(teams);
      break;
    case 0:
      break;
    default:
      std::cout << "Unknown option.\n";
    }
    if (choice != 0)
      pause();
  }

  // Cleanup
  for (auto *t : teams)
    delete t;

  std::cout << "\nFinal team count: " << teams.size() << ". Goodbye!\n";
  return 0;
}
