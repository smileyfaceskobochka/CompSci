#include "TeamFactory.h"
#include "F1Team.h"
#include "F2Team.h"
#include "FormulaETeam.h"

std::vector<std::shared_ptr<FormulaTeam>> createTeams() {
  std::vector<std::shared_ptr<FormulaTeam>> teams;

  // F1 teams (points aligned with DB seeds)
  auto mclaren = std::make_shared<F1Team>("McLaren F1 Team", "Andrea Stella", "Woking, UK",
                               "Mercedes", 135.0, 2, 291, 5, 12);
  mclaren->addDriver(FormulaDriver("Oscar", "Piastri", 81, 168));
  mclaren->addDriver(FormulaDriver("Lando", "Norris", 4, 123));
  teams.push_back(mclaren);

  auto ferrari = std::make_shared<F1Team>("Ferrari", "Fred Vasseur", "Maranello, Italy",
                               "Ferrari", 135.0, 3, 249, 3, 8);
  ferrari->addDriver(FormulaDriver("Charles", "Leclerc", 16, 141));
  ferrari->addDriver(FormulaDriver("Lewis", "Hamilton", 44, 108));
  teams.push_back(ferrari);

  auto redbull =
      std::make_shared<F1Team>("Red Bull Racing", "Christian Horner", "Milton Keynes, UK",
                 "Ford RBPT", 135.0, 1, 206, 12, 15);
  redbull->addDriver(FormulaDriver("Max", "Verstappen", 1, 127));
  redbull->addDriver(FormulaDriver("Isack", "Hadjar", 6, 79));
  teams.push_back(redbull);

  auto mercedes =
      std::make_shared<F1Team>("Mercedes-AMG Petronas", "Toto Wolff", "Brackley, UK",
                 "Mercedes", 135.0, 4, 138, 0, 2);
  mercedes->addDriver(FormulaDriver("George", "Russell", 63, 98));
  mercedes->addDriver(FormulaDriver("Kimi", "Antonelli", 12, 40));
  teams.push_back(mercedes);

  auto aston = std::make_shared<F1Team>("Aston Martin Aramco", "Mike Krack",
                             "Silverstone, UK", "Honda", 135.0, 5, 96, 0, 1);
  aston->addDriver(FormulaDriver("Fernando", "Alonso", 14, 62));
  aston->addDriver(FormulaDriver("Lance", "Stroll", 18, 34));
  teams.push_back(aston);

  auto audi = std::make_shared<F1Team>("Audi F1 Team", "Mattia Binotto",
                            "Hinwil, Switzerland", "Audi", 135.0, 11, 4, 0, 0);
  audi->addDriver(FormulaDriver("Nico", "Hulkenberg", 27, 4));
  audi->addDriver(FormulaDriver("Gabriel", "Bortoleto", 5, 0));
  teams.push_back(audi);

  auto cadillac = std::make_shared<F1Team>("Cadillac Racing", "Michael Andretti",
                                "Fishers, USA", "Cadillac", 135.0, 10, 6, 0, 0);
  cadillac->addDriver(FormulaDriver("Sergio", "Perez", 11, 6));
  cadillac->addDriver(FormulaDriver("Valtteri", "Bottas", 77, 0));
  teams.push_back(cadillac);

  // F2 teams (points aligned with DB seeds)
  auto prema = std::make_shared<F2Team>("Prema Racing", "René Rosin", "Grisignano, Italy",
                             "Dallara F2 2024", 0, true, 89, 2, 5);
  prema->addDriver(FormulaDriver("Paul", "Aron", 7, 56));
  prema->addDriver(FormulaDriver("Ugo", "Ugochukwu", 3, 33));
  teams.push_back(prema);

  auto art = std::make_shared<F2Team>("ART Grand Prix", "Sébastien Philippe",
                           "Villeneuve-la-Garenne, France", "Dallara F2 2024",
                           14, true, 58, 1, 3);
  art->addDriver(FormulaDriver("James", "Wharton", 1, 35));
  art->addDriver(FormulaDriver("Tuukka", "Taponen", 2, 23));
  teams.push_back(art);

  // Formula E teams (points aligned with DB seeds)
  auto jaguar =
      std::make_shared<FormulaETeam>("Jaguar TCS Racing", "James Barclay", "Silverstone, UK",
                       "Castrol", 90, 38.0, 201, 6, 12);
  jaguar->addDriver(FormulaDriver("Nick", "Cassidy", 37, 118));
  jaguar->addDriver(FormulaDriver("Mitch", "Evans", 9, 83));
  teams.push_back(jaguar);

  auto nissan =
      std::make_shared<FormulaETeam>("Nissan Formula E Team", "Tommaso Volpe",
                       "Yokohama, Japan", "Nissan Energy", 88, 38.0, 98, 1, 4);
  nissan->addDriver(FormulaDriver("Oliver", "Rowland", 22, 62));
  nissan->addDriver(FormulaDriver("Norman", "Nato", 17, 36));
  teams.push_back(nissan);

  return teams;
}
