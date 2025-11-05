#include "my_bot_v2.c"
#include <stdio.h>
#include <stdlib.h>

#define OPPONENT_SEQUENCE_LENGTH 100

int OPPONENT_SEQUENCE[OPPONENT_SEQUENCE_LENGTH] = {
    1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2,
    1, 1, 1, 1, 1, 3, 1, 1, 3, 2, 1, 1, 1, 3, 1, 3, 3, 3, 1, 3, 2, 1, 1, 1, 1,
    1, 3, 2, 1, 3, 2, 1, 3, 2, 1, 2, 1, 3, 2, 1, 3, 2, 1, 3, 2, 1, 3, 3, 1, 3,
    1, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3};

int moveIndex = 0;

int getOpponentMove(void) {
  int m = OPPONENT_SEQUENCE[moveIndex];
  moveIndex = (moveIndex + 1) % OPPONENT_SEQUENCE_LENGTH;
  return m;
}

const char *moveToString(int move) {
  switch (move) {
  case ROCK:
    return "К";
  case PAPER:
    return "Б";
  case SCISSORS:
    return "Н";
  default:
    return "?";
  }
}
const char *resultToString(int outcome) {
  switch (outcome) {
  case WIN:
    return "в";
  case DRAW:
    return "н";
  case LOSE:
    return "п";
  default:
    return "?";
  }
}

int main(void) {
  onGameStart();

  printf("| № | МХ | ХС | Рез |\n");
  printf("|----|----|----|----|\n");

  int wins = 0, draws = 0, losses = 0;

  // ТЕСТИРОВАНИЕ НА 50 РАУНДОВ (ПО НОВЫМ ТРЕБОВАНИЯМ)
  int test_rounds = 50;

  for (int round = 1; round <= test_rounds; ++round) {
    int opp = getOpponentMove();
    int bot = choose(opp);
    int res = determineOutcome(bot, opp);

    printf("| %2d | %s | %s | %s |\n", round, moveToString(bot),
           moveToString(opp), resultToString(res));

    if (res == WIN)
      ++wins;
    else if (res == DRAW)
      ++draws;
    else
      ++losses;
  }

  double total = (double)test_rounds;
  printf("\n**Итог (50 раундов)**: %d побед, %d ничьих, %d поражений "
         "(%.1f%% побед, %.1f%% ничьих, %.1f%% поражений)\n",
         wins, draws, losses, wins * 100.0 / total, draws * 100.0 / total,
         losses * 100.0 / total);
  return 0;
}
