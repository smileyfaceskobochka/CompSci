program test_bot_pas;

uses bot, Math;

const
  OPPONENT_SEQUENCE_LENGTH = 100;

var
  OPPONENT_SEQUENCE: array[0..OPPONENT_SEQUENCE_LENGTH-1] of integer = (
    1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2,
    1, 1, 1, 1, 1, 3, 1, 1, 3, 2, 1, 1, 1, 3, 1, 3, 3, 3, 1, 3, 2, 1, 1, 1, 1,
    1, 3, 2, 1, 3, 2, 1, 3, 2, 1, 2, 1, 3, 2, 1, 3, 2, 1, 3, 2, 1, 3, 3, 1, 3,
    1, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3
  );

var
  moveIndex: integer;
  wins, draws, losses: integer;
  round: integer;
  opp, bot_move, res: integer;
  total: real;

function getOpponentMove(): integer;
begin
  getOpponentMove := OPPONENT_SEQUENCE[moveIndex];
  moveIndex := (moveIndex + 1) mod OPPONENT_SEQUENCE_LENGTH;
end;

function moveToString(move: integer): string;
begin
  case move of
    ROCK: moveToString := 'К';
    PAPER: moveToString := 'Б';
    SCISSORS: moveToString := 'Н';
    else moveToString := '?';
  end;
end;

function resultToString(outcome: integer): string;
begin
  case outcome of
    WIN: resultToString := 'в';
    DRAW: resultToString := 'н';
    LOSE: resultToString := 'п';
    else resultToString := '?';
  end;
end;

begin
  onGameStart();
  moveIndex := 0;
  wins := 0;
  draws := 0;
  losses := 0;

  writeln('| № | МХ | ХС | Рез |');
  writeln('|----|----|----|----|');

  // ТЕСТИРОВАНИЕ НА 50 РАУНДОВ (ПО НОВЫМ ТРЕБОВАНИЯМ)
  for round := 1 to 50 do
  begin
    opp := getOpponentMove();
    bot_move := choose(opp);
    res := determineOutcome(bot_move, opp);

    writeln('| ', round:2, ' | ', moveToString(bot_move), ' | ',
            moveToString(opp), ' | ', resultToString(res), ' |');

    case res of
      WIN: Inc(wins);
      DRAW: Inc(draws);
      LOSE: Inc(losses);
    end;
  end;

  total := 50.0;
  writeln();
  writeln('**Итог (50 раундов)**: ', wins, ' побед, ', draws, ' ничьих, ',
          losses, ' поражений (', (wins/total*100):0:1, '% побед, ',
          (draws/total*100):0:1, '% ничьих, ', (losses/total*100):0:1, '% поражений)');
end.
