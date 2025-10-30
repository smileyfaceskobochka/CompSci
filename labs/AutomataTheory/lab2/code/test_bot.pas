program test_bot;

uses my_bot_v2;

const
  ROCK = 1;
  PAPER = 2;
  SCISSORS = 3;

var
  OPPONENT_SEQUENCE: array[1..25] of integer;
  moveIndex: integer = 1;

function getOpponentMove(): integer;
begin
  getOpponentMove := OPPONENT_SEQUENCE[moveIndex];
  Inc(moveIndex);
  if moveIndex > 25 then moveIndex := 1;
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
    1: resultToString := 'в';  // выиграл
    0: resultToString := 'н';  // ничья
    -1: resultToString := 'п'; // проиграл
    else resultToString := '?';
  end;
end;

function determineOutcome(ourMove, opponentMove: integer): integer;
begin
  if ourMove = opponentMove then
    determineOutcome := 0  // ничья
  else if ((ourMove = ROCK) and (opponentMove = SCISSORS)) or
          ((ourMove = PAPER) and (opponentMove = ROCK)) or
          ((ourMove = SCISSORS) and (opponentMove = PAPER)) then
    determineOutcome := 1  // выиграл
  else
    determineOutcome := -1; // проиграл
end;

var
  round: integer;
  botMove, opponentMove, outcome: integer;
  totalRounds: integer = 25;

begin
  Randomize;

  // Initialize opponent sequence
  OPPONENT_SEQUENCE[1] := 1; OPPONENT_SEQUENCE[2] := 2; OPPONENT_SEQUENCE[3] := 3; OPPONENT_SEQUENCE[4] := 1;
  OPPONENT_SEQUENCE[5] := 2; OPPONENT_SEQUENCE[6] := 3; OPPONENT_SEQUENCE[7] := 1; OPPONENT_SEQUENCE[8] := 2;
  OPPONENT_SEQUENCE[9] := 3; OPPONENT_SEQUENCE[10] := 1; OPPONENT_SEQUENCE[11] := 1; OPPONENT_SEQUENCE[12] := 1;
  OPPONENT_SEQUENCE[13] := 2; OPPONENT_SEQUENCE[14] := 2; OPPONENT_SEQUENCE[15] := 3; OPPONENT_SEQUENCE[16] := 3;
  OPPONENT_SEQUENCE[17] := 1; OPPONENT_SEQUENCE[18] := 1; OPPONENT_SEQUENCE[19] := 2; OPPONENT_SEQUENCE[20] := 2;
  OPPONENT_SEQUENCE[21] := 3; OPPONENT_SEQUENCE[22] := 1; OPPONENT_SEQUENCE[23] := 2; OPPONENT_SEQUENCE[24] := 3;
  OPPONENT_SEQUENCE[25] := 1;

  my_bot_v2.onGameStart();

  Writeln('| № | МХ | ХС | Рез |');
  Writeln('|---|---|----|-----|');

  for round := 1 to totalRounds do
  begin
    opponentMove := getOpponentMove();
    botMove := my_bot_v2.choose(opponentMove);
    outcome := determineOutcome(botMove, opponentMove);

    Writeln('| ', round:2, ' | ', moveToString(botMove), ' | ', moveToString(opponentMove), ' | ', resultToString(outcome), ' |');
  end;

  my_bot_v2.onGameEnd();
end.
