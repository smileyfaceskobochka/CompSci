program test_bot_fixed;

uses bot, Math, SysUtils;

const
  OPPONENT_SEQUENCE_LENGTH = 100;

var
  OPPONENT_SEQUENCE: array[0..OPPONENT_SEQUENCE_LENGTH-1] of integer;

var
  moveIndex: integer;
  wins, draws, losses: integer;
  round: integer;
  opp, bot_move, res: integer;
  total: real;

procedure loadOpponentMoves();
var
  fileHandle: TextFile;
  line: string;
  token: string;
  i, pos: integer;
begin
  Assign(fileHandle, 'opponent_moves.txt');
  Reset(fileHandle);
  ReadLn(fileHandle, line);
  Close(fileHandle);
  i := 0;
  pos := 1;
  while (pos <= Length(line)) and (i < OPPONENT_SEQUENCE_LENGTH) do
  begin
    token := '';
    while (pos <= Length(line)) and (line[pos] <> ',') do
    begin
      token := token + line[pos];
      Inc(pos);
    end;
    Inc(pos); // skip comma
    OPPONENT_SEQUENCE[i] := StrToInt(Trim(token));
    Inc(i);
  end;
end;

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
  loadOpponentMoves();
  onGameStart();
  moveIndex := 0;
  wins := 0;
  draws := 0;
  losses := 0;

  writeln('| № | МХ | ХС | ИР |');
  writeln('|----|----|----|----|');

  // ТЕСТИРОВАНИЕ НА 100 РАУНДОВ
  for round := 1 to 100 do
  begin
    opp := getOpponentMove();
    
    // КРИТИЧЕСКОЕ ИСПРАВЛЕНИЕ: Для первого раунда вызываем choose(0)
    if round = 1 then
      bot_move := choose(0)
    else
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

  total := 100.0;
  writeln();
  writeln('**Итог (100 раундов)**: ', wins, ' побед, ', draws, ' ничьих, ',
          losses, ' поражений (', (wins/total*100):0:1, '% побед, ',
          (draws/total*100):0:1, '% ничьих, ', (losses/total*100):0:1, '% поражений)');
end.
