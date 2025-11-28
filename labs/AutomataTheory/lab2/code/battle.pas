program battle;

uses bot, dumb_bot_unit, Math, SysUtils;

// Используем константы из модуля bot
const
  WIN = 1;                     // Победа
  LOSE = -1;                   // Поражение
  DRAW = 0;                    // Ничья

var
  round: integer;
  bot_move, dumb_move: integer;
  res_bot, res_dumb: integer;
  bot_wins, bot_draws, bot_losses: integer;
  dumb_wins, dumb_draws, dumb_losses: integer;

function moveToString(move: integer): string;
begin
  case move of
    bot.ROCK: moveToString := 'К';
    bot.PAPER: moveToString := 'Б';
    bot.SCISSORS: moveToString := 'Н';
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

// Determine outcome for bot
function determineOutcome(myMove, opponentMove: integer): integer;
begin
  if myMove = opponentMove then
    determineOutcome := DRAW
  else if ((myMove = bot.ROCK) and (opponentMove = bot.SCISSORS)) or
          ((myMove = bot.PAPER) and (opponentMove = bot.ROCK)) or
          ((myMove = bot.SCISSORS) and (opponentMove = bot.PAPER)) then
    determineOutcome := WIN
  else
    determineOutcome := LOSE;
end;

begin
  // Initialize both bots
  onGameStart();
  dumb_bot_unit.onGameStart();

  bot_wins := 0;
  bot_draws := 0;
  bot_losses := 0;
  dumb_wins := 0;
  dumb_draws := 0;
  dumb_losses := 0;

  writeln('| № | Bot ход | Dumb ход | Bot результат | Dumb результат |');
  writeln('|----|---------|----------|---------------|----------------|');

  // Simulate 100 rounds
  for round := 1 to 100 do
  begin
    if round = 1 then
    begin
      bot_move := choose(0);
      dumb_move := dumb_bot_unit.choose(0);
    end
    else
    begin
      bot_move := choose(dumb_move);
      dumb_move := dumb_bot_unit.choose(bot_move);
    end;

    // Determine outcomes
    res_bot := determineOutcome(bot_move, dumb_move);
    res_dumb := determineOutcome(dumb_move, bot_move);

    case res_bot of
      WIN: Inc(bot_wins);
      DRAW: Inc(bot_draws);
      LOSE: Inc(bot_losses);
    end;

    case res_dumb of
      WIN: Inc(dumb_wins);
      DRAW: Inc(dumb_draws);
      LOSE: Inc(dumb_losses);
    end;

    writeln('| ', round:2, ' | ', moveToString(bot_move), '    | ',
            moveToString(dumb_move), '     | ', resultToString(res_bot),
            '            | ', resultToString(res_dumb), '             |');
  end;

  writeln();
  writeln('Bot statistics: ', bot_wins, ' wins, ', bot_draws, ' draws, ',
          bot_losses, ' losses (', (bot_wins/100*100):0:1, '% wins, ',
          (bot_draws/100*100):0:1, '% draws, ', (bot_losses/100*100):0:1, '% losses)');
  writeln('Dumb bot statistics: ', dumb_wins, ' wins, ', dumb_draws, ' draws, ',
          dumb_losses, ' losses (', (dumb_wins/100*100):0:1, '% wins, ',
          (dumb_draws/100*100):0:1, '% draws, ', (dumb_losses/100*100):0:1, '% losses)');
end.
