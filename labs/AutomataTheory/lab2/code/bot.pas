unit bot;

interface

uses
  Math;

const
  ROCK     = 1;
  PAPER    = 2;
  SCISSORS = 3;

  WIN  = 1;
  DRAW = 0;
  LOSE = -1;

  MAX_HISTORY = 1000;

  BASE_PATTERN_LEN = 10;
  BASE_WINDOW      = 80;

  LOSE_STREAK_THRESHOLD = 5;

var
  TournamentSetCount   : integer = 0;
  TournamentWinsPerSet : integer = 0;

  history: record
    oppMove : array[0..MAX_HISTORY-1] of integer;
    myMove  : array[0..MAX_HISTORY-1] of integer;
    outcome : array[0..MAX_HISTORY-1] of integer;
    len     : integer;
  end;

  loseStreak      : integer = 0;
  currentTactic   : integer = 1;  // 1 = паттерны, 2 = частоты
  patternLen      : integer = BASE_PATTERN_LEN;
  analysisWindow  : integer = BASE_WINDOW;
  adaptationLevel : double  = 0.6;

procedure setParameters(setCount, winsPerSet: integer);
function  choose(previousOpponentChoice: integer): integer;
procedure onGameStart();
procedure onGameEnd();

implementation

// Вспомогательные функции
function beats(move: integer): integer;
begin
  if move = ROCK     then beats := PAPER else
  if move = PAPER    then beats := SCISSORS else
  if move = SCISSORS then beats := ROCK
  else beats := PAPER;
end;

function getCounterMove(predicted: integer): integer;
begin
  if predicted <= 0 then Exit(PAPER);
  getCounterMove := beats(predicted);
end;

function getOutcome(my, opp: integer): integer;
begin
  if my = opp then Exit(DRAW);
  if ((my = ROCK) and (opp = SCISSORS)) or
     ((my = PAPER) and (opp = ROCK)) or
     ((my = SCISSORS) and (opp = PAPER)) then
    getOutcome := WIN
  else
    getOutcome := LOSE;
end;

// Замена Min/Max для integer
function myMin(a, b: integer): integer;
begin
  if a < b then myMin := a else myMin := b;
end;

function myMax(a, b: integer): integer;
begin
  if a > b then myMax := a else myMax := b;
end;

// Частотный анализ
function analyzeFrequency(): integer;
var
  r, p, s: double;
  i, startPos, wnd: integer;
  weight, ageFactor: double;
begin
  if history.len < 3 then Exit(-1);

  r := 0.01; p := 0.01; s := 0.01;
  wnd := myMin(history.len - 1, analysisWindow);
  startPos := history.len - wnd;

  for i := startPos to history.len - 2 do
  begin
    ageFactor := (i - startPos + 1) / wnd;
    weight := 0.2 + ageFactor * 0.8;
    weight := weight * (1.0 + adaptationLevel);

    if history.outcome[i] = LOSE then weight := weight * 2.0;

    case history.oppMove[i] of
      ROCK:     r := r + weight;
      PAPER:    p := p + weight;
      SCISSORS: s := s + weight;
    end;
  end;

  if (r >= p) and (r >= s) then analyzeFrequency := ROCK
  else if (p >= r) and (p >= s) then analyzeFrequency := PAPER
  else analyzeFrequency := SCISSORS;
end;

// Поиск паттернов
function analyzePatterns(): integer;
var
  len, i, j, curStart: integer;
  bestScore, score: double;
  bestNext : integer;
  match    : boolean;
  nextMove : integer;
begin
  if history.len < 5 then Exit(-1);

  bestScore := -1e9;
  bestNext  := -1;

  for len := myMin(patternLen, history.len - 2) downto 2 do
  begin
    curStart := history.len - len;

    for i := 0 to history.len - len - 1 do
    begin
      match := True;
      for j := 0 to len-1 do
        if history.oppMove[i + j] <> history.oppMove[curStart + j] then
        begin
          match := False;
          break;
        end;

      if match and (i + len < history.len) then
      begin
        nextMove := history.oppMove[i + len];
        score := len * (2.0 + adaptationLevel);

        if history.outcome[i + len] = WIN  then score := score + 10 else
        if history.outcome[i + len] = DRAW then score := score + 3
        else score := score - 6;

        if score > bestScore then
        begin
          bestScore := score;
          bestNext  := nextMove;
        end;
      end;
    end;

    if bestScore >= 12 then Exit(bestNext);
  end;

  if bestNext > 0 then analyzePatterns := bestNext
  else analyzePatterns := -1;
end;

// Адаптация по истории
procedure adaptParameters();
var
  wins, i: integer;
  winRate: double;
begin
  wins := 0;
  if history.len = 0 then Exit;

  for i := 0 to history.len - 1 do
    if history.outcome[i] = WIN then Inc(wins);

  winRate := wins / history.len;

  if winRate > 0.60 then
  begin
    adaptationLevel := adaptationLevel + 0.04;
    if adaptationLevel > 1.0 then adaptationLevel := 1.0;
    patternLen := patternLen + 1;
    if patternLen > 14 then patternLen := 14;
    analysisWindow := analysisWindow + 8;
    if analysisWindow > 120 then analysisWindow := 120;
  end
  else if winRate < 0.45 then
  begin
    adaptationLevel := adaptationLevel - 0.06;
    if adaptationLevel < 0.4 then adaptationLevel := 0.4;
    patternLen := patternLen - 1;
    if patternLen < 6 then patternLen := 6;
    analysisWindow := analysisWindow - 10;
    if analysisWindow < 50 then analysisWindow := 50;
  end;

  if history.len > 150 then
    if adaptationLevel > 0.5 then adaptationLevel := adaptationLevel - 0.02;
end;

procedure setParameters(setCount, winsPerSet: integer);
begin
  TournamentSetCount   := setCount;
  TournamentWinsPerSet := winsPerSet;
end;

procedure onGameStart();
begin
  FillChar(history, SizeOf(history), 0);
  history.len := 0;

  loseStreak      := 0;
  currentTactic   := 1;
  patternLen      := BASE_PATTERN_LEN;
  analysisWindow  := BASE_WINDOW;
  adaptationLevel := 0.6;
end;

function choose(previousOpponentChoice: integer): integer;
var
  pred, myMove: integer;
begin
  if previousOpponentChoice = 0 then
  begin
    onGameStart();
    myMove := PAPER;
    history.myMove[0]  := myMove;
    history.oppMove[0] := 0;
    history.outcome[0] := DRAW;
    history.len := 1;
    choose := myMove;
    Exit;
  end;

  // Записываем ход противника и результат
  history.oppMove[history.len - 1] := previousOpponentChoice;
  history.outcome[history.len - 1] := getOutcome(history.myMove[history.len - 1], previousOpponentChoice);

  // Смена тактики
  if history.outcome[history.len - 1] = LOSE then
  begin
    Inc(loseStreak);
    if loseStreak >= LOSE_STREAK_THRESHOLD then
    begin
      if currentTactic = 1 then currentTactic := 2 else currentTactic := 1;
      loseStreak := 0;
    end;
  end
  else
    loseStreak := 0;

  adaptParameters();

  // Предсказание
  if currentTactic = 1 then
    pred := analyzePatterns()
  else
    pred := analyzeFrequency();

  if pred <= 0 then pred := PAPER;

  myMove := getCounterMove(pred);

  if history.len < MAX_HISTORY then
  begin
    history.myMove[history.len] := myMove;
    Inc(history.len);
  end;

  choose := myMove;
end;

procedure onGameEnd();
begin
  // ничего
end;

end.