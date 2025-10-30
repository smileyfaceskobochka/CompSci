unit my_bot_v2;

interface

uses Math;

const
  ROCK = 1;
  PAPER = 2;
  SCISSORS = 3;

  WIN = 1;
  LOSE = -1;
  DRAW = 0;

  MAX_HISTORY = 1000;
  MAX_PATTERN_LENGTH = 10;
  ANALYSIS_WINDOW = 15;
  MIN_STRATEGY_DATA = 3;
  RECENT_WINDOW = 20;

type
  THistory = record
    opponentMoves: array[0..MAX_HISTORY-1] of integer;
    ourMoves: array[0..MAX_HISTORY-1] of integer;
    outcomes: array[0..MAX_HISTORY-1] of integer;
    length: integer;
  end;

  TStrategyStats = record
    correct: integer;
    total: integer;
  end;

var
  history: THistory;
  patternStats, frequencyStats: TStrategyStats;

function beats(move: integer): integer;
function getCounterMove(predictedMove: integer): integer;
function determineOutcome(ourMove, opponentMove: integer): integer;
function analyzePatterns(patternLength: integer): integer;
function analyzeFrequency(): integer;
function choose(previousOpponentChoice: integer): integer;
procedure onGameStart();
procedure setParameters(setCount, winsPerSet: integer);
procedure onGameEnd();

implementation

var
  current_round: integer;

function beats(move: integer): integer;
begin
  case move of
    ROCK: beats := PAPER;
    PAPER: beats := SCISSORS;
    SCISSORS: beats := ROCK;
    else beats := PAPER;
  end;
end;

function getCounterMove(predictedMove: integer): integer;
begin
  getCounterMove := beats(predictedMove);
end;

function determineOutcome(ourMove, opponentMove: integer): integer;
begin
  if ourMove = opponentMove then
    determineOutcome := DRAW
  else if ((ourMove = ROCK) and (opponentMove = SCISSORS)) or
          ((ourMove = PAPER) and (opponentMove = ROCK)) or
          ((ourMove = SCISSORS) and (opponentMove = PAPER)) then
    determineOutcome := WIN
  else
    determineOutcome := LOSE;
end;

procedure calculateRecentWinrates(var patternWinrate, freqWinrate: real);
var
  patternRecent, patternRecentTotal: integer;
  freqRecent, freqRecentTotal: integer;
begin
  patternRecent := 0;
  patternRecentTotal := 0;
  freqRecent := 0;
  freqRecentTotal := 0;

  if patternStats.total > 0 then
  begin
    patternRecentTotal := Min(patternStats.total, RECENT_WINDOW);
    patternRecent := patternStats.correct;
    if patternStats.total > RECENT_WINDOW then
      patternRecent := Round(patternStats.correct * RECENT_WINDOW / patternStats.total);
  end;

  if frequencyStats.total > 0 then
  begin
    freqRecentTotal := Min(frequencyStats.total, RECENT_WINDOW);
    freqRecent := frequencyStats.correct;
    if frequencyStats.total > RECENT_WINDOW then
      freqRecent := Round(frequencyStats.correct * RECENT_WINDOW / frequencyStats.total);
  end;

  if patternRecentTotal >= MIN_STRATEGY_DATA then
    patternWinrate := patternRecent / patternRecentTotal
  else
    patternWinrate := -1.0;

  if freqRecentTotal >= MIN_STRATEGY_DATA then
    freqWinrate := freqRecent / freqRecentTotal
  else
    freqWinrate := -1.0;
end;

function selectBestPrediction(patternPred, freqPred: integer;
                              patternWinrate, freqWinrate: real): integer;
begin
  if (patternPred <> -1) and (freqPred <> -1) then
  begin
    if patternWinrate > freqWinrate then
      selectBestPrediction := patternPred
    else
      selectBestPrediction := freqPred;
  end
  else if patternPred <> -1 then
    selectBestPrediction := patternPred
  else if freqPred <> -1 then
    selectBestPrediction := freqPred
  else
    selectBestPrediction := -1;
end;

function getFallbackPrediction(patternPred, freqPred: integer): integer;
var
  recentMoves: array[0..2] of integer;
  count, i, start: integer;
begin
  if patternPred <> -1 then
  begin
    getFallbackPrediction := patternPred;
    exit;
  end;
  if freqPred <> -1 then
  begin
    getFallbackPrediction := freqPred;
    exit;
  end;

  if history.length >= 2 then
  begin
    count := 0;
    start := Max(0, history.length - 3);
    for i := start to history.length - 1 do
    begin
      if (history.opponentMoves[i] >= ROCK) and (history.opponentMoves[i] <= SCISSORS) then
      begin
        recentMoves[count] := history.opponentMoves[i];
        Inc(count);
        if count >= 3 then break;
      end;
    end;
    if count > 0 then
    begin
      getFallbackPrediction := getCounterMove(recentMoves[count - 1]);
      exit;
    end;
  end;

  getFallbackPrediction := (history.length mod 3) + 1;
end;

function analyzePatterns(patternLength: integer): integer;
var
  pLen, patternStart, i, j: integer;
  match: boolean;
begin
  if history.length < patternLength + 1 then
  begin
    analyzePatterns := -1;
    exit;
  end;

  for pLen := patternLength downto 2 do
  begin
    if history.length < pLen + 1 then continue;
    patternStart := history.length - pLen;

    for i := history.length - pLen - 1 downto pLen do
    begin
      match := true;
      for j := 0 to pLen - 1 do
      begin
        if history.opponentMoves[i - pLen + j] <> history.opponentMoves[patternStart + j] then
        begin
          match := false;
          break;
        end;
      end;
      if match then
      begin
        analyzePatterns := history.opponentMoves[i];
        exit;
      end;
    end;
  end;

  analyzePatterns := -1;
end;

function analyzeFrequency(): integer;
var
  rockCount, paperCount, scissorsCount: real;
  window, startIdx, i: integer;
begin
  if history.length < 3 then
  begin
    analyzeFrequency := -1;
    exit;
  end;

  rockCount := 0.0;
  paperCount := 0.0;
  scissorsCount := 0.0;

  window := Min(history.length, 20);
  startIdx := history.length - window;

  for i := startIdx to history.length - 1 do
  begin
    case history.opponentMoves[i] of
      ROCK: rockCount := rockCount + (0.5 + (i - startIdx) / window);
      PAPER: paperCount := paperCount + (0.5 + (i - startIdx) / window);
      SCISSORS: scissorsCount := scissorsCount + (0.5 + (i - startIdx) / window);
    end;
  end;

  if (rockCount >= paperCount) and (rockCount >= scissorsCount) then
    analyzeFrequency := ROCK
  else if paperCount >= scissorsCount then
    analyzeFrequency := PAPER
  else
    analyzeFrequency := SCISSORS;
end;

function choose(previousOpponentChoice: integer): integer;
var
  ourMove, prediction, outcome: integer;
  lastMove, sameCount, windowStart, i: integer;
  counts: array[0..2] of integer;
  maxIdx: integer;
  drawCount: integer;
  patternPred, freqPred: integer;
  patternWinrate, freqWinrate: real;
begin
  if previousOpponentChoice = 0 then
  begin
    ourMove := PAPER;
    history.ourMoves[0] := PAPER;
    history.opponentMoves[0] := 0;
    history.length := 1;
    current_round := 1;
    choose := PAPER;
    exit;
  end;

  history.opponentMoves[history.length - 1] := previousOpponentChoice;

  outcome := determineOutcome(history.ourMoves[history.length - 1], previousOpponentChoice);
  history.outcomes[history.length - 1] := outcome;

  // Update stats
  patternPred := analyzePatterns(MAX_PATTERN_LENGTH);
  if patternPred <> -1 then
  begin
    Inc(patternStats.total);
    if patternPred = previousOpponentChoice then Inc(patternStats.correct);
  end;

  freqPred := analyzeFrequency();
  if freqPred <> -1 then
  begin
    Inc(frequencyStats.total);
    if freqPred = previousOpponentChoice then Inc(frequencyStats.correct);
  end;

  // Choose strategy
  patternPred := analyzePatterns(MAX_PATTERN_LENGTH);
  freqPred := analyzeFrequency();
  calculateRecentWinrates(patternWinrate, freqWinrate);

  prediction := selectBestPrediction(patternPred, freqPred, patternWinrate, freqWinrate);

  if prediction = -1 then
    prediction := getFallbackPrediction(patternPred, freqPred);

  ourMove := getCounterMove(prediction);

  // Anti-repetition
  if history.length >= 5 then
  begin
    lastMove := history.ourMoves[history.length - 1];
    sameCount := 1;
    for i := history.length - 2 downto Max(0, history.length - 10) do
    begin
      if history.ourMoves[i] = lastMove then
        Inc(sameCount)
      else
        break;
    end;

    if (sameCount >= 3) and (ourMove = lastMove) then
    begin
      FillChar(counts, SizeOf(counts), 0);
      windowStart := Max(0, history.length - 5);
      for i := windowStart to history.length - 1 do
      begin
        if (history.opponentMoves[i] >= ROCK) and (history.opponentMoves[i] <= SCISSORS) then
          Inc(counts[history.opponentMoves[i] - 1]);
      end;

      maxIdx := 0;
      if counts[1] > counts[maxIdx] then maxIdx := 1;
      if counts[2] > counts[maxIdx] then maxIdx := 2;

      prediction := maxIdx + 1;
      ourMove := getCounterMove(prediction);

      if ourMove = lastMove then
        ourMove := (ourMove mod 3) + 1;
    end;
  end;

  // Anti-draw cycle
  if history.length >= 3 then
  begin
    drawCount := 0;
    for i := history.length - 3 to history.length - 1 do
    begin
      if history.outcomes[i] = DRAW then Inc(drawCount);
    end;

    if drawCount >= 3 then
    begin
      FillChar(counts, SizeOf(counts), 0);
      for i := Max(0, history.length - 8) to history.length - 1 do
      begin
        if (history.opponentMoves[i] >= ROCK) and (history.opponentMoves[i] <= SCISSORS) then
          Inc(counts[history.opponentMoves[i] - 1]);
      end;

      maxIdx := 0;
      if counts[1] > counts[maxIdx] then maxIdx := 1;
      if counts[2] > counts[maxIdx] then maxIdx := 2;

      prediction := maxIdx + 1;
      ourMove := getCounterMove(prediction);
    end;
  end;

  if history.length < MAX_HISTORY then
  begin
    history.ourMoves[history.length] := ourMove;
    history.opponentMoves[history.length] := 0;
    Inc(history.length);
  end;

  Inc(current_round);
  choose := ourMove;
end;

procedure onGameStart();
begin
  history.length := 0;
  patternStats.correct := 0;
  patternStats.total := 0;
  frequencyStats.correct := 0;
  frequencyStats.total := 0;
  current_round := 0;
end;

procedure setParameters(setCount, winsPerSet: integer);
begin
end;

procedure onGameEnd();
begin
end;

end.
