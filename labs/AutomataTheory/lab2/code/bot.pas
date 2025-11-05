unit bot;

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
  LOSE_STREAK_THRESHOLD = 5;
  SEQ: array[0..9] of integer = (SCISSORS, SCISSORS, PAPER, ROCK, SCISSORS, ROCK, PAPER, SCISSORS, PAPER, PAPER);

type
  THistory = record
    opponentMoves: array[0..MAX_HISTORY-1] of integer;
    ourMoves: array[0..MAX_HISTORY-1] of integer;
    outcomes: array[0..MAX_HISTORY-1] of integer;
    length: integer;
  end;

var
  history: THistory;
  current_round: integer;
  current_lose_streak: integer;
  current_strategy: integer;
  sequence_idx: integer;

function beats(move: integer): integer;
function getCounterMove(predictedMove: integer): integer;
function cycleMove(move: integer): integer;
function determineOutcome(ourMove, opponentMove: integer): integer;
function analyzeInteractionPatterns(maxLen: integer): integer;
function analyzeOutcomeReaction(): integer;
function analyzePatterns(maxLen: integer): integer;
function predictLastMove(): integer;
function analyzeFrequency(): integer;
function getFallbackPrediction(): integer;
function getPrediction(): integer;
procedure switchStrategyIfNeeded(outcome: integer);
function choose(previousOpponentChoice: integer): integer;
procedure onGameStart();
procedure setParameters(setCount, winsPerSet: integer);

implementation

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

function cycleMove(move: integer): integer;
begin
  if move = ROCK then
    cycleMove := PAPER
  else if move = PAPER then
    cycleMove := SCISSORS
  else if move = SCISSORS then
    cycleMove := ROCK
  else
    cycleMove := PAPER;
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

function analyzeInteractionPatterns(maxLen: integer): integer;
var
  len, cur_start, i, j, search_start: integer;
  match: boolean;
begin
  if history.length < maxLen + 1 then
  begin
    analyzeInteractionPatterns := -1;
    exit;
  end;

  search_start := 0;
  if history.length > 5 then search_start := history.length - 5;

  for len := maxLen downto 2 do
  begin
    if history.length < len + 1 then continue;
    cur_start := history.length - len;

    for i := search_start to history.length - len - 1 do
    begin
      match := true;
      for j := 0 to len - 1 do
      begin
        if (history.ourMoves[i + j] <> history.ourMoves[cur_start + j]) or
           (history.opponentMoves[i + j] <> history.opponentMoves[cur_start + j]) then
        begin
          match := false;
          break;
        end;
      end;
      if match then
      begin
        analyzeInteractionPatterns := history.opponentMoves[i + len];
        exit;
      end;
    end;
  end;

  analyzeInteractionPatterns := -1;
end;

function analyzeOutcomeReaction(): integer;
var
  last_outcome, last_opp: integer;
begin
  if history.length < 3 then
  begin
    analyzeOutcomeReaction := -1;
    exit;
  end;

  last_outcome := history.outcomes[history.length - 2];
  last_opp := history.opponentMoves[history.length - 2];

  if last_outcome = -WIN then
    analyzeOutcomeReaction := beats(last_opp)
  else if last_outcome = WIN then
    analyzeOutcomeReaction := last_opp
  else
    analyzeOutcomeReaction := -1;
end;

function analyzePatterns(maxLen: integer): integer;
var
  len, cur_start, i, j: integer;
  match: boolean;
begin
  if history.length < maxLen + 1 then
  begin
    analyzePatterns := -1;
    exit;
  end;

  for len := maxLen downto 2 do
  begin
    if history.length < len + 1 then continue;
    cur_start := history.length - len;

    for i := len to history.length - len - 1 do
    begin
      match := true;
      for j := 0 to len - 1 do
      begin
        if history.opponentMoves[i + j] <> history.opponentMoves[cur_start + j] then
        begin
          match := false;
          break;
        end;
      end;
      if match then
      begin
        analyzePatterns := history.opponentMoves[i + len];
        exit;
      end;
    end;
  end;

  analyzePatterns := -1;
end;

function predictLastMove(): integer;
begin
  if history.length < 2 then
  begin
    predictLastMove := -1;
    exit;
  end;
  predictLastMove := history.opponentMoves[history.length - 1];
end;

function analyzeFrequency(): integer;
var
  r, p, s: double;
  window, start, i: integer;
  w: double;
begin
  if history.length < 2 then
  begin
    analyzeFrequency := -1;
    exit;
  end;

  r := 0.0; p := 0.0; s := 0.0;
  window := Min(history.length, 20);
  start := history.length - window;

  for i := start to history.length - 1 do
  begin
    w := 0.5 + (i - start) / window;
    if i >= history.length - 3 then w := w * 2.0;
    case history.opponentMoves[i] of
      ROCK: r := r + w;
      PAPER: p := p + w;
      SCISSORS: s := s + w;
    end;
  end;

  if (r >= p) and (r >= s) then
    analyzeFrequency := ROCK
  else if p >= s then
    analyzeFrequency := PAPER
  else
    analyzeFrequency := SCISSORS;
end;

function getFallbackPrediction(): integer;
begin
  getFallbackPrediction := (history.length mod 3) + 1;
end;

function getPrediction(): integer;
var
  pred: integer;
begin
  pred := analyzeInteractionPatterns(MAX_PATTERN_LENGTH);
  if pred <> -1 then
  begin
    getPrediction := pred;
    exit;
  end;

  pred := analyzeOutcomeReaction();
  if pred <> -1 then
  begin
    getPrediction := pred;
    exit;
  end;

  pred := analyzePatterns(MAX_PATTERN_LENGTH);
  if pred <> -1 then
  begin
    getPrediction := pred;
    exit;
  end;

  pred := predictLastMove();
  if pred <> -1 then
  begin
    getPrediction := pred;
    exit;
  end;

  pred := analyzeFrequency();
  if pred <> -1 then
  begin
    getPrediction := pred;
    exit;
  end;

  getPrediction := getFallbackPrediction();
end;

procedure switchStrategyIfNeeded(outcome: integer);
begin
  if outcome = LOSE then
  begin
    Inc(current_lose_streak);
    if current_lose_streak >= LOSE_STREAK_THRESHOLD then
    begin
      current_strategy := 3 - current_strategy;
      current_lose_streak := 0;
    end;
  end
  else
    current_lose_streak := 0;
end;

function choose(previousOpponentChoice: integer): integer;
var
  ourMove, prediction, outcome: integer;
begin
  Inc(current_round);

  if previousOpponentChoice = 0 then
  begin
    history.ourMoves[0] := PAPER;
    history.opponentMoves[0] := 0;
    history.length := 1;
    current_strategy := 1;
    current_lose_streak := 0;
    choose := PAPER;
    exit;
  end;

  history.opponentMoves[history.length - 1] := previousOpponentChoice;
  outcome := determineOutcome(history.ourMoves[history.length - 1], previousOpponentChoice);
  history.outcomes[history.length - 1] := outcome;

  prediction := getPrediction();

  if current_strategy = 1 then
  begin
    ourMove := getCounterMove(prediction);

    if history.length >= 2 then
    begin
      if (history.outcomes[history.length - 1] = LOSE) and
         (history.outcomes[history.length - 2] = LOSE) then
      begin
        ourMove := cycleMove(ourMove);
      end;
    end;

    if (history.length >= 3) and
       (history.ourMoves[history.length - 1] = history.ourMoves[history.length - 2]) and
       (history.ourMoves[history.length - 2] = history.ourMoves[history.length - 3]) and
       (history.outcomes[history.length - 1] = LOSE) then
    begin
      ourMove := cycleMove(ourMove);
    end;
  end
  else
  begin
    if sequence_idx < 6 then
      ourMove := getCounterMove(prediction)
    else if sequence_idx < 8 then
      ourMove := beats(history.ourMoves[history.length - 1])
    else
      ourMove := SEQ[sequence_idx];

    sequence_idx := (sequence_idx + 1) mod 10;
  end;

  switchStrategyIfNeeded(outcome);

  if history.length < MAX_HISTORY then
  begin
    history.ourMoves[history.length] := ourMove;
    history.opponentMoves[history.length] := 0;
    Inc(history.length);
  end;

  choose := ourMove;
end;

procedure onGameStart();
begin
  FillChar(history, SizeOf(history), 0);
  current_round := 0;
  current_strategy := 1;
  current_lose_streak := 0;
  sequence_idx := 0;
end;

procedure setParameters(setCount, winsPerSet: integer);
begin
end;

end.
