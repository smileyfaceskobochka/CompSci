unit bot;

interface

uses Math;

const
  ROCK = 1;                    // Камень
  PAPER = 2;                   // Бумага
  SCISSORS = 3;                // Ножницы
  WIN = 1;                     // Победа
  LOSE = -1;                   // Поражение
  DRAW = 0;                    // Ничья
  MAX_HISTORY = 1000;          // Максимальная длина истории
  BASE_MAX_PATTERN_LENGTH = 10; // Базовая максимальная длина паттерна (увеличена для лучшего поиска)
  BASE_ANALYSIS_WINDOW = 80;   // Базовое окно анализа
  LOSE_STREAK_THRESHOLD = 5;   // Порог поражений подряд для смены тактики

type
  THistory = record
    opponentMoves: array[0..MAX_HISTORY-1] of integer; // Ходы противника
    ourMoves: array[0..MAX_HISTORY-1] of integer;      // Наши ходы
    outcomes: array[0..MAX_HISTORY-1] of integer;      // Исходы раундов
    length: integer;                                   // Текущая длина истории
  end;

var
  history: THistory;                    // История игры
  current_lose_streak: integer;         // Текущая серия поражений
  current_tactic: integer;              // Текущая тактика (1-паттерны, 2-частоты)
  current_pattern_length: integer;      // Текущая длина паттерна
  current_analysis_window: integer;     // Текущее окно анализа
  adaptation_level: double;             // Уровень адаптации (0.3-1.0)

function choose(previousOpponentChoice: integer): integer;          // Основная функция выбора хода
function determineOutcome(ourMove, opponentMove: integer): integer; // Определение исхода раунда
function beats(move: integer): integer;                             // Функция получения выигрышного хода
function getCounterMove(predictedMove: integer): integer;           // Функция получения контр-хода
function analyzeFrequency(): integer;                               // Функция анализа частоты
function analyzePatterns(maxLen: integer): integer;                 // Функция анализа паттернов
function getPrediction(): integer;                                  // Функция получения предсказания
procedure switchTacticIfNeeded(outcome: integer);                   // Проверка смены тактики
procedure onGameStart();                                            // Инициализация игры
procedure setParameters(setCount, winsPerSet: integer);             // Настройка параметров
procedure onGameEnd();

implementation

// Функция получения выигрышного хода
function beats(move: integer): integer;
begin
  case move of
    ROCK: beats := PAPER;       // Камень бьет ножницы
    PAPER: beats := SCISSORS;   // Бумага бьет камень
    SCISSORS: beats := ROCK;    // Ножницы бьют бумагу
    else beats := PAPER;        // По умолчанию - бумага
  end;
end;

// Функция получения контр-хода против предсказанного хода противника
function getCounterMove(predictedMove: integer): integer;
begin
  getCounterMove := beats(predictedMove);
end;

// Функция определения исхода раунда
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

function analyzeFrequency(): integer;
var
  r, p, s, w, age_factor: double; // Счетчики для Камень, Бумага, Ножницы и веса
  window, start, i: integer;      // Параметры окна анализа
begin
  if history.length < 2 then
  begin
    analyzeFrequency := -1;       // Недостаточно данных
    exit;
  end;

  r := 0.0001; p := 0.0001; s := 0.0001; // Инициализация счетчиков
  window := Min(history.length - 1, current_analysis_window); // Размер окна анализа
  start := history.length - window - 1; // Начальная позиция
  if start < 0 then start := 0;

  for i := start to history.length - 2 do
  begin
    age_factor := (i - start + 1) / window; // Возрастной фактор
    w := 0.1 + age_factor * 0.9;            // Базовый вес с приоритетом новых данных
    if i >= history.length - 10 then w := w * 3.0; // Усиление последних 10 ходов

    if i < history.length - 1 then
      case history.outcomes[i] of
        LOSE: w := w * (4.0 * adaptation_level + 2.0); // Больше внимания после проигрыша
        WIN: w := w * (0.2 * adaptation_level + 0.3);  // Меньше внимания после победы
        DRAW: w := w * (0.7 * adaptation_level + 0.8); // Среднее внимание после ничьи
      end;

    case history.opponentMoves[i] of
      ROCK: r := r + w;    // Увеличиваем счетчик камня
      PAPER: p := p + w;   // Увеличиваем счетчик бумаги
      SCISSORS: s := s + w; // Увеличиваем счетчик ножниц
    end;
  end;

  // Выбираем ход с максимальным счетчиком
  if (r > p) and (r > s) then analyzeFrequency := ROCK
  else if (p > r) and (p > s) then analyzeFrequency := PAPER
  else if (s > r) and (s > p) then analyzeFrequency := SCISSORS
  else analyzeFrequency := PAPER; // По умолчанию - бумага
end;

// Функция анализа паттернов в ходах противника
function analyzePatterns(maxLen: integer): integer;
var
  len, i, j: integer;              // Параметры циклов
  best_score: double;              // Лучший счет паттерна
  match: boolean;                  // Флаг совпадения паттерна
  best_prediction: integer;        // Лучшее предсказание
  actualMaxLen: integer;           // Фактическая максимальная длина
  cur_start: integer;              // Текущая начальная позиция
  next_move: integer;              // Следующий ход после паттерна
  outcome_bonus: double;           // Бонус за исходы
  pattern_weight: double;          // Вес паттерна
begin
  if history.length < 3 then
  begin
    analyzePatterns := -1; // Недостаточно данных
    exit;
  end;

  best_score := -9999;      // Начальное значение лучшего счета
  best_prediction := -1;    // Начальное предсказание
  actualMaxLen := Min(current_pattern_length, history.length - 1); // Максимальная длина для анализа

  // Проверяем паттерны от большей длины к меньшей
  for len := actualMaxLen downto 2 do
  begin
    cur_start := history.length - len; // Начальная позиция текущего паттерна
    
    // Ищем все вхождения паттерна данной длины
    for i := len - 1 to history.length - len - 1 do
    begin
      match := true; // Предполагаем совпадение
      
      // Проверяем совпадение всех элементов паттерна
      for j := 0 to len - 1 do
        if history.opponentMoves[i + j] <> history.opponentMoves[cur_start + j] then
        begin
          match := false; // Не совпадает
          break;
        end;

      // Если паттерн найден и есть следующий ход
      if match and (i + len < history.length) then
      begin
        next_move := history.opponentMoves[i + len]; // Ход после паттерна
        
        pattern_weight := 1.0; // Базовый вес паттерна
        outcome_bonus := 0;    // Изначальный бонус
        
        // Бонус за длину паттерна
        pattern_weight := pattern_weight + (len * 0.5 * adaptation_level);
        
        // Анализируем исходы 1-3 раундов после паттерна (усиление бонусов для лучшего выбора паттернов)
        for j := 0 to Min(2, len - 1) do
        begin
          if (i + len + j) < history.length then
            case history.outcomes[i + len + j] of
              WIN: outcome_bonus := outcome_bonus + (25 * adaptation_level + 15);  // Бонус за победу
              DRAW: outcome_bonus := outcome_bonus + (12 * adaptation_level + 8);   // Бонус за ничью
              LOSE: outcome_bonus := outcome_bonus - (12 * adaptation_level + 8);   // Штраф за поражение
            end;
        end;
        
        // Итоговый счет паттерна
        outcome_bonus := outcome_bonus * pattern_weight;

        // Если это лучший паттерн - запоминаем его
        if outcome_bonus > best_score then
        begin
          best_score := outcome_bonus;
          best_prediction := next_move;
        end;
      end;
    end;
    
    // Если нашли достаточно хороший паттерн - используем его (пониженный порог для более активного выбора)
    if best_score >= (15 * adaptation_level + 10) then
    begin
      analyzePatterns := best_prediction;
      exit;
    end;
  end;

  analyzePatterns := best_prediction;
end;

// Функция получения предсказания на основе текущей тактики
function getPrediction(): integer;
var
  pred: integer; // Предсказание
begin
  if current_tactic = 1 then
    pred := analyzePatterns(current_pattern_length)
  else
    pred := analyzeFrequency();

  if pred = -1 then pred := PAPER; // По умолчанию - бумага
  getPrediction := pred;
end;

// Процедура проверки необходимости смены тактики
procedure switchTacticIfNeeded(outcome: integer);
begin
  if outcome = LOSE then
  begin
    inc(current_lose_streak); // Увеличиваем счетчик поражений
    
    // Смена тактики только после серии поражений
    if current_lose_streak >= LOSE_STREAK_THRESHOLD then
    begin
      if current_tactic = 1 then current_tactic := 2
      else current_tactic := 1;
      current_lose_streak := 0;                      // Сбрасываем счетчик
    end;
  end
  else
    current_lose_streak := 0; // Сбрасываем при победе или ничье
end;

// Основная функция выбора хода
function choose(previousOpponentChoice: integer): integer;
var
  ourMove, prediction, outcome: integer; // Наш ход, предсказание и исход
begin
  // Принудительно возвращаем PAPER для первого хода
  if previousOpponentChoice = 0 then
  begin
    // Первый ход - инициализация
    onGameStart();
    history.ourMoves[0] := PAPER;      // Начинаем с бумаги
    history.opponentMoves[0] := 0;     // Пустой ход противника
    history.outcomes[0] := DRAW;       // Ничья по умолчанию
    history.length := 1;               // Длина истории = 1
    choose := PAPER;                   // Возвращаем бумагу
    exit;
  end;

  // Сохраняем ход противника и вычисляем исход
  history.opponentMoves[history.length - 1] := previousOpponentChoice;
  outcome := determineOutcome(history.ourMoves[history.length - 1], previousOpponentChoice);
  history.outcomes[history.length - 1] := outcome;

  // Проверяем необходимость смены тактики
  switchTacticIfNeeded(outcome);

  // Получаем предсказание и выбираем контр-ход
  prediction := getPrediction();
  if prediction <> -1 then
    ourMove := getCounterMove(prediction) // Контр-ход против предсказанного
  else
    ourMove := PAPER; // По умолчанию - бумага

  // Сохраняем наш ход для следующего раунда
  if history.length < MAX_HISTORY then
  begin
    history.ourMoves[history.length] := ourMove;
    inc(history.length);
  end;

  choose := ourMove;
end;

// Процедура инициализации в начале игры
procedure onGameStart();
begin
  FillChar(history, SizeOf(history), 0); // Очищаем всю историю
  current_lose_streak := 0;              // Обнуляем счетчик поражений
  current_tactic := 1;                   // Начинаем с анализа паттернов
  current_pattern_length := BASE_MAX_PATTERN_LENGTH;   // Базовая длина паттерна
  current_analysis_window := BASE_ANALYSIS_WINDOW;     // Базовое окно анализа
  adaptation_level := 0.5;               // Уровенный адаптации (0.3-1.0)
end;

// Процедура настройки параметров на основе статистики
procedure setParameters(setCount, winsPerSet: integer);
var
  win_rate: double; // Текущий винрейт
begin
  // Вычисляем текущий винрейт
  if setCount > 0 then
    win_rate := winsPerSet / setCount
  else
    win_rate := 0.5; // По умолчанию 50%
  
  // Микро-настройка на основе производительности
  if win_rate > 0.6 then
  begin
    // При высоком винрейте делаем более агрессивным
    adaptation_level := Min(1.0, adaptation_level + 0.1);
    current_pattern_length := Min(12, current_pattern_length + 1);
    current_analysis_window := Min(120, current_analysis_window + 10);
  end
  else if win_rate < 0.4 then
  begin
    // При низком винрейте делаем более консервативным
    adaptation_level := Max(0.3, adaptation_level - 0.1);
    current_pattern_length := Max(6, current_pattern_length - 1);
    current_analysis_window := Max(60, current_analysis_window - 10);
  end;
  
  // После многих сетов снижаем адаптацию для стабильности
  if setCount > 20 then
    adaptation_level := Max(0.4, adaptation_level - 0.05);
end;

procedure onGameEnd();
begin

end;

end.
