unit dumb_bot_unit;

interface

const
    ROCK: integer = 1;
    PAPER: integer = 2;
    SCISSORS: integer = 3;

procedure setParameters(setCount: integer; winsPerSet: integer);
procedure onGameStart();
function choose(previousOpponentChoice: integer): integer;
procedure onGameEnd();

implementation

var
    myMoves: array of integer;        // Таблица 1: МХ - Мои ходы
    oppMoves: array of integer;       // Таблица 1: ХП - Ходы противника
    results: array of char;           // Таблица 1: Рез - Результаты
    currentStrategy: integer;         // Текущая стратегия
    defeatCount: integer;             // С.П. - Счетчик поражений
    roundCount: integer;              // С.Р. - Счетчик раундов
    maxSearchDepth: integer;          // Максимальная глубина поиска

// Таблица 2: Определение результата раунда
function getRoundResult(myMove, oppMove: integer): char;
begin
    if myMove = oppMove then
        getRoundResult := 'd'
    else if ((myMove = ROCK) and (oppMove = SCISSORS)) or
            ((myMove = PAPER) and (oppMove = ROCK)) or
            ((myMove = SCISSORS) and (oppMove = PAPER)) then
        getRoundResult := 'w'
    else
        getRoundResult := 'l';
end;

// Таблица 3: Определение контрхода
function getWinningMove(oppMove: integer): integer;
begin
    if oppMove = ROCK then
        getWinningMove := PAPER
    else if oppMove = PAPER then
        getWinningMove := SCISSORS
    else
        getWinningMove := ROCK;
end;

// Анализ истории 2: Поиск паттернов
function findPattern(patternLength: integer): integer;
var
    i, j, n: integer;
    patternFound: boolean;
    currentPattern: array of integer;
    currentResults: array of char;
begin
    findPattern := 0;
    n := Length(oppMoves);
    
    if n < patternLength + 1 then
        Exit;
    
    SetLength(currentPattern, patternLength);
    SetLength(currentResults, patternLength);
    
    for i := 0 to patternLength - 1 do
    begin
        currentPattern[i] := oppMoves[n - patternLength + i];
        currentResults[i] := results[n - patternLength + i];
    end;
    
    for i := n - patternLength - 1 downto 0 do
    begin
        if n - i > maxSearchDepth + patternLength then
            Break;
            
        patternFound := true;
        
        for j := 0 to patternLength - 1 do
        begin
            if (oppMoves[i + j] <> currentPattern[j]) or 
               (results[i + j] <> currentResults[j]) then
            begin
                patternFound := false;
                Break;
            end;
        end;
        
        if patternFound and (i + patternLength < n) then
        begin
            findPattern := oppMoves[i + patternLength];
            Exit;
        end;
    end;
end;

// Анализ истории 1: Паттерн-аналитическая стратегия
function strategy0(): integer;
var
    predictedMove: integer;
begin
    predictedMove := findPattern(2);
    
    if predictedMove <> 0 then
        strategy0 := getWinningMove(predictedMove)
    else
        strategy0 := ROCK;
end;

// Результатно-ориентированная стратегия
function strategy1(): integer;
var
    lastResult: char;
    lastMyMove: integer;
begin
    if Length(results) = 0 then
    begin
        strategy1 := ROCK;
        Exit;
    end;
    
    lastResult := results[Length(results) - 1];
    lastMyMove := myMoves[Length(myMoves) - 1];
    
    if lastResult = 'w' then
        strategy1 := lastMyMove
    else
        strategy1 := ROCK;
end;

// Проверка смены стратегии
procedure checkStrategyChange();
begin
    if defeatCount >= 5 then
    begin
        currentStrategy := 1 - currentStrategy;
        defeatCount := 0;
    end;
end;

procedure setParameters(setCount: integer; winsPerSet: integer);
begin
end;

// Начало игры
procedure onGameStart();
begin
    SetLength(myMoves, 0);
    SetLength(oppMoves, 0); 
    SetLength(results, 0);
    
    currentStrategy := 0;
    defeatCount := 0;
    roundCount := 1;
    maxSearchDepth := 15;
end;

// Основной алгоритм выбора хода
function choose(previousOpponentChoice: integer): integer;
var
    currentChoice: integer;
    lastResult: char;
begin
    // Получение ответа от сервера и обновление истории
    if (roundCount > 1) and (previousOpponentChoice <> 0) then
    begin
        SetLength(oppMoves, Length(oppMoves) + 1);
        oppMoves[Length(oppMoves) - 1] := previousOpponentChoice;
        
        // Определяем ПР (предыдущий результат)
        lastResult := getRoundResult(myMoves[Length(myMoves) - 1], previousOpponentChoice);
        
        SetLength(results, Length(results) + 1);
        results[Length(results) - 1] := lastResult;
        
        // Обновление С.П. (счетчика поражений)
        if lastResult = 'l' then
            defeatCount := defeatCount + 1
        else
            defeatCount := 0;
        
        // Проверка смены стратегии
        checkStrategyChange();
    end;
    
    // Логика выбора хода по раундам
    if roundCount <= 3 then
    begin
        if roundCount = 1 then
            currentChoice := ROCK      // Раунд 1: Камень
        else if roundCount = 2 then
            currentChoice := ROCK      // Раунд 2: Камень
        else 
            currentChoice := PAPER;    // Раунд 3: Бумага
    end
    else
    begin
        // Выбор стратегии
        if currentStrategy = 0 then
            currentChoice := strategy0()  // Паттерн-аналитическая
        else
            currentChoice := strategy1(); // Результатно-ориентированная
    end;
    
    // Сохранение хода в историю
    SetLength(myMoves, Length(myMoves) + 1);
    myMoves[Length(myMoves) - 1] := currentChoice;
    
    // Увеличиваем С.Р. (счетчик раундов)
    roundCount := roundCount + 1;
    choose := currentChoice;
end;

procedure onGameEnd();
begin
    SetLength(myMoves, 0);
    SetLength(oppMoves, 0);
    SetLength(results, 0);
end;

end.
