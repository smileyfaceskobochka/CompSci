program BattleshipTester;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

const
  FIELD_SIZE = 10;
  SHOT_RESULT_EMPTY = 0;
  SHOT_RESULT_DAMAGE = 2;
  SHOT_RESULT_KILL = 3;
  
  CELL_EMPTY = 0;
  CELL_SHIP = 1;
  CELL_HIT = 2;
  CELL_MISS = 3;

type
  TField = array[0..FIELD_SIZE-1, 0..FIELD_SIZE-1] of integer;
  TShipInfo = record
    size: integer;
    cells: array[0..3] of record row, col: integer; end;
    cellCount: integer;
    hits: integer;
  end;
  TShipList = array[0..9] of TShipInfo;

var
  testField: TField;
  ships: TShipList;
  shipCount: integer;
  totalShots: integer;
  hits, misses, kills: integer;
  enableVerboseLog: boolean;

procedure Log(const msg: string; verbose: boolean = false);
begin
  if verbose and not enableVerboseLog then Exit;
  WriteLn(msg);
end;

function InBounds(r, c: integer): boolean;
begin
  Result := (r >= 0) and (r < FIELD_SIZE) and (c >= 0) and (c < FIELD_SIZE);
end;

{ Генерация случайной валидной расстановки кораблей }
procedure GenerateRandomField;
var
  shipSizes: array[0..9] of integer;
  i, size, row, col, dr, dc, r, c, j, ddr, ddc: integer;
  horizontal: boolean;
  canPlace: boolean;
  attempts: integer;
begin
  { Инициализация поля }
  for row := 0 to FIELD_SIZE-1 do
    for col := 0 to FIELD_SIZE-1 do
      testField[row, col] := CELL_EMPTY;
  
  shipCount := 0;
  
  { Список кораблей: 4x1, 3x2, 2x3, 1x4 }
  shipSizes[0] := 1; shipSizes[1] := 1; shipSizes[2] := 1; shipSizes[3] := 1;
  shipSizes[4] := 2; shipSizes[5] := 2; shipSizes[6] := 2;
  shipSizes[7] := 3; shipSizes[8] := 3;
  shipSizes[9] := 4;
  
  { Размещаем корабли от больших к меньшим }
  for i := 9 downto 0 do
  begin
    size := shipSizes[i];
    attempts := 0;
    
    repeat
      Inc(attempts);
      if attempts > 1000 then
      begin
        Log('ERROR: Не удалось разместить корабль размера ' + IntToStr(size));
        Exit;
      end;
      
      horizontal := Random(2) = 0;
      
      if horizontal then
      begin
        row := Random(FIELD_SIZE);
        col := Random(FIELD_SIZE - size + 1);
        dr := 0;
        dc := 1;
      end
      else
      begin
        row := Random(FIELD_SIZE - size + 1);
        col := Random(FIELD_SIZE);
        dr := 1;
        dc := 0;
      end;
      
      { Проверяем, можно ли разместить корабль }
      canPlace := true;
      for j := 0 to size - 1 do
      begin
        r := row + dr * j;
        c := col + dc * j;
        
        { Проверяем клетку и все соседние клетки }
        for ddr := -1 to 1 do
          for ddc := -1 to 1 do
          begin
            if InBounds(r + ddr, c + ddc) then
              if testField[r + ddr, c + ddc] <> CELL_EMPTY then
              begin
                canPlace := false;
                Break;
              end;
            if not canPlace then Break;
          end;
        if not canPlace then Break;
      end;
      
    until canPlace;
    
    { Размещаем корабль }
    if horizontal then
    begin
      dr := 0; dc := 1;
    end
    else
    begin
      dr := 1; dc := 0;
    end;
    
    ships[shipCount].size := size;
    ships[shipCount].cellCount := size;
    ships[shipCount].hits := 0;
    
    for j := 0 to size - 1 do
    begin
      r := row + dr * j;
      c := col + dc * j;
      testField[r, c] := CELL_SHIP;
      ships[shipCount].cells[j].row := r;
      ships[shipCount].cells[j].col := c;
    end;
    
    Inc(shipCount);
    if horizontal then
      Log('  Размещен ' + IntToStr(size) + '-палубник в [' +
          IntToStr(row) + ',' + IntToStr(col) + '] горизонтально', true)
    else
      Log('  Размещен ' + IntToStr(size) + '-палубник в [' +
          IntToStr(row) + ',' + IntToStr(col) + '] вертикально', true);
  end;
end;

procedure PrintField;
var
  row, col: integer;
begin
  WriteLn('   0 1 2 3 4 5 6 7 8 9');
  for row := 0 to FIELD_SIZE-1 do
  begin
    Write(row:2, ' ');
    for col := 0 to FIELD_SIZE-1 do
    begin
      case testField[row, col] of
        CELL_EMPTY: Write('. ');
        CELL_SHIP:  Write('O ');
        CELL_HIT:   Write('X ');
        CELL_MISS:  Write('~ ');
      end;
    end;
    WriteLn;
  end;
end;

function ProcessShot(row, col: integer): integer;
var
  i, j: integer;
  shipIndex: integer;
  allHit: boolean;
begin
  { Проверка границ }
  if not InBounds(row, col) then
  begin
    Log('ERROR: Выстрел за пределы поля [' + IntToStr(row) + ',' + IntToStr(col) + ']');
    Result := -1;
    Exit;
  end;
  
  { Проверка повторного выстрела }
  if (testField[row, col] = CELL_HIT) or (testField[row, col] = CELL_MISS) then
  begin
    Log('ERROR: Повторный выстрел в [' + IntToStr(row) + ',' + IntToStr(col) + ']');
    Result := -1;
    Exit;
  end;
  
  Inc(totalShots);
  
  { Промах }
  if testField[row, col] = CELL_EMPTY then
  begin
    testField[row, col] := CELL_MISS;
    Inc(misses);
    Log('  Выстрел #' + IntToStr(totalShots) + ' [' + IntToStr(row) + ',' + 
        IntToStr(col) + '] -> ПРОМАХ', true);
    Result := SHOT_RESULT_EMPTY;
    Exit;
  end;
  
  { Попадание }
  testField[row, col] := CELL_HIT;
  Inc(hits);
  
  { Находим корабль }
  shipIndex := -1;
  for i := 0 to shipCount - 1 do
  begin
    for j := 0 to ships[i].cellCount - 1 do
    begin
      if (ships[i].cells[j].row = row) and (ships[i].cells[j].col = col) then
      begin
        shipIndex := i;
        Inc(ships[i].hits);
        Break;
      end;
    end;
    if shipIndex >= 0 then Break;
  end;
  
  { Проверяем, убит ли корабль }
  if shipIndex >= 0 then
  begin
    allHit := ships[shipIndex].hits = ships[shipIndex].size;
    
    if allHit then
    begin
      Inc(kills);
      Log('  Выстрел #' + IntToStr(totalShots) + ' [' + IntToStr(row) + ',' + 
          IntToStr(col) + '] -> УБИТ ' + IntToStr(ships[shipIndex].size) + '-палубник', true);
      Result := SHOT_RESULT_KILL;
    end
    else
    begin
      Log('  Выстрел #' + IntToStr(totalShots) + ' [' + IntToStr(row) + ',' + 
          IntToStr(col) + '] -> РАНЕН (попаданий: ' + 
          IntToStr(ships[shipIndex].hits) + '/' + IntToStr(ships[shipIndex].size) + ')', true);
      Result := SHOT_RESULT_DAMAGE;
    end;
  end
  else
  begin
    Log('ERROR: Попадание в неизвестный корабль');
    Result := SHOT_RESULT_DAMAGE;
  end;
end;

function AllShipsDestroyed: boolean;
var
  i: integer;
begin
  Result := true;
  for i := 0 to shipCount - 1 do
  begin
    if ships[i].hits < ships[i].size then
    begin
      Result := false;
      Exit;
    end;
  end;
end;

{ ===== ИНТЕГРАЦИЯ С БОТОМ ===== }
{ Здесь должны быть функции из bot.pas }

type
  TMapRow = array of integer;
  TMap = array of TMapRow;
  TCoordinates = array of integer;

const
  MAX_SHIP_SIZE = 4;
  CELL_UNKNOWN = 0;
  CELL_BOT_MISS = 1;
  CELL_HIT_BOT = 2;
  CELL_KILLED = 3;
  CELL_IMPOSSIBLE = 4;

type
  TBotMode = (MODE_HUNT, MODE_TARGET);
  THuntPhase = (HUNT_4, HUNT_3, HUNT_2_1);
  TDirection = (DIR_NONE, DIR_HORIZONTAL, DIR_VERTICAL);

  TCoord = record
    row, col: integer;
  end;

var
  botField: array[0..FIELD_SIZE-1, 0..FIELD_SIZE-1] of integer;
  botShips: array[1..4] of integer;
  currentMode: TBotMode;

  targetHits: array[0..MAX_SHIP_SIZE-1] of TCoord;
  targetHitsCount: integer;
  targetDirection: TDirection;

  huntPhase: THuntPhase;
  hunt4Pass: integer;
  hunt3Pass: integer;
  huntCells: array of TCoord;
  huntIndex: integer;

  lastShot: TCoord;

{ Вспомогательные функции бота }
procedure resetTargetMode;
begin
  targetHitsCount := 0;
  targetDirection := DIR_NONE;
end;

procedure detectDirection;
begin
  if targetHitsCount < 2 then Exit;
  if targetHits[0].row = targetHits[1].row then
    targetDirection := DIR_HORIZONTAL
  else if targetHits[0].col = targetHits[1].col then
    targetDirection := DIR_VERTICAL;
end;

procedure sortTargetHitsByCol;
var
  i, j: integer;
  tmp: TCoord;
begin
  for i := 0 to targetHitsCount - 1 do
    for j := i + 1 to targetHitsCount - 1 do
      if targetHits[j].col < targetHits[i].col then
      begin
        tmp := targetHits[i];
        targetHits[i] := targetHits[j];
        targetHits[j] := tmp;
      end;
end;

procedure sortTargetHitsByRow;
var
  i, j: integer;
  tmp: TCoord;
begin
  for i := 0 to targetHitsCount - 1 do
    for j := i + 1 to targetHitsCount - 1 do
      if targetHits[j].row < targetHits[i].row then
      begin
        tmp := targetHits[i];
        targetHits[i] := targetHits[j];
        targetHits[j] := tmp;
      end;
end;

function getPossibleShotsForSingleHit(var output: array of TCoord): integer;
const
  directions: array[0..3, 0..1] of integer = ((-1, 0), (0, 1), (1, 0), (0, -1));
var
  i, r, c, count: integer;
  row, col: integer;
begin
  count := 0;
  row := targetHits[0].row;
  col := targetHits[0].col;
  for i := 0 to 3 do
  begin
    r := row + directions[i, 0];
    c := col + directions[i, 1];
    if InBounds(r, c) and (botField[r, c] = CELL_UNKNOWN) then
    begin
      output[count].row := r;
      output[count].col := c;
      Inc(count);
    end;
  end;
  result := count;
end;

function getPossibleShotsForHorizontal(var output: array of TCoord): integer;
var
  row, minCol, maxCol, col, count: integer;
begin
  sortTargetHitsByCol;
  row := targetHits[0].row;
  minCol := targetHits[0].col;
  maxCol := targetHits[targetHitsCount - 1].col;
  count := 0;
  for col := minCol to maxCol do
    if botField[row, col] = CELL_UNKNOWN then
    begin
      output[count].row := row;
      output[count].col := col;
      Inc(count);
    end;
  if (minCol > 0) and (botField[row, minCol - 1] = CELL_UNKNOWN) then
  begin
    output[count].row := row;
    output[count].col := minCol - 1;
    Inc(count);
  end;
  if (maxCol < FIELD_SIZE - 1) and (botField[row, maxCol + 1] = CELL_UNKNOWN) then
  begin
    output[count].row := row;
    output[count].col := maxCol + 1;
    Inc(count);
  end;
  result := count;
end;

function getPossibleShotsForVertical(var output: array of TCoord): integer;
var
  col, minRow, maxRow, row, count: integer;
begin
  sortTargetHitsByRow;
  col := targetHits[0].col;
  minRow := targetHits[0].row;
  maxRow := targetHits[targetHitsCount - 1].row;
  count := 0;
  for row := minRow to maxRow do
    if botField[row, col] = CELL_UNKNOWN then
    begin
      output[count].row := row;
      output[count].col := col;
      Inc(count);
    end;
  if (minRow > 0) and (botField[minRow - 1, col] = CELL_UNKNOWN) then
  begin
    output[count].row := minRow - 1;
    output[count].col := col;
    Inc(count);
  end;
  if (maxRow < FIELD_SIZE - 1) and (botField[maxRow + 1, col] = CELL_UNKNOWN) then
  begin
    output[count].row := maxRow + 1;
    output[count].col := col;
    Inc(count);
  end;
  result := count;
end;

function getPossibleShots(var output: array of TCoord): integer;
begin
  if targetHitsCount = 1 then
    result := getPossibleShotsForSingleHit(output)
  else if targetDirection = DIR_HORIZONTAL then
    result := getPossibleShotsForHorizontal(output)
  else if targetDirection = DIR_VERTICAL then
    result := getPossibleShotsForVertical(output)
  else
    result := 0;
end;

procedure markAroundShipAsImpossible;
var
  i, dr, dc, r, c: integer;
  row, col: integer;
begin
  for i := 0 to targetHitsCount-1 do
  begin
    row := targetHits[i].row;
    col := targetHits[i].col;
    for dr := -1 to 1 do
      for dc := -1 to 1 do
      begin
        if (dr = 0) and (dc = 0) then continue;
        r := row + dr;
        c := col + dc;
        if InBounds(r, c) and (botField[r, c] = CELL_UNKNOWN) then
          botField[r, c] := CELL_IMPOSSIBLE;
      end;
  end;
end;

procedure convertHitsToKilled;
var
  i: integer;
begin
  for i := 0 to targetHitsCount - 1 do
    botField[targetHits[i].row, targetHits[i].col] := CELL_KILLED;
end;

procedure initHunt4Cells(pass: integer);
var
  r, c: integer;
  offsetR, offsetC: integer;
begin
  SetLength(huntCells, 0);
  
  case pass of
    0: begin offsetR := 0; offsetC := 0; end;
    1: begin offsetR := 1; offsetC := 1; end;
    2: begin offsetR := 2; offsetC := 2; end;
    3: begin offsetR := 3; offsetC := 3; end;
    4: begin offsetR := 4; offsetC := 4; end;
    5: begin offsetR := 5; offsetC := 5; end;
  else
    offsetR := 0; offsetC := 0;
  end;
  
  for r := 0 to FIELD_SIZE - 1 do
    for c := 0 to FIELD_SIZE - 1 do
      if ((r mod 4) = (offsetR mod 4)) and ((c mod 4) = (offsetC mod 4)) and
         (botField[r, c] = CELL_UNKNOWN) then
      begin
        SetLength(huntCells, Length(huntCells) + 1);
        huntCells[High(huntCells)].row := r;
        huntCells[High(huntCells)].col := c;
      end;
  
  huntIndex := 0;
end;

procedure initHunt3Cells(pass: integer);
var
  r, c: integer;
  offsetR, offsetC: integer;
begin
  SetLength(huntCells, 0);
  
  case pass of
    0: begin offsetR := 0; offsetC := 0; end;
    1: begin offsetR := 1; offsetC := 1; end;
    2: begin offsetR := 2; offsetC := 2; end;
    3: begin offsetR := 3; offsetC := 3; end;
  else
    offsetR := 0; offsetC := 0;
  end;
  
  for r := 0 to FIELD_SIZE - 1 do
    for c := 0 to FIELD_SIZE - 1 do
      if ((r mod 3) = (offsetR mod 3)) and ((c mod 3) = (offsetC mod 3)) and
         (botField[r, c] = CELL_UNKNOWN) then
      begin
        SetLength(huntCells, Length(huntCells) + 1);
        huntCells[High(huntCells)].row := r;
        huntCells[High(huntCells)].col := c;
      end;
  
  huntIndex := 0;
end;

procedure initHunt2And1Cells;
var
  r, c: integer;
begin
  SetLength(huntCells, 0);
  
  for r := 0 to FIELD_SIZE - 1 do
    for c := 0 to FIELD_SIZE - 1 do
      if botField[r, c] = CELL_UNKNOWN then
      begin
        SetLength(huntCells, Length(huntCells) + 1);
        huntCells[High(huntCells)].row := r;
        huntCells[High(huntCells)].col := c;
      end;
  
  huntIndex := 0;
end;

procedure initCurrentHuntPhase;
begin
  case huntPhase of
    HUNT_4: initHunt4Cells(hunt4Pass);
    HUNT_3: initHunt3Cells(hunt3Pass);
    HUNT_2_1: initHunt2And1Cells;
  end;
end;

function advanceHuntPhase: boolean;
begin
  case huntPhase of
    HUNT_4:
    begin
      if hunt4Pass < 5 then
      begin
        Inc(hunt4Pass);
        initHunt4Cells(hunt4Pass);
        result := true;
      end
      else if botShips[4] = 0 then
      begin
        huntPhase := HUNT_3;
        hunt3Pass := 0;
        initHunt3Cells(hunt3Pass);
        result := true;
      end
      else
      begin
        hunt4Pass := 0;
        initHunt4Cells(hunt4Pass);
        result := true;
      end;
    end;
    
    HUNT_3:
    begin
      if hunt3Pass < 3 then
      begin
        Inc(hunt3Pass);
        initHunt3Cells(hunt3Pass);
        result := true;
      end
      else if botShips[3] = 0 then
      begin
        huntPhase := HUNT_2_1;
        initHunt2And1Cells;
        result := true;
      end
      else
      begin
        hunt3Pass := 0;
        initHunt3Cells(hunt3Pass);
        result := true;
      end;
    end;
    
    HUNT_2_1:
    begin
      initHunt2And1Cells;
      result := (Length(huntCells) > 0);
    end;
  else
    result := false;
  end;
end;

function getNextHuntShot(var shot: TCoord): boolean;
begin
  while huntIndex < Length(huntCells) do
  begin
    shot := huntCells[huntIndex];
    Inc(huntIndex);
    if botField[shot.row, shot.col] = CELL_UNKNOWN then
      Exit(true);
  end;
  
  if advanceHuntPhase then
    result := getNextHuntShot(shot)
  else
    result := false;
end;

procedure bot_shoot(var r, c: integer);
var
  possible: array[0..7] of TCoord;
  count: integer;
  shot: TCoord;
begin
  if currentMode = MODE_TARGET then
  begin
    count := getPossibleShots(possible);
    if count > 0 then
    begin
      shot := possible[0];
      r := shot.row;
      c := shot.col;
      lastShot.row := r;
      lastShot.col := c;
      Exit;
    end
    else
    begin
      resetTargetMode;
      currentMode := MODE_HUNT;
    end;
  end;

  if not getNextHuntShot(shot) then
  begin
    r := -1;
    c := -1;
    Exit;
  end;

  r := shot.row;
  c := shot.col;
  lastShot.row := r;
  lastShot.col := c;
end;

procedure bot_shotResult(resultCode: integer);
var
  row, col: integer;
begin
  row := lastShot.row;
  col := lastShot.col;
  if (row < 0) or (col < 0) or (not InBounds(row, col)) then
    Exit;

  if resultCode = SHOT_RESULT_EMPTY then
    botField[row, col] := CELL_BOT_MISS
  else if resultCode = SHOT_RESULT_DAMAGE then
  begin
    botField[row, col] := CELL_HIT_BOT;
    if targetHitsCount < MAX_SHIP_SIZE then
    begin
      targetHits[targetHitsCount].row := row;
      targetHits[targetHitsCount].col := col;
      Inc(targetHitsCount);
    end;
    currentMode := MODE_TARGET;
    if targetHitsCount >= 2 then
      detectDirection;
  end
  else if resultCode = SHOT_RESULT_KILL then
  begin
    botField[row, col] := CELL_HIT_BOT;
    if targetHitsCount < MAX_SHIP_SIZE then
    begin
      targetHits[targetHitsCount].row := row;
      targetHits[targetHitsCount].col := col;
      Inc(targetHitsCount);
    end;
    convertHitsToKilled;
    markAroundShipAsImpossible;
    if (targetHitsCount >= 1) and (targetHitsCount <= 4) then
      Dec(botShips[targetHitsCount]);
    resetTargetMode;
    currentMode := MODE_HUNT;
  end;
end;

procedure bot_onSetStart;
begin
  FillChar(botField, SizeOf(botField), CELL_UNKNOWN);
  botShips[1] := 4;
  botShips[2] := 3;
  botShips[3] := 2;
  botShips[4] := 1;
  currentMode := MODE_HUNT;
  huntPhase := HUNT_4;
  hunt4Pass := 0;
  hunt3Pass := 0;
  initHunt4Cells(hunt4Pass);
  resetTargetMode;
end;

{ ===== ОСНОВНАЯ ПРОГРАММА ТЕСТИРОВАНИЯ ===== }

procedure RunTest(testNum: integer; verbose: boolean);
var
  row, col, result: integer;
  maxShots: integer;
begin
  enableVerboseLog := verbose;
  
  Log('');
  Log('========================================');
  Log('ТЕСТ #' + IntToStr(testNum));
  Log('========================================');
  
  { Генерация поля }
  Log('Генерация случайного поля...');
  GenerateRandomField;
  
  if verbose then
  begin
    Log('');
    Log('Сгенерированное поле:');
    PrintField;
  end;
  
  { Инициализация счетчиков }
  totalShots := 0;
  hits := 0;
  misses := 0;
  kills := 0;
  
  { Инициализация бота }
  bot_onSetStart;
  
  Log('');
  Log('Начало игры...');
  Log('');
  
  { Игровой цикл }
  maxShots := 200; { Защита от бесконечного цикла }
  
  while (not AllShipsDestroyed) and (totalShots < maxShots) do
  begin
    { Бот делает выстрел }
    bot_shoot(row, col);
    
    if (row < 0) or (col < 0) then
    begin
      Log('ERROR: Бот вернул некорректные координаты');
      Break;
    end;
    
    { Обработка выстрела }
    result := ProcessShot(row, col);
    
    if result < 0 then
    begin
      Log('ERROR: Ошибка при обработке выстрела');
      Break;
    end;
    
    { Передаем результат боту }
    bot_shotResult(result);
  end;
  
  { Результаты теста }
  Log('');
  Log('========================================');
  Log('РЕЗУЛЬТАТЫ ТЕСТА #' + IntToStr(testNum));
  Log('========================================');
  Log('Всего выстрелов: ' + IntToStr(totalShots));
  Log('Попаданий: ' + IntToStr(hits));
  Log('Промахов: ' + IntToStr(misses));
  Log('Убито кораблей: ' + IntToStr(kills) + '/' + IntToStr(shipCount));
  if totalShots > 0 then
    Log('Точность: ' + FormatFloat('0.00', (hits * 1.0 / totalShots) * 100) + '%');
  
  if AllShipsDestroyed then
    Log('СТАТУС: ВСЕ КОРАБЛИ УНИЧТОЖЕНЫ ✓')
  else
    Log('СТАТУС: НЕ ВСЕ КОРАБЛИ УНИЧТОЖЕНЫ ✗');
  
  if verbose then
  begin
    Log('');
    Log('Финальное состояние поля:');
    PrintField;
  end;
end;

procedure RunMultipleTests(count: integer);
var
  i: integer;
  totalShotsSum: integer;
  minShots, maxShots: integer;
  successCount: integer;
begin
  totalShotsSum := 0;
  minShots := MaxInt;
  maxShots := 0;
  successCount := 0;
  
  Randomize;
  
  WriteLn('');
  WriteLn('========================================');
  WriteLn('ЗАПУСК ' + IntToStr(count) + ' ТЕСТОВ');
  WriteLn('========================================');
  WriteLn('');
  
  for i := 1 to count do
  begin
    RunTest(i, false);
    
    if AllShipsDestroyed then
    begin
      Inc(successCount);
      totalShotsSum := totalShotsSum + totalShots;
      if totalShots < minShots then minShots := totalShots;
      if totalShots > maxShots then maxShots := totalShots;
    end;
  end;
  
  WriteLn('');
  WriteLn('========================================');
  WriteLn('ОБЩАЯ СТАТИСТИКА');
  WriteLn('========================================');
  WriteLn('Проведено тестов: ' + IntToStr(count));
  WriteLn('Успешных тестов: ' + IntToStr(successCount));
  if count > 0 then
    WriteLn('Процент успеха: ' + FormatFloat('0.00', (successCount / count) * 100) + '%');
  
  if successCount > 0 then
  begin
    WriteLn('');
    WriteLn('Среднее количество выстрелов: ' + FormatFloat('0.00', totalShotsSum / successCount));
    WriteLn('Минимум выстрелов: ' + IntToStr(minShots));
    WriteLn('Максимум выстрелов: ' + IntToStr(maxShots));
  end;
end;

{ Главная программа }
var
  choice: string;
  testCount: integer;
begin
  WriteLn('========================================');
  WriteLn('СИМУЛЯТОР ТЕСТИРОВАНИЯ БОТА МОРСКОЙ БОЙ');
  WriteLn('========================================');
  WriteLn('');
  WriteLn('Выберите режим:');
  WriteLn('1 - Один тест с подробными логами');
  WriteLn('2 - Множественные тесты со статистикой');
  WriteLn('');
  Write('Ваш выбор: ');
  ReadLn(choice);
  
  case choice of
    '1':
    begin
      Randomize;
      RunTest(1, true);
    end;
    
    '2':
    begin
      Write('Количество тестов (рекомендуется 100-1000): ');
      ReadLn(testCount);
      RunMultipleTests(testCount);
    end;
    
  else
    WriteLn('Некорректный выбор');
  end;
  
  WriteLn('');
  WriteLn('Нажмите Enter для выхода...');
  ReadLn;
end.
