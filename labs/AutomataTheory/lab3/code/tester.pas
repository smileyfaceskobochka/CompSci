program BattleshipTester;

{$mode objfpc}{$H+}

uses
  SysUtils, bot;

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

procedure Log(const msg: string; verbose: boolean);
begin
  if verbose and not enableVerboseLog then Exit;
  WriteLn(msg);
end;

function InBounds(r, c: integer): boolean;
begin
  result := (r >= 0) and (r < FIELD_SIZE) and (c >= 0) and (c < FIELD_SIZE);
end;

procedure GenerateRandomField;
var
  shipSizes: array[0..9] of integer;
  i, size, row, col, dr, dc, r, c, j, ddr, ddc: integer;
  horizontal: boolean;
  canPlace: boolean;
  attempts: integer;
begin
  for row := 0 to FIELD_SIZE-1 do
    for col := 0 to FIELD_SIZE-1 do
      testField[row, col] := CELL_EMPTY;
  
  shipCount := 0;
  
  shipSizes[0] := 1; shipSizes[1] := 1; shipSizes[2] := 1; shipSizes[3] := 1;
  shipSizes[4] := 2; shipSizes[5] := 2; shipSizes[6] := 2;
  shipSizes[7] := 3; shipSizes[8] := 3;
  shipSizes[9] := 4;
  
  for i := 9 downto 0 do
  begin
    size := shipSizes[i];
    attempts := 0;
    
    repeat
      Inc(attempts);
      if attempts > 1000 then
      begin
        Log('ERROR: Не удалось разместить корабль размера ' + IntToStr(size), false);
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
      
      canPlace := true;
      for j := 0 to size - 1 do
      begin
        r := row + dr * j;
        c := col + dc * j;
        
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
  if not InBounds(row, col) then
  begin
    Log('ERROR: Выстрел за пределы поля [' + IntToStr(row) + ',' + IntToStr(col) + ']', false);
    result := -1;
    Exit;
  end;
  
  if (testField[row, col] = CELL_HIT) or (testField[row, col] = CELL_MISS) then
  begin
    Log('ERROR: Повторный выстрел в [' + IntToStr(row) + ',' + IntToStr(col) + ']', false);
    result := -1;
    Exit;
  end;
  
  Inc(totalShots);
  
  if testField[row, col] = CELL_EMPTY then
  begin
    testField[row, col] := CELL_MISS;
    Inc(misses);
    Log('  Выстрел #' + IntToStr(totalShots) + ' [' + IntToStr(row) + ',' + 
        IntToStr(col) + '] -> ПРОМАХ', true);
    result := SHOT_RESULT_EMPTY;
    Exit;
  end;
  
  testField[row, col] := CELL_HIT;
  Inc(hits);
  
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
  
  if shipIndex >= 0 then
  begin
    allHit := ships[shipIndex].hits = ships[shipIndex].size;
    
    if allHit then
    begin
      Inc(kills);
      Log('  Выстрел #' + IntToStr(totalShots) + ' [' + IntToStr(row) + ',' + 
          IntToStr(col) + '] -> УБИТ ' + IntToStr(ships[shipIndex].size) + '-палубник', true);
      result := SHOT_RESULT_KILL;
    end
    else
    begin
      Log('  Выстрел #' + IntToStr(totalShots) + ' [' + IntToStr(row) + ',' + 
          IntToStr(col) + '] -> РАНЕН (попаданий: ' + 
          IntToStr(ships[shipIndex].hits) + '/' + IntToStr(ships[shipIndex].size) + ')', true);
      result := SHOT_RESULT_DAMAGE;
    end;
  end
  else
  begin
    Log('ERROR: Попадание в неизвестный корабль', false);
    result := SHOT_RESULT_DAMAGE;
  end;
end;

function AllShipsDestroyed: boolean;
var
  i: integer;
begin
  result := true;
  for i := 0 to shipCount - 1 do
  begin
    if ships[i].hits < ships[i].size then
    begin
      result := false;
      Exit;
    end;
  end;
end;

procedure RunTest(testNum: integer; verbose: boolean);
var
  row, col, shotResult: integer;
  coords: TCoordinates;
  maxShots: integer;
begin
  enableVerboseLog := verbose;
  
  Log('', false);
  Log('========================================', false);
  Log('ТЕСТ #' + IntToStr(testNum), false);
  Log('========================================', false);
  
  Log('Генерация случайного поля...', false);
  GenerateRandomField;
  
  if verbose then
  begin
    Log('', false);
    Log('Сгенерированное поле:', false);
    PrintField;
  end;
  
  totalShots := 0;
  hits := 0;
  misses := 0;
  kills := 0;
  
  bot.onSetStart;
  
  Log('', false);
  Log('Начало игры...', false);
  Log('', false);
  
  maxShots := 200;
  
  while (not AllShipsDestroyed) and (totalShots < maxShots) do
  begin
    coords := bot.shoot();
    row := coords[0];
    col := coords[1];
    
    if (row < 0) or (col < 0) then
    begin
      Log('ERROR: Бот вернул некорректные координаты', false);
      Break;
    end;
    
    shotResult := ProcessShot(row, col);
    
    if shotResult < 0 then
    begin
      Log('ERROR: Ошибка при обработке выстрела', false);
      Break;
    end;
    
    bot.shotResult(shotResult);
  end;
  
  Log('', false);
  Log('========================================', false);
  Log('РЕЗУЛЬТАТЫ ТЕСТА #' + IntToStr(testNum), false);
  Log('========================================', false);
  Log('Всего выстрелов: ' + IntToStr(totalShots), false);
  Log('Попаданий: ' + IntToStr(hits), false);
  Log('Промахов: ' + IntToStr(misses), false);
  Log('Убито кораблей: ' + IntToStr(kills) + '/' + IntToStr(shipCount), false);
  if totalShots > 0 then
    Log('Точность: ' + FormatFloat('0.00', (hits * 1.0 / totalShots) * 100) + '%', false);
  
  if AllShipsDestroyed then
    Log('СТАТУС: ВСЕ КОРАБЛИ УНИЧТОЖЕНЫ ✓', false)
  else
    Log('СТАТУС: НЕ ВСЕ КОРАБЛИ УНИЧТОЖЕНЫ ✗', false);
  
  if verbose then
  begin
    Log('', false);
    Log('Финальное состояние поля:', false);
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
  
  bot.onGameStart;
  
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
      bot.onGameStart;
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