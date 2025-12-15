{ Модуль бота для игры в морской бой }
{ Реализует ИИ для автоматической игры в battleship }
unit bot;

interface

{ Типы данных для представления карты и координат }
type
  TMapRow = array of integer; { Строка карты }
  TMap = array of TMapRow; { Карта поля }
  TCoordinates = array of integer; { Координаты выстрела }

{ Константы для результатов выстрелов }
const
  SHOT_RESULT_EMPTY = 0; { Промах }
  SHOT_RESULT_DAMAGE = 2; { Попадание }
  SHOT_RESULT_KILL = 3; { Убийство корабля }

  FIELD_SIZE = 10; { Размер поля }
  MAX_SHIP_SIZE = 4; { Максимальный размер корабля }

{ Константы для состояний клеток поля }
  CELL_UNKNOWN = 0; { Неизвестная клетка }
  CELL_MISS = 1; { Промах }
  CELL_HIT = 2; { Попадание }
  CELL_KILLED = 3; { Убитый корабль }
  CELL_IMPOSSIBLE = 4; { Невозможное место для корабля }

{ Устанавливает параметры игры (не используется) }
procedure setParameters(setCount: integer);
{ Вызывается в начале игры }
procedure onGameStart();
{ Вызывается в начале сета }
procedure onSetStart();
{ Возвращает очередную предопределенную карту для размещения кораблей }
function getMap(): TMap;
{ Возвращает координаты следующего выстрела }
function shoot(): TCoordinates;
{ Обрабатывает результат выстрела }
procedure shotResult(resultCode: integer);
{ Вызывается при выстреле противника (не используется) }
procedure onOpponentShot(cell: TCoordinates);
{ Вызывается в конце сета (не используется) }
procedure onSetEnd();
{ Вызывается в конце игры (не используется) }
procedure onGameEnd();

implementation

{ Внутренние типы для работы бота }
type
  TBotMode = (MODE_HUNT, MODE_TARGET); { Режимы: поиск или добивание }
  THuntPhase = (HUNT_4, HUNT_3, HUNT_2_1); { Фазы поиска по размерам кораблей }
  TDirection = (DIR_NONE, DIR_HORIZONTAL, DIR_VERTICAL); { Направления корабля }

  TCoord = record
    row, col: integer; { Координаты клетки }
  end;

{ Глобальные переменные бота }
var
  field: array[0..FIELD_SIZE-1, 0..FIELD_SIZE-1] of integer; { Карта состояния поля }
  ships: array[1..4] of integer; { Счетчики оставшихся кораблей по размерам }
  currentMode: TBotMode; { Текущий режим работы бота }

  targetHits: array[0..MAX_SHIP_SIZE-1] of TCoord; { Координаты попаданий в текущий корабль }
  targetHitsCount: integer; { Количество попаданий в текущий корабль }
  targetDirection: TDirection; { Направление текущего корабля }

  huntPhase: THuntPhase; { Текущая фаза поиска }
  hunt4Pass: integer; { Номер прохода для 4-палубников }
  hunt3Pass: integer; { Номер прохода для 3-палубников }
  huntCells: array[0..99] of TCoord; { Очередь клеток для поиска }
  huntCellsCount: integer; { Количество клеток в очереди }
  huntIndex: integer; { Текущий индекс в очереди }

  lastShot: TCoord; { Координаты последнего выстрела }
  currentMapIndex: integer; { Индекс текущей карты }

{ Проверяет, находятся ли координаты в пределах поля }
function inBounds(r, c: integer): boolean;
begin
  inBounds := (r >= 0) and (r < FIELD_SIZE) and (c >= 0) and (c < FIELD_SIZE);
end;

{ Сбрасывает режим добивания }
procedure resetTargetMode;
begin
  targetHitsCount := 0;
  targetDirection := DIR_NONE;
end;

{ Помечает клетки вокруг корабля как невозможные для стрельбы }
procedure markAroundShipAsImpossible;
var
  i, dr, dc, r, c, row, col: integer;
begin
  for i := 0 to targetHitsCount - 1 do
  begin
    row := targetHits[i].row;
    col := targetHits[i].col;
    for dr := -1 to 1 do
      for dc := -1 to 1 do
      begin
        if (dr = 0) and (dc = 0) then continue;
        r := row + dr;
        c := col + dc;
        if inBounds(r, c) and (field[r, c] = CELL_UNKNOWN) then
          field[r, c] := CELL_IMPOSSIBLE;
      end;
  end;
end;

{ Помечает попадания в корабль как убитые }
procedure convertHitsToKilled;
var
  i: integer;
begin
  for i := 0 to targetHitsCount - 1 do
    field[targetHits[i].row, targetHits[i].col] := CELL_KILLED;
end;

{ Определяет направление корабля по попаданиям }
procedure detectDirection;
begin
  if targetHitsCount < 2 then Exit;
  if targetHits[0].row = targetHits[1].row then
    targetDirection := DIR_HORIZONTAL
  else if targetHits[0].col = targetHits[1].col then
    targetDirection := DIR_VERTICAL;
end;

{ Сортирует попадания по столбцам }
procedure sortTargetHitsByCol;
var
  i, j: integer;
  tmp: TCoord;
begin
  for i := 0 to targetHitsCount - 2 do
    for j := i + 1 to targetHitsCount - 1 do
      if targetHits[j].col < targetHits[i].col then
      begin
        tmp := targetHits[i];
        targetHits[i] := targetHits[j];
        targetHits[j] := tmp;
      end;
end;

{ Сортирует попадания по строкам }
procedure sortTargetHitsByRow;
var
  i, j: integer;
  tmp: TCoord;
begin
  for i := 0 to targetHitsCount - 2 do
    for j := i + 1 to targetHitsCount - 1 do
      if targetHits[j].row < targetHits[i].row then
      begin
        tmp := targetHits[i];
        targetHits[i] := targetHits[j];
        targetHits[j] := tmp;
      end;
end;

{ Получает возможные выстрелы для одного попадания (4 направления) }
function getPossibleShotsForSingleHit(var output: array of TCoord): integer;
const
  directions: array[0..3, 0..1] of integer = ((-1, 0), (0, 1), (1, 0), (0, -1));
var
  i, r, c, count, row, col: integer;
begin
  count := 0;
  row := targetHits[0].row;
  col := targetHits[0].col;
  for i := 0 to 3 do
  begin
    r := row + directions[i, 0];
    c := col + directions[i, 1];
    if inBounds(r, c) and (field[r, c] = CELL_UNKNOWN) then
    begin
      output[count].row := r;
      output[count].col := c;
      Inc(count);
    end;
  end;
  getPossibleShotsForSingleHit := count;
end;

{ Получает возможные выстрелы для горизонтального корабля }
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
    if field[row, col] = CELL_UNKNOWN then
    begin
      output[count].row := row;
      output[count].col := col;
      Inc(count);
    end;
  
  if (minCol > 0) and (field[row, minCol - 1] = CELL_UNKNOWN) then
  begin
    output[count].row := row;
    output[count].col := minCol - 1;
    Inc(count);
  end;
  
  if (maxCol < FIELD_SIZE - 1) and (field[row, maxCol + 1] = CELL_UNKNOWN) then
  begin
    output[count].row := row;
    output[count].col := maxCol + 1;
    Inc(count);
  end;
  
  getPossibleShotsForHorizontal := count;
end;

{ Получает возможные выстрелы для вертикального корабля }
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
    if field[row, col] = CELL_UNKNOWN then
    begin
      output[count].row := row;
      output[count].col := col;
      Inc(count);
    end;
  
  if (minRow > 0) and (field[minRow - 1, col] = CELL_UNKNOWN) then
  begin
    output[count].row := minRow - 1;
    output[count].col := col;
    Inc(count);
  end;
  
  if (maxRow < FIELD_SIZE - 1) and (field[maxRow + 1, col] = CELL_UNKNOWN) then
  begin
    output[count].row := maxRow + 1;
    output[count].col := col;
    Inc(count);
  end;
  
  getPossibleShotsForVertical := count;
end;

{ Получает возможные выстрелы в зависимости от текущего состояния }
function getPossibleShots(var output: array of TCoord): integer;
begin
  if targetHitsCount = 1 then
    getPossibleShots := getPossibleShotsForSingleHit(output)
  else if targetDirection = DIR_HORIZONTAL then
    getPossibleShots := getPossibleShotsForHorizontal(output)
  else if targetDirection = DIR_VERTICAL then
    getPossibleShots := getPossibleShotsForVertical(output)
  else
    getPossibleShots := 0;
end;

{ Инициализация клеток для поиска 4-палубников }
procedure initHunt4Cells(pass: integer);
var
  r, c, offsetR, offsetC: integer;
begin
  huntCellsCount := 0;
  
  offsetR := pass;
  offsetC := pass;
  
  for r := 0 to FIELD_SIZE - 1 do
    for c := 0 to FIELD_SIZE - 1 do
      if ((r mod 4) = (offsetR mod 4)) and ((c mod 4) = (offsetC mod 4)) and
         (field[r, c] = CELL_UNKNOWN) then
      begin
        huntCells[huntCellsCount].row := r;
        huntCells[huntCellsCount].col := c;
        Inc(huntCellsCount);
      end;
  
  huntIndex := 0;
end;

{ Инициализация клеток для поиска 3-палубников }
procedure initHunt3Cells(pass: integer);
var
  r, c, offsetR, offsetC: integer;
begin
  huntCellsCount := 0;
  
  offsetR := pass;
  offsetC := pass;
  
  for r := 0 to FIELD_SIZE - 1 do
    for c := 0 to FIELD_SIZE - 1 do
      if ((r mod 3) = (offsetR mod 3)) and ((c mod 3) = (offsetC mod 3)) and
         (field[r, c] = CELL_UNKNOWN) then
      begin
        huntCells[huntCellsCount].row := r;
        huntCells[huntCellsCount].col := c;
        Inc(huntCellsCount);
      end;
  
  huntIndex := 0;
end;

{ Инициализация клеток для поиска 2-палубников и 1-палубников }
procedure initHunt2And1Cells;
var
  r, c: integer;
begin
  huntCellsCount := 0;
  
  { Все оставшиеся неизвестные клетки }
  for r := 0 to FIELD_SIZE - 1 do
    for c := 0 to FIELD_SIZE - 1 do
      if field[r, c] = CELL_UNKNOWN then
      begin
        huntCells[huntCellsCount].row := r;
        huntCells[huntCellsCount].col := c;
        Inc(huntCellsCount);
      end;
  
  huntIndex := 0;
end;

{ Переход к следующей фазе поиска }
function advanceHuntPhase: boolean;
begin
  advanceHuntPhase := false;
  
  case huntPhase of
    HUNT_4:
    begin
      if hunt4Pass < 5 then
      begin
        Inc(hunt4Pass);
        initHunt4Cells(hunt4Pass);
        advanceHuntPhase := true;
      end
      else if ships[4] = 0 then
      begin
        { Все 4-палубники уничтожены, переходим к 3-палубникам }
        huntPhase := HUNT_3;
        hunt3Pass := 0;
        initHunt3Cells(0);
        advanceHuntPhase := true;
      end
      else
      begin
        { Начинаем проходы заново }
        hunt4Pass := 0;
        initHunt4Cells(0);
        advanceHuntPhase := true;
      end;
    end;
    
    HUNT_3:
    begin
      if hunt3Pass < 3 then
      begin
        Inc(hunt3Pass);
        initHunt3Cells(hunt3Pass);
        advanceHuntPhase := true;
      end
      else if ships[3] = 0 then
      begin
        { Все 3-палубники уничтожены, переходим к 2-палубникам и 1-палубникам }
        huntPhase := HUNT_2_1;
        initHunt2And1Cells;
        advanceHuntPhase := true;
      end
      else
      begin
        { Начинаем проходы заново }
        hunt3Pass := 0;
        initHunt3Cells(0);
        advanceHuntPhase := true;
      end;
    end;
    
    HUNT_2_1:
    begin
      { Обновляем список клеток }
      initHunt2And1Cells;
      advanceHuntPhase := huntCellsCount > 0;
    end;
  end;
end;

{ Получить следующий выстрел в режиме поиска }
function getNextHuntShot(var shot: TCoord): boolean;
begin
  while huntIndex < huntCellsCount do
  begin
    shot := huntCells[huntIndex];
    Inc(huntIndex);
    if field[shot.row, shot.col] = CELL_UNKNOWN then
    begin
      getNextHuntShot := true;
      Exit;
    end;
  end;
  
  { Текущая очередь исчерпана, переходим к следующей фазе }
  if advanceHuntPhase then
    getNextHuntShot := getNextHuntShot(shot)
  else
    getNextHuntShot := false;
end;

{ Внутренняя функция для выбора клетки для выстрела }
procedure internal_shoot(var r, c: integer);
var
  possible: array[0..19] of TCoord;
  count: integer;
  shot: TCoord;
begin
  { Режим добивания }
  if currentMode = MODE_TARGET then
  begin
    count := getPossibleShots(possible);
    if count > 0 then
    begin
      shot := possible[0];
      r := shot.row;
      c := shot.col;
      Exit;
    end
    else
    begin
      { Не можем добить корабль, возвращаемся к поиску }
      resetTargetMode;
      currentMode := MODE_HUNT;
    end;
  end;

  { Режим поиска }
  if not getNextHuntShot(shot) then
  begin
    r := -1;
    c := -1;
    Exit;
  end;

  r := shot.row;
  c := shot.col;
end;

{ Основная функция для выбора выстрела }
function shoot(): TCoordinates;
var
  r, c: integer;
begin
  shoot := nil; { Инициализация }
  internal_shoot(r, c);
  lastShot.row := r;
  lastShot.col := c;
  SetLength(shoot, 2);
  shoot[0] := r;
  shoot[1] := c;
end;

{ Обрабатывает результат последнего выстрела }
procedure shotResult(resultCode: integer);
var
  row, col: integer;
begin
  row := lastShot.row;
  col := lastShot.col;
  if (row < 0) or (col < 0) or (not inBounds(row, col)) then
    Exit;

  if resultCode = SHOT_RESULT_EMPTY then
  begin
    field[row, col] := CELL_MISS;
  end
  else if resultCode = SHOT_RESULT_DAMAGE then
  begin
    field[row, col] := CELL_HIT;
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
    field[row, col] := CELL_HIT;
    if targetHitsCount < MAX_SHIP_SIZE then
    begin
      targetHits[targetHitsCount].row := row;
      targetHits[targetHitsCount].col := col;
      Inc(targetHitsCount);
    end;
    
    { Уменьшаем счетчик кораблей }
    if (targetHitsCount >= 1) and (targetHitsCount <= 4) then
      Dec(ships[targetHitsCount]);
    
    { Помечаем корабль как уничтоженный и окружение как невозможное }
    convertHitsToKilled;
    markAroundShipAsImpossible;
    
    { Сбрасываем режим добивания }
    resetTargetMode;
    currentMode := MODE_HUNT;
  end;
end;

{ Устанавливает параметры игры (не используется) }
procedure setParameters(setCount: integer);
begin
end;

{ Инициализация в начале игры }
procedure onGameStart();
begin
  currentMapIndex := 0;
end;

{ Инициализация в начале сета }
procedure onSetStart();
begin
  FillChar(field, SizeOf(field), CELL_UNKNOWN);
  ships[1] := 4;
  ships[2] := 3;
  ships[3] := 2;
  ships[4] := 1;
  currentMode := MODE_HUNT;
  huntPhase := HUNT_4;
  hunt4Pass := 0;
  hunt3Pass := 0;
  initHunt4Cells(0);
  resetTargetMode;
end;

{ Предопределенные карты для размещения кораблей }
const
  mapData1: array[0..9, 0..9] of integer = (
    (0, 0, 0, 0, 0, 0, 0, 1, 0, 1),
    (0, 0, 0, 0, 0, 0, 0, 1, 0, 1),
    (0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 1, 0, 1),
    (0, 0, 1, 0, 0, 0, 0, 1, 0, 1),
    (0, 0, 0, 0, 1, 0, 0, 1, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
    (0, 1, 0, 0, 0, 0, 0, 1, 0, 1),
    (0, 0, 0, 0, 0, 0, 0, 1, 0, 1),
    (0, 0, 0, 0, 0, 0, 0, 1, 0, 1)
  );
  mapData2: array[0..9, 0..9] of integer = (
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
    (0, 0, 0, 0, 0, 0, 0, 1, 0, 1),
    (0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
    (0, 0, 1, 0, 0, 0, 0, 0, 0, 1),
    (1, 0, 0, 0, 0, 0, 0, 0, 0, 1),
    (1, 0, 0, 0, 0, 0, 1, 0, 0, 1),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (1, 1, 0, 1, 1, 0, 1, 1, 1, 0)
  );
  mapData3: array[0..9, 0..9] of integer = (
    (0, 0, 0, 0, 0, 0, 0, 1, 0, 1),
    (0, 0, 0, 0, 0, 0, 0, 1, 0, 1),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 1, 0, 0, 1, 0, 1),
    (0, 0, 0, 0, 0, 0, 0, 1, 0, 1),
    (0, 1, 0, 0, 0, 0, 0, 1, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
    (0, 0, 0, 0, 1, 0, 0, 1, 0, 1),
    (0, 0, 1, 0, 0, 0, 0, 1, 0, 1),
    (0, 0, 0, 0, 0, 0, 0, 1, 0, 1)
  );
  mapData4: array[0..9, 0..9] of integer = (
    (0, 1, 1, 0, 0, 0, 0, 1, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 1, 0, 1),
    (0, 0, 0, 0, 1, 0, 0, 0, 0, 1),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
    (0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
    (0, 0, 0, 1, 0, 0, 0, 0, 0, 1),
    (0, 0, 0, 0, 0, 0, 0, 1, 0, 1),
    (0, 1, 1, 1, 0, 0, 0, 1, 0, 1)
  );
  mapData5: array[0..9, 0..9] of integer = (
    (0, 0, 0, 0, 0, 0, 0, 1, 0, 1),
    (0, 0, 0, 0, 0, 0, 0, 1, 0, 1),
    (0, 1, 0, 0, 0, 0, 0, 0, 0, 1),
    (0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
    (1, 0, 0, 0, 0, 1, 0, 0, 0, 0),
    (1, 0, 0, 1, 0, 0, 0, 0, 0, 0),
    (1, 0, 0, 0, 0, 0, 0, 0, 0, 1),
    (1, 0, 0, 0, 0, 1, 1, 1, 0, 1)
  );
  mapData6: array[0..9, 0..9] of integer = (
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 1, 0, 0, 0, 1, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (1, 1, 0, 1, 1, 1, 0, 1, 1, 1),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (1, 1, 0, 1, 1, 0, 1, 1, 1, 1)
  );

function getMap(): TMap;
var
  i, j: integer;
  res: TMap;
  selectedMap: array[0..9, 0..9] of integer;
begin
  case currentMapIndex mod 6 of
    0: selectedMap := mapData1;
    1: selectedMap := mapData2;
    2: selectedMap := mapData3;
    3: selectedMap := mapData4;
    4: selectedMap := mapData5;
    5: selectedMap := mapData6;
  else
    selectedMap := mapData1;
  end;

  Inc(currentMapIndex);

  SetLength(res, 10);
  for i := 0 to 9 do
  begin
    SetLength(res[i], 10);
    for j := 0 to 9 do
      res[i][j] := selectedMap[i, j];
  end;
  
  getMap := res;
end;

procedure onOpponentShot(cell: TCoordinates);
begin
end;

procedure onSetEnd();
begin
end;

procedure onGameEnd();
begin
end;

end.
