unit bot;

interface

type
  TMapRow = array of integer;
  TMap = array of TMapRow;
  TCoordinates = array of integer;

const
  SHOT_RESULT_EMPTY = 0;
  SHOT_RESULT_DAMAGE = 2;
  SHOT_RESULT_KILL = 3;

  FIELD_SIZE = 10;
  MAX_SHIP_SIZE = 4;

  CELL_UNKNOWN = 0;
  CELL_MISS = 1;
  CELL_HIT = 2;
  CELL_KILLED = 3;
  CELL_IMPOSSIBLE = 4;

type
  TBotMode = (MODE_HUNT, MODE_TARGET);
  TDirection = (DIR_NONE, DIR_HORIZONTAL, DIR_VERTICAL);

  TCoord = record
    row, col: integer;
  end;

procedure setParameters(setCount: integer);
procedure onGameStart();
procedure onSetStart();
function getMap(): TMap;
function shoot(): TCoordinates;
procedure shotResult(resultCode: integer);
procedure onOpponentShot(cell: TCoordinates);
procedure onSetEnd();
procedure onGameEnd();

implementation

var
  field: array[0..FIELD_SIZE-1, 0..FIELD_SIZE-1] of integer;
  ships: array[1..4] of integer; // Number of alive ships of each size
  currentMode: TBotMode;

  // For targeting mode
  targetHits: array[0..MAX_SHIP_SIZE-1] of TCoord;
  targetHitsCount: integer;
  targetDirection: TDirection;

  // For hunt mode
  globalHuntQueue: array of TCoord;
  globalHuntIndex: integer;

  lastShot: TCoord;
  currentMapIndex: integer;
  huntCells: array of TCoord;

function InBounds(r, c: integer): boolean;
begin
  result := (r >= 0) and (r < FIELD_SIZE) and (c >= 0) and (c < FIELD_SIZE);
end;

procedure resetTargetMode;
begin
  targetHitsCount := 0;
  targetDirection := DIR_NONE;
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
        if InBounds(r, c) and (field[r, c] = CELL_UNKNOWN) then
          field[r, c] := CELL_IMPOSSIBLE;
      end;
  end;
end;

procedure convertHitsToKilled;
var
  i: integer;
begin
  for i := 0 to targetHitsCount - 1 do
    field[targetHits[i].row, targetHits[i].col] := CELL_KILLED;
end;

procedure detectDirection;
begin
  if targetHitsCount < 2 then Exit;
  if targetHits[0].row = targetHits[1].row then
    targetDirection := DIR_HORIZONTAL
  else if targetHits[0].col = targetHits[1].col then
    targetDirection := DIR_VERTICAL;
end;

// Функция для сортировки точек попадания в порядке возрастания координаты col (для горизонтального корабля)
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

// Функция для сортировки точек попадания в порядке возрастания координаты row (для вертикального корабля)
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

// Получение возможных выстрелов для одиночного попадания
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
    if InBounds(r, c) and (field[r, c] = CELL_UNKNOWN) then
    begin
      output[count].row := r;
      output[count].col := c;
      Inc(count);
    end;
  end;
  result := count;
end;

// Получение возможных выстрелов для горизонтального корабля
function getPossibleShotsForHorizontal(var output: array of TCoord): integer;
var
  minCol, maxCol, col, count: integer;
  row: integer;
begin
  sortTargetHitsByCol; // Сортируем точки по колонке
  row := targetHits[0].row; // Все точки имеют одинаковую строку
  minCol := targetHits[0].col;
  maxCol := targetHits[targetHitsCount - 1].col; // Последняя точка после сортировки - самая правая
  count := 0;

  // Проверяем клетки вдоль уже подбитого участка корабля (если есть промежутки)
  for col := minCol to maxCol do
    if field[row, col] = CELL_UNKNOWN then
    begin
      output[count].row := row;
      output[count].col := col;
      Inc(count);
    end;

  // Проверяем клетки по обе стороны от "куска" корабля
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
  result := count;
end;

// Получение возможных выстрелов для вертикального корабля
function getPossibleShotsForVertical(var output: array of TCoord): integer;
var
  minRow, maxRow, row, count: integer;
  col: integer;
begin
  sortTargetHitsByRow; // Сортируем точки по строке
  col := targetHits[0].col; // Все точки имеют одинаковую колонку
  minRow := targetHits[0].row;
  maxRow := targetHits[targetHitsCount - 1].row; // Последняя точка после сортировки - самая нижняя
  count := 0;

  // Проверяем клетки вдоль уже подбитого участка корабля (если есть промежутки)
  for row := minRow to maxRow do
    if field[row, col] = CELL_UNKNOWN then
    begin
      output[count].row := row;
      output[count].col := col;
      Inc(count);
    end;

  // Проверяем клетки по обе стороны от "куска" корабля
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
  result := count;
end;

// Основная функция получения возможных выстрелов в режиме добивания
function getPossibleShots(var output: array of TCoord): integer;
begin
  if targetHitsCount = 1 then
    result := getPossibleShotsForSingleHit(output)
  else if targetDirection = DIR_HORIZONTAL then
    result := getPossibleShotsForHorizontal(output)
  else if targetDirection = DIR_VERTICAL then
    result := getPossibleShotsForVertical(output)
  else
    result := 0; // Неопределенное направление, невозможно выбрать следующую цель
end;

// Поиск размера самого большого живого корабля
function findLargestAliveShip: integer;
var
  i: integer;
begin
  for i := 4 downto 1 do
    if ships[i] > 0 then
    begin
      Result := i;
      Exit;
    end;
  Result := 0; // Все уничтожены
end;

// Инициализация глобальной очереди поиска
procedure initGlobalHuntQueue;
var
  largest: integer;
begin
  largest := findLargestAliveShip;
  if largest = 0 then
  begin
    SetLength(globalHuntQueue, 0); // Нечего искать
    globalHuntIndex := 0;
    Exit;
  end;

  if largest >= 4 then
    initHunt4Cells(0)
  else if largest >= 3 then
    initHunt3Cells(0)
  else
    initHunt2And1Cells;

  globalHuntQueue := huntCells;
  globalHuntIndex := 0;
end;

// Внутренняя функция выстрела
procedure internal_shoot(var r, c: integer);
var
  possible: array[0..FIELD_SIZE * 2 - 1] of TCoord; // Максимум 20 возможных выстрелов вдоль строки/столбца
  count: integer;
  shot: TCoord;
begin
  if currentMode = MODE_TARGET then
  begin
    count := getPossibleShots(possible);
    if count > 0 then
    begin
      shot := possible[0]; // Берем первую возможную клетку
      r := shot.row;
      c := shot.col;
      Exit;
    end
    else
    begin
      // Не нашли куда стрелять, сбрасываем режим
      resetTargetMode;
      currentMode := MODE_HUNT;
    end;
  end;

  // Режим поиска
  while globalHuntIndex < Length(globalHuntQueue) do
  begin
    shot := globalHuntQueue[globalHuntIndex];
    Inc(globalHuntIndex);
    // Проверяем, вдруг клетка стала невозможной с момента формирования очереди
    if field[shot.row, shot.col] = CELL_UNKNOWN then
    begin
      r := shot.row;
      c := shot.col;
      Exit;
    end;
  end;

  // Не осталось доступных клеток
  r := -1;
  c := -1;
end;

// Функция выстрела (публичная)
function shoot(): TCoordinates;
var
  r, c: integer;
begin
  internal_shoot(r, c);
  lastShot.row := r;
  lastShot.col := c;
  result := TCoordinates.Create(r, c);
end;

// Обработка результата выстрела
procedure shotResult(resultCode: integer);
var
  row, col: integer;
  prevLargest: integer;
begin
  row := lastShot.row;
  col := lastShot.col;
  if (row < 0) or (col < 0) or (not InBounds(row, col)) then
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
    field[row, col] := CELL_HIT; // Поле помечается как CELL_HIT, а не CELL_KILLED, пока не обработаем весь корабль
    if targetHitsCount < MAX_SHIP_SIZE then
    begin
      targetHits[targetHitsCount].row := row;
      targetHits[targetHitsCount].col := col;
      Inc(targetHitsCount);
    end;
    // Update ship count
    if (targetHitsCount >= 1) and (targetHitsCount <= 4) then
      Dec(ships[targetHitsCount]);
    // Помечаем корабль как убитый и пространство вокруг как невозможное
    convertHitsToKilled;
    markAroundShipAsImpossible;
    // Сбрасываем режим добивания
    resetTargetMode();
    currentMode := MODE_HUNT;
  end;

  // После обработки результата, возможно, нужно обновить очередь поиска
  if currentMode = MODE_HUNT then
  begin
      // Проверяем, изменился ли самый большой живой корабль
      // (это важно, если только что убит корабль или промахнулись в режиме поиска, но очередь исчерпана)
      // Однако, обычно очередь обновляется только после уничтожения корабля или исчерпания.
      // Лучше обновлять, если largest изменился или очередь пуста.
      initGlobalHuntQueue(); // Всегда обновляем очередь после выстрела в режиме hunt, чтобы учесть уничтожение и исчерпание
  end;
end;

// Функция, вызываемая в начале игры
procedure setParameters(setCount: integer);
begin
  // Используем параметр, если нужно
end;

// Функция, вызываемая в начале игры
procedure onGameStart();
begin
  currentMapIndex := 0;
end;

// Функция, вызываемая в начале сета
procedure onSetStart();
begin
  // Инициализация поля
  FillChar(field, SizeOf(field), CELL_UNKNOWN);
  // Инициализация счетчика кораблей
  ships[1] := 4;
  ships[2] := 3;
  ships[3] := 2;
  ships[4] := 1;
  // Инициализация режима
  currentMode := MODE_HUNT;
  // Инициализация режима добивания
  resetTargetMode();
  // Инициализация режима поиска
  initGlobalHuntQueue();
  // Сброс последнего выстрела
  lastShot.row := -1;
  lastShot.col := -1;
end;

// Константы для расстановок (пример, добавьте свои)
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

// Функция возврата карты
function getMap(): TMap;
var
  i, j: integer;
  res: TMap;
  selectedMapData: array of array of integer;
begin
  // Выбираем данные карты в зависимости от номера сета
  case currentMapIndex mod 6 of
    0: selectedMapData := @mapData1;
    1: selectedMapData := @mapData2;
    2: selectedMapData := @mapData3;
    3: selectedMapData := @mapData4;
    4: selectedMapData := @mapData5;
    5: selectedMapData := @mapData6;
  else
    selectedMapData := @mapData1; // Резервный случай
  end;

  currentMapIndex := (currentMapIndex + 1); // Увеличиваем индекс для следующего сета

  // Создаем и заполняем возвращаемую карту
  SetLength(res, 10);
  for i := 0 to 9 do
  begin
    SetLength(res[i], 10);
    for j := 0 to 9 do
      res[i][j] := selectedMapData^[i][j];
  end;
  result := res;
end;

// Заглушка для обработки выстрела противника
procedure onOpponentShot(cell: TCoordinates);
begin
  // Здесь можно обновить свое поле, если нужно
end;

// Заглушка для окончания сета
procedure onSetEnd();
begin
end;

// Заглушка для окончания игры
procedure onGameEnd();
begin
end;

end.
