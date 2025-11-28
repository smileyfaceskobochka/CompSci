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
  ships: array[1..4] of integer;
  currentMode: TBotMode;

  targetHits: array[0..MAX_SHIP_SIZE-1] of TCoord;
  targetHitsCount: integer;
  targetDirection: TDirection;

  hunt_n: integer;
  huntIndex: integer;
  huntCells: array of TCoord;

  lastShot: TCoord;

function inBounds(r, c: integer): boolean;
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
        if inBounds(r, c) and (field[r, c] = CELL_UNKNOWN) then
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
    if inBounds(r, c) and (field[r, c] = CELL_UNKNOWN) then
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

procedure initHuntCells(n: integer);
var
  r, c: integer;
begin
  SetLength(huntCells, 0);
  for r := 0 to FIELD_SIZE - 1 do
    for c := 0 to FIELD_SIZE - 1 do
      if (r mod n < 2) and (c mod n < 2) and (field[r, c] = CELL_UNKNOWN) then
      begin
        SetLength(huntCells, Length(huntCells) + 1);
        huntCells[High(huntCells)].row := r;
        huntCells[High(huntCells)].col := c;
      end;
  huntIndex := 0;
end;

function getNextHuntShot(var shot: TCoord): boolean;
begin
  while huntIndex < Length(huntCells) do
  begin
    shot := huntCells[huntIndex];
    Inc(huntIndex);
    if field[shot.row, shot.col] = CELL_UNKNOWN then
      Exit(true);
  end;
  result := false;
end;

procedure internal_shoot(var r, c: integer);
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
      Exit;
    end
    else
    begin
      resetTargetMode;
      hunt_n := 4;
      initHuntCells(hunt_n);
      currentMode := MODE_HUNT;
    end;
  end;

  while not getNextHuntShot(shot) do
  begin
    case hunt_n of
      4: hunt_n := 3;
      3: hunt_n := 2;
      2: hunt_n := 1;
      1:
      begin
        r := -1;
        c := -1;
        Exit;
      end;
    end;
    initHuntCells(hunt_n);
  end;

  r := shot.row;
  c := shot.col;
end;

procedure shoot_v2(var r, c: integer);
begin
  internal_shoot(r, c);
  lastShot.row := r;
  lastShot.col := c;
end;

procedure shotResult_v2(resultCode: integer);
var
  row, col, i: integer;
begin
  row := lastShot.row;
  col := lastShot.col;
  if (row < 0) or (col < 0) or (not inBounds(row, col)) then
    Exit;

  if resultCode = SHOT_RESULT_EMPTY then
    field[row, col] := CELL_MISS
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
    convertHitsToKilled;
    markAroundShipAsImpossible;
    if (targetHitsCount >= 1) and (targetHitsCount <= 4) then
      Dec(ships[targetHitsCount]);
    resetTargetMode;
    hunt_n := 4;
    initHuntCells(hunt_n);
    currentMode := MODE_HUNT;
  end;
end;

procedure setParameters(setCount: integer);
begin
end;

procedure onGameStart();
begin
end;

procedure onSetStart();
begin
  FillChar(field, SizeOf(field), CELL_UNKNOWN);
  ships[1] := 4;
  ships[2] := 3;
  ships[3] := 2;
  ships[4] := 1;
  currentMode := MODE_HUNT;
  hunt_n := 4;
  initHuntCells(hunt_n);
  resetTargetMode;
end;

function getMap(): TMap;
begin
  result := TMap.Create(
    TMapRow.Create(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    TMapRow.Create(0, 0, 0, 1, 0, 0, 0, 1, 0, 0),
    TMapRow.Create(0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
    TMapRow.Create(0, 1, 0, 0, 0, 1, 1, 0, 0, 1),
    TMapRow.Create(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    TMapRow.Create(0, 0, 1, 1, 1, 0, 0, 0, 0, 0),
    TMapRow.Create(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    TMapRow.Create(0, 0, 1, 0, 1, 0, 1, 1, 1, 1),
    TMapRow.Create(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
    TMapRow.Create(0, 0, 0, 0, 0, 0, 1, 1, 1, 0)
  );
end;

function shoot(): TCoordinates;
var
  r, c: integer;
begin
  shoot_v2(r, c);
  result := TCoordinates.Create(r, c);
end;

procedure shotResult(resultCode: integer);
begin
  shotResult_v2(resultCode);
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