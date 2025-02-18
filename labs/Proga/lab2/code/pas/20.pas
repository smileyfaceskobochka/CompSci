var
  n, i, firstZero, secondZero, sum: integer;
  arr: array of Integer;
begin
  readln(n);
  SetLength(arr, n);
  for i := 0 to n - 1 do
    read(arr[i]);
  
  firstZero := -1;
  secondZero := -1;
  for i := n - 1 downto 0 do
  begin
    if arr[i] = 0 then
    begin
      if firstZero = -1 then
        firstZero := i
      else if secondZero = -1 then
      begin
        secondZero := i;
        Break;
      end;
    end;
  end;
  sum := 0;
  if (secondZero <> -1) and (firstZero <> -1) then
  begin
    for i := secondZero + 1 to firstZero - 1 do
      sum := sum + arr[i];
  end;
  writeln(sum);
end.