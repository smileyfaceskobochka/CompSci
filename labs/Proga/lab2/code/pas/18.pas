uses
  Math;
var
  n, i, div3, evenSum, evenCount, evenAvg: Integer;
  arr, narr: array of Integer;
begin
  ReadLn(n);

  div3 := 0;
  evenSum := 0;
  evenCount := 0;

  SetLength(arr, n);
  SetLength(narr, n + 2);

  for i := 0 to n - 1 do
  begin
    ReadLn(arr[i]);

    if arr[i] mod 3 = 0 then
      Inc(div3);
    if arr[i] mod 2 = 0 then
    begin
      evenSum := evenSum + arr[i];
      Inc(evenCount);
    end;
  end;

  if evenCount > 0 then
    evenAvg := Round(evenSum / evenCount)
  else
    evenAvg := 0;
  narr[0] := div3;

  for i := 0 to n - 1 do
    narr[i + 1] := arr[i];
  narr[n + 1] := evenAvg;

  for i := 0 to n + 1 do
    Write(narr[i], ' ');
  WriteLn;
end.