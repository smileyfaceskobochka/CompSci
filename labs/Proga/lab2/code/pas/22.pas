var
  n, i, firstViol: integer;
  arr: array of real;
  notDecr: boolean;
begin
  readln(n);
  SetLength(arr, n);
  for i := 0 to n - 1 do read(arr[i]);
  notDecr := True;
  firstViol := -1;
  for i := 1 to N - 1 do begin
    if arr[i] < arr[i - 1] then begin
      notDecr := False;
      firstViol := i;
      break;
    end;
  end;
  if notDecr then writeln(0)
  else writeln(firstViol);
end.