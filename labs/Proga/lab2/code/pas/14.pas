var
    n, i, j, prev: integer;
    arr: array of integer;
    flag, nigg: boolean;
begin
    readln(n);
    SetLength(arr, n);

    nigg := True;
    
    for i := 0 to n-1 do begin
        read(arr[i]);
    end;

    j := 1;
    flag := True;
    prev := arr[0];
    if arr[0] < 0 then nigg := False;

    while (arr[j] >= 0) do begin
        if (arr[j] >= prev) then flag := False;
        prev := arr[j];
        j := j + 1
    end;

    if (flag and nigg) = True then writeln('Yes')
    else writeln('No')
end.