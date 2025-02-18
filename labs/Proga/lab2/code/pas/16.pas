var
    n, i, newSize: Integer; // Кол-во эоементов в массиве, счетчик, размер нового массива
    arr: array of Integer; // Исходный массив
    newArr: array of Integer; // Новый массив
begin
    readln(n);
    SetLength(arr, n);
    SetLength(newArr, n);
    for i := 0 to n - 1 do begin
        read(arr[i]);
    end;

    newSize := 0;

    for i := 0 to n - 2 do begin
        if arr[i] < arr[i + 1] then begin
            newArr[newSize] := arr[i];
            newSize := newSize + 1;
        end;
    end;

    if newSize = 0 then begin
        writeln('NO');
    end
    else begin
        writeln(newSize);
        for i := 0 to newSize - 1 do begin
            write(newArr[i], ' ');
        end;
        writeln;
    end;
end.