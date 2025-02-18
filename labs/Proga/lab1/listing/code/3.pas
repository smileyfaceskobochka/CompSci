program third;
var
    n, a, b, c, temp: integer;
begin
    readln(n);
    a := n div 100;
    b := (n div 10) mod 10;
    c := n mod 10;
    if a > b then begin
        temp := a;
        a := b;
        b := temp;
    end;
    if b > c then begin
        temp := b;
        b := c;
        c := temp;
    end;
    if a > b then begin
        temp := a;
        a := b;
        b := temp;
    end;
    if ((a + c) div 2 < b) then writeln('YES')
    else writeln('NO');
end.
