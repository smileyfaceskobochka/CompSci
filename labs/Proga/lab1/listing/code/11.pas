program eleven;
var k1, b1, k2, b2, e: integer;
    x, y, d: real;
begin
    read(k1, b1, k2, b2, e);
    if k1 = k2 then begin
        writeln('No');
        exit;
    end;
    x := (b2 - b1) / (k1 - k2);
    y := k1 * x + b1;
    d := sqrt(x*x + y*y);
    if (d <= e) then writeln('Yes')
    else writeln('No');
end.