var a, b, x, y, z: longint;
begin
    readln(a, b, x, y, z);
    if (x*z <= a*b) or (y*z <= a*b) or (x*y <= a*b) then
        writeln('Yes')
    else
        writeln('No');
end.