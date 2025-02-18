Program seven;
var n, m_cur, m_prev, cnt, i: longint;
begin
    read(n);
    read(m_prev);
    cnt := 0;
    for i := 0 to n - 2 do begin
        read(m_cur);
        if ((m_cur > 0) and (m_prev < 0)) or
            ((m_cur < 0) and (m_prev > 0)) then
            cnt := cnt + 1
        m_prev := m_cur;
    end;
    write(cnt)
end.
