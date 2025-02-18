program first;
var
  n, m_cur, m_prev, cur_cnt, max_cnt: integer;
  i: integer;
begin
  read(n);
  read(m_prev);
  cur_cnt := 1;
  max_cnt := 1;
  for i := 0 to n-2 do begin
    read(m_cur);
    if m_cur > m_prev then begin
      cur_cnt := cur_cnt + 1;
      if cur_cnt > max_cnt then max_cnt := cur_cnt;
    end
    else cur_cnt := 1;
    m_prev := m_cur;
  end;
  writeln(max_cnt);
end.