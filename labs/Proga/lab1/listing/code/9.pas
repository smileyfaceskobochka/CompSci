Program nine;
var n, m, mft_first, mft_mid, mft_last, mft, mlt_first,
    mlt_mid, mlt_last, mlt, mft_sum, mlt_sum, i, cnt: longint;
begin
    read(n);
    read(m);
    cnt := 0;
    for i := n to m do
    begin
        mlt := i mod 1000;
        mft := i div 1000;
        mft_first := mft div 100;
        mft_mid := (mft div 10) mod 10;
        mft_last := mft mod 10;
        mft_sum := mft_first + mft_mid + mft_last;
        mlt_first := mlt div 100;
        mlt_mid := (mlt div 10) mod 10;
        mlt_last := mlt mod 10;
        mlt_sum := mlt_first + mlt_mid + mlt_last;
        if mft_sum = mlt_sum then
            cnt := cnt + 1
    end;
    write(cnt)
end.