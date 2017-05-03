pro plot_ts,x, ts, tser,i1,i2

km=42.692535

FOR i=i1, i2 do begin  
in=where(ts[*,i] ne -1)
tsi=ts[in,i]
tseri=tser[in,i]
xi=x[in]
IF i eq 0 THEN  p=errorplot(xi,tsi*km,tseri*km,/curr,/over,xrange=[-1,310],xtitle='Time (s)',ytitle='Displcement (km)') ELSE p=errorplot(xi,tsi*km+300*i,tseri*km,/curr,/over,xrange=[-1,310],xtitle='Time (s)',ytitle='Displcement (km)')


ENDFOR
END