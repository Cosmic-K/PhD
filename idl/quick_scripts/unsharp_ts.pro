pro unsharp_ts,dat,dat_out,d=d

sz=size(dat)
dat_out=fltarr(sz(1),sz(2),sz(3))

FOR i=0, sz(3)-1 DO BEGIN

dum=unsharp(data=dat[*,*,i],dx=d)
dat_out[*,*,i]=dum

ENDFOR
END