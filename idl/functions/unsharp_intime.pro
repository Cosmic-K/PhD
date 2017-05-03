function unsharp_intime,dat,dxx
sz=size(dat)
out_dat=fltarr(sz(1),sz(2),sz(3))

FOR i=0, sz(3)-1 DO BEGIN 

out_dat[*,*,i]=unsharp(data=dat[*,*,i],dx=dxx)

ENDFOR


return, out_dat
END