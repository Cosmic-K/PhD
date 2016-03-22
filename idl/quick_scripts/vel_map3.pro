pro vel_map3,data,map,coords

sz=size(data)

x=([-0.29000000,-0.21700000,-0.14500000,-0.073000000,0.0000000,0.073000000,0.14500000,0.21700000,0.29000000]+8542)/10.0
coords=0

map=fltarr(sz(1),sz(2),sz(4), /nozero)
grad=10000

FOR k=0 , sz(4)-1     DO BEGIN
FOR j=0 , sz(2)-1     DO BEGIN
FOR e=0 , sz(1)-1     DO BEGIN

d_cut=reform(data[e,j,0:8,k])
const=max(d_cut)
d_cut=d_cut-const

minval=min(d_cut,i)

IF i gt 6 then i = 6
IF i lt 2 then i = 2
in=i

res1=(d_cut[in]-d_cut[in-2])/(x[in]-x[in-2])
res2=(d_cut[in+2]-d_cut[in])/(x[in+2]-x[in])

IF (res1 LT -1*grad) AND (res2 GT grad) THEN BEGIN

x2=x[in-2:in+2]
d2=d_cut[in-2:in+2]
der=sqrt(d2)

cent=total(double(d2)^2*x2)/total(double(d2)^2)

p=[min(d2,n),cent,0.8,600]

f=mpfitpeak(x2,d2,res,nterms=4,errors=der,estimates=p,perror=perr,/quiet,/moffat,/negative,chisq=chi,niter=5)

map[e,j,k]=res(1)

ENDIF ELSE BEGIN
map[e,j,k]=854.2
coords=[temporary(coords),e,j,k]

ENDELSE
ENDFOR
ENDFOR
ENDFOR
coords=coords[1:*]
;outputs cent positions still need to chnage the positions that fail then run through vlos
END