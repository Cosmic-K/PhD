pro vel_map2,data,map,coords
sz=size(data)
ref_spec=854.2

x=[ -0.94200000,-0.58000000,-0.39800000,-0.29000000,-0.21700000,-0.14500000,-0.073000000,0.0000000,0.073000000,0.14500000,0.21700000,0.29000000,0.39800000,0.58000000,0.94200000]+8542
coords=0

map=fltarr(sz(1),sz(2));,sz(3))

FOR e=0   , sz(1)-1     DO BEGIN
FOR j=0 , sz(2)-1     DO BEGIN
;FOR k=0  , sz(2)-1     DO BEGIN

d=reform(data[e,j,*,0])

const=max(d)
d=d-const
d_cut=d[3:11]
x_cut=x[3:11]


meas_size=7
start_val=2
grad=190000
len=2;meas_size/2

;plot,x,d+const

FOR i=start_val, (n_elements(d_cut)-start_val-1) DO BEGIN
;print,'i ',i
minval=min(d_cut[i-2:i+2],xi)

;Wait till maximum is at centre of search bar
IF xi EQ 2 THEN BEGIN
;vline,x_cut[i-2]
;vline,x_cut[i+2]
;pause
in1=x_cut[i-2:i]
in2=x_cut[i:i+2]

res1=linfit(in1,d_cut[i-2:i],yfit=fit)
res2=linfit(in2,d_cut[i:i+2],yfit=fit2)

;print,res1[1],res2[1]
IF (res1[1] LT -1*grad) AND (res2[1] GT grad) THEN BEGIN

minpos=i-len
maxpos=i+len

;print,'mm ',minpos,maxpos

x2=x_cut[minpos:maxpos]/10.0
d2=d_cut[minpos:maxpos]

m=moment(d_cut[minpos:maxpos],sdev=sd)
der=randomn(seed,n_elements(d2))*sd


line_centroid,x2,d2,cent,/silent

p=[min(d2,n),cent,0.8,600]

;print,'p ',p

f=mpfitpeak(x2,d2,res,nterms=4,errors=der,estimates=p,perror=perr,/quiet,/moffat,/negative,chisq=chi)
;print,'res ',res
;print,chi

IF chi GT 1e8 THEN BEGIN
p1=[min(d2,n),cent,0.6,600]
f=mpfitpeak(x2,d2,res,nterms=4,errors=der,estimates=p1,perror=perr,/quiet,/moffat,/negative,chisq=chi)
;print,chi

ENDIF

IF chi GT 1e8 THEN BEGIN
p2=[min(d2,n),cent,0.5,600]
f=mpfitpeak(x2,d2,res,nterms=4,errors=der,estimates=p2,perror=perr,/quiet,/moffat,/negative,chisq=chi)
;print,chi

ENDIF

ENDIF
ENDIF
ENDFOR

;print,e,j

IF (res1[1] LT -1*grad) AND (res2[1] GT grad) THEN BEGIN
;plot,x/10.,d+const
;oplot,x2,f+const,linestyle=3,thick=3
;oploterr,res(1),res(0)+const,perr(1),perr(0),psym=5
mu=res(1)
delta_lambda=ref_spec-mu
v=vlos(ref_spec,delta_lambda)/1000.
map[e,j]=v
;print,v
;pause

ENDIF ELSE BEGIN
map[e,j]=-1.
;print,map[i,j]
coords=[temporary(coords),e,j]
;print,'raa'
ENDELSE
ENDFOR
ENDFOR
coords=coords[1:*]

END