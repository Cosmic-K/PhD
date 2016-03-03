pro vel_map,data,map
x=findgen(15)
sz=size(data)

x=[ -0.94200000,-0.58000000,-0.39800000,-0.29000000,-0.21700000,-0.14500000,-0.073000000,0.0000000,0.073000000,0.14500000,0.21700000,0.29000000,0.39800000,0.58000000,0.94200000]+8542


map=fltarr(sz(1),sz(2));,sz(3))

FOR i=0 ,  sz(1)-1     DO BEGIN
FOR j=0  , sz(2)-1     DO BEGIN
;FOR k=0  , sz(2)-1     DO BEGIN

d=reform(data[i,j,*,0])
const=max(d)
d=d-const

d_cut=d[3:11]
x_cut=x[3:11]

;minval=min(d_cut,xi)
;minpos=xi-4
;maxpos=xi+4
xdex=findgen(15)
xdex=xdex[3:11]

;print,minpos,maxpos

;IF maxpos EQ 9 THEN maxpos=8
;IF minpos LT 0 THEN minpos=0

line_centroid,xdex,d_cut,cent,wrange=xdex[0:8],min=min(d_cut),/silent

minpos=round(cent-2)
maxpos=round(cent+2)

IF maxpos EQ 9 THEN maxpos=8
IF minpos LT 0 THEN minpos=0

d2=d_cut[minpos:maxpos]
x2=x_cut[minpos:maxpos]/10.




diff=maxpos-minpos+1

m=moment(d2,sdev=sd)
der=randomn(diff)*sd


;p=[min(d2,n),x2[n],0.022,1e4]

p=[min(d2,n),x2[n],0.7,600]
;print,p

f=mpfitpeak(x2,d2,a,nterms=4,errors=der,estimates=p,perror=perr,/quiet,/moffat)
;res=mpfitfun('mygauss',x[minpos:maxpos],d[minpos:maxpos],der,p,/quiet,perror=perr,parinfo=par_in)
;print,a

print,x2

;print,i,j

plot,x/10.,d+const

;oplot,x[minpos:maxpos],mygauss(x[minpos:maxpos],res),linestyle=3,thick=4
oplot,x2,f+const,linestyle=3,thick=3
oploterr,a(1),a(0)+a(3)+const,perr(1),perr(0),psym=5

;print,res(0)+const


pause
;mu=res(1)
;ref_spec=7.20768

;delta_lambda=ref_spec-mu

;v=vlos(ref_spec,delta_lambda)/1000.

;map[i,j]=v;-1*res(0)+const

ENDFOR
ENDFOR

END