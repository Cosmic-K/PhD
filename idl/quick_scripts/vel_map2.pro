pro vel_map2,data,map
x=findgen(15)
sz=size(data)

x=[ -0.94200000,-0.58000000,-0.39800000,-0.29000000,-0.21700000,-0.14500000,-0.073000000,0.0000000,0.073000000,0.14500000,0.21700000,0.29000000,0.39800000,0.58000000,0.94200000]+854.2


map=fltarr(sz(1),sz(2));,sz(3))

FOR e=0 ,  sz(1)-1     DO BEGIN
FOR j=0  , sz(2)-1     DO BEGIN
;FOR k=0  , sz(2)-1     DO BEGIN

d=reform(data[e,j,*,0])

const=max(d)

;d=d-const

meas_size=7
start_val=2
grad=300000
len=meas_size/2

m=moment(d,sdev=sd)
der=randomn(seed,n_elements(d))*sd
;der=sqrt(d)

plot,x,d


FOR i=start_val, (n_elements(d)-start_val-1) DO BEGIN
print,i
minval=min(d[i-2:i+2],xi)
;print,xi

;Wait till maximum is at centre of search bar
IF xi EQ 2 THEN BEGIN
vline,x[i-2]
vline,x[i+2]


in1=x[i-2:i]
in2=x[i:i+2]

res=linfit(in1,d[i-2:i],yfit=fit)
res2=linfit(in2,d[i:i+2],yfit=fit2)

print,res[1],res2[1]


IF (res[1] LT -1*grad) AND (res2[1] GT grad) THEN BEGIN


x2=x[(i-len):(i+len)]



p=[min(d[(i-len):(i+len)],n),x2[n],0.22];(x[i+len]-x[i-len])]
;par_in = replicate({fixed:0, limited:[0,0], limits:[0.D,0.D]},3)
;par_in(2).fixed = 1

;print,p
f=mpfitpeak(x2,d[(i-len):(i+len)],res,nterms=4,errors=der[(i-len):(i+len)],perror=perr,/quiet,/gaussian)
;res=mpfitfun('mygauss',x2,d[(i-len):(i+len)],der[(i-len):(i+len)],p,/quiet,perror=perr,parinfo=par_in)
print,res

ENDIF
ENDIF
ENDFOR

print,e,j

;plot,x,d+const

;cgplot,x2,mygauss(x2,res)+const,linestyle=3,thick=4,/overplot,color='red',/window
oplot,x2,f,linestyle=3,thick=4
vline,res(1)
;oploterror,res(1),-1*res(0)+const,perr(1),perr(0),psym=2

pause
;mu=res(1)
;ref_spec=7.20768

;delta_lambda=ref_spec-mu

;v=vlos(ref_spec,delta_lambda)/1000.

;map[i,j]=v;-1*res(0)+const

ENDFOR
ENDFOR

END