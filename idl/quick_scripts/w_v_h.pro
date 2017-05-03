;Author: Krishna Mooroogen
;Institution: Northumbria University 
;Date: 16/05/16
;Dexcription fit gaussian to td diagrams to determine width as an average 
;input array of td diagrams
;out plot widht against height makesure td orientted with time in x 

pro w_v_h, data ,plot_=plot_

sz=size(data)
z=findgen(sz(3))
width=fltarr(sz(3))
w_er=fltarr(sz(3))
a=8.2064e-7
b=-0.00055
c=0.087


FOR i=0, sz(3)-1 DO BEGIN
temp=fltarr(10)
temp_er=fltarr(10)
FOR j=0, 9 DO BEGIN
d=reform(data[60+j,*,i])
;d=d[mins:maxs]
x=findgen(N_ELEMENTS(d))
errors=exp((a*d^2)+(b*d)+c)

r=gaussfit(x,d,coeff,measure_errors=errors,nterms=3,sigma=sig)

IF KEYWORD_SET(plot_) THEN BEGIN 
plot,x,d
oplot,x,r,linestyle=3 
pause
ENDIF

temp[j]=coeff(2)
temp_er[j]=sig(2)
ENDFOR

width[i]=total(abs(temp)/(temp_er^2))/total(1/(temp_er^2))
w_er[i]=sqrt(1./total(1./temp_er^2))

ENDFOR


p = errorplot(z,width,w_er)


END