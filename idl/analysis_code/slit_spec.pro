PRO slit_spec,data1,data2,data3=data3,n,wt_ar,td,td_disp,td_v=td_v,tdv_disp=tdv_disp

spline_slit,data1,pickn=n,slits=td,distance=15,spacing=1,gpoints=gp,oversample=2,intim=data1(*,*,0),/vector

IF n_elements(data3) NE 0 THEN BEGIN
spline_slit,data3,slits=td_v,distance=25,spacing=1,gpoints=gp,oversample=2
sz1=size(td_v)
ENDIF

print,gp[0,0],gp[0,1]
wt_ar=fltarr(15,81,n_elements(gp)/2)

FOR j=0, (n_elements(gp)/2) -1 do begin
wave_time,data2,wt,x=gp[j,0],y=gp[j,1]
wt_ar[*,*,j]=wt
endfor

sz=size(td)
ch=''
print,'Number of time series: ',strtrim(sz(3),1)
READ,ch,PROMPT='Prime?'
IF ch EQ 'y' THEN BEGIN 
print,'1 less',strtrim(sz(3),1)-1 
READ,dt,PROMPT='Value for summing over?'
td_disp=fltarr(sz(1),sz(2),float(sz(3)-1/dt))
IF n_elements(data3) NE 0 THEN tdv_disp=fltarr(sz1(1),sz1(2),float(sz1(3)-1/dt))
mo=2
ENDIF ELSE BEGIN
READ,dt,PROMPT='Value for summing over?'
td_disp=fltarr(sz(1),sz(2),float(sz(3)/dt))
IF n_elements(data3) NE 0 THEN tdv_disp=fltarr(sz1(1),sz1(2),float(sz1(3)/dt))
mo=1
ENDELSE


FOR i=0,sz(3)-mo,dt DO BEGIN
IF n_elements(data3) eq 0 THEN BEGIN td_disp(*,*,i/dt)=sum(td[*,*,i:i+dt-1],2) 
ENDIF ELSE BEGIN 
td_disp(*,*,i/dt)=sum(td[*,*,i:i+dt-1],2) 
tdv_disp(*,*,i/dt)=sum(td_v[*,*,i:i+dt-1],2)
ENDELSE

ENDFOR

tvim,unsharp(data=rotate(td_disp[*,*,2],3),dx=12),aspect=7

window,1
tvim,unsharp(data=wt_ar(*,*,0),dx=10)

save,wt_ar,description='Spectral profile over time at each spline point',$
filename='wt_ar_'+strtrim(round(gp[0,0]),2)+strtrim(round(gp[0,1]),2)+'.idl'
save,td,description='t/d array',$
filename='td_'+strtrim(round(gp[0,0]),2)+strtrim(round(gp[0,1]),2)+'.idl'
save,td_disp,description='averaged t/d array',$
filename='tddisp_'+strtrim(round(gp[0,0]),2)+strtrim(round(gp[0,1]),2)+'.idl'

IF n_elements(data3) NE 0 THEN BEGIN
save,td_v,description='t/d array of velocity 10 bigger fov',$
filename='tdv_'+strtrim(round(gp[0,0]),2)+strtrim(round(gp[0,1]),2)+'.idl'

save,tdv_disp,description='averaged t/d array of velocity 10 bigger fov',$
filename='tddispv_'+strtrim(round(gp[0,0]),2)+strtrim(round(gp[0,1]),2)+'.idl'
ENDIF

END



