PRO slit_spec,data1,data2,n,wt,td,td_disp

spline_slit,data1,pickn=n,slits=td,distance=15,gpoints=gp,intim=data1(*,*,0),/vector

wave_time,data2,wt,x=gp[0,0],y=gp[0,1]

sz=size(td)
print,'Number of time series: ',strtrim(sz(3),1)

READ,dt,PROMPT='Value for summing over?'

td_disp=fltarr(sz(1),sz(2),float(sz(3)/dt))

idx=findgen(sz(3)/dt)*dt
idx=[0,idx[1:*]-1]

FOR i=0, n_elements(idx)-2 DO BEGIN
IF i EQ 0 THEN BEGIN
td_disp(*,*,i)=sum(td[*,*,idx[i]:idx[i+1]],2)
ENDIF ELSE BEGIN
td_disp(*,*,i)=sum(td[*,*,idx[i]+1:idx[i+1]],2)
ENDELSE
ENDFOR

;PRIME NUMBERS HADLING JUST DO IT AGAIN? MAYBE JUST FOR DISPLAY DONT WORRY ABOUT GETTING IT RIGHT
;HOW TO AVERAGE WAVE?



tvim,unsharp(data=rotate(td_disp[*,*,2],3),dx=12),aspect=7

window,1
tvim,unsharp(data=wt,dx=10)


END

;FOR i=0, n_elements(idx)-2 DO $ IF i EQ 0 THEN td_disp(*,*,i)=sum(td[*,*,idx[i]:idx[i+1]],2) $ ELSE td_disp(*,*,i)=sum(td[*,*,idx[i]+1:idx[i+1]],2)

