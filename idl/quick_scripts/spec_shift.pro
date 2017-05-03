;Krishna Mooroogen 
;17/10/16
;code to examine spectral wing shift along height of features
;input 4d data x,y,lamda,t
;comapares shift in wing with average profile 
;positive means broder over the av profile neg means shrunk behind the av profile 
;or pos is less intensity neg is more 

PRO spec_shift,data,out,pickn=pickn,time=time,timeseries=timeseries
;calculate average profile
s=size(data)
avtime=total(data,4)/s(4)
avsp1=total(avtime,1)/s(1)
avsp2=total(avsp1,1)/s(2)
avprof=reform(avsp2)

cent=3
st=2
en=4
dt=1.343

IF n_elements(time) EQ 0 THEN time=0

;can do by time rathe rthan by height
IF keyword_set(timeseries) THEN BEGIN
t=findgen(s(4))*dt
red=fltarr(s(4))
blue=fltarr(s(4))
tvim,data[*,*,cent,time]
pick,x,y
timspec=reform(data[x,y,*,*])

FOR i=0, s(4)-1 DO BEGIN

blue[i]=mean(timspec[0:st,i]-avprof[0:st])
red[i]=mean(timspec[en:*,i]-avprof[en:*])

ENDFOR

p1=plot(t,blue,color='blue',xtitle='Time (s)',ytitle='index',xrange=[0,310])
p2=plot(t,red,color='red',/over,/curr)
out=[[blue],[red]]

ENDIF ELSE BEGIN

IF n_elements(pickn) EQ 0 THEN pickn=6
red=fltarr(pickn)
blue=fltarr(pickn)
FOR i = 0, pickn -1 DO BEGIN
tvim,data[*,*,3,time]
pick,x,y
prof=data[x,y,*,time]
blue[i]=mean(prof[0:2]-avprof[0:2])
red[i]=mean(prof[3:*]-avprof[3:*])
;plot,avprof
;oplot,prof,linestyle=3
;pause
ENDFOR

p1=plot(blue,color='blue',xtitle='Arbituary height along feature',ytitle='index')
p2=plot(red,color='red',/over,/curr)

ENDELSE

END