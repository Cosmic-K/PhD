;Krishna Mooroogen
;Northumbria University Solar Physics
;krishna.mooroogen@northumbria.ac.uk
;Supervisor:Richard Morton

;CODE TO MEASURE WAVES PROPERTIES USING TRACK WAVES AS A BASE
;DOES TRACK WAVES, FITS SINE TO INTENSITY AND FINDS THREADS
;USING INTENSITY THREADS TO MAKE MASK FOR VELOCITY DATA
;TAKES VELOCITY PROJECTION AND FITS SINE TO IT
;CALCULATES PHASE VELOCITY,AMPLITUDE, PERIOD FROM FITS
;PEAK DETECTOR WILL SPEED UP PEAK FITTING

pro wave_measure,int_ts,vel_ts,fit,int_info,vel_info

sz=size(int_ts)
a=3.55463e-7
b=-0.001
c=1.178

Mm = (0.725/16.981891892)
sz = size(im_in)
xsz = Mm*(float(sz(1))-1.0)
ysz = Mm*(float(sz(2))-1.0)


time = 1.343

int_time_max=0
int_time_pos_max=0

int_time=0
int_time_pos=0

int_amplitudes=0
int_periods=0

vel_amp=0
vel_per=0

vel_time_max=0
vel_time_pos_max=0

vel_time=0
vel_time_pos=0

vel_amplitudes=0
vel_periods=0

width=3

int_tube_list=0
vel_tube_list=0

t_xer=0
t_yer=0

vel_time_xer = 0
vel_time_yer = 0




FOR i=0,(sz(3)-1) DO BEGIN

print, 'YOU ARE MEASURING TIME SERIES NUMBER: ',i

int_amp=0
int_per=0

ts_in = int_ts(*,*,i)
ts_er = ((a*ts_in^2)+(b*ts_in)+c)

track_saus_test,data=(-1.*ts_in),errors=ts_er,fit,threads


print,'#####################################'
print,'END OF FITTING BEGINING TUBE MASKING'
print,'Enter tube bounderies for each thread to complete the structure. Do not exceed boundary limit.'

sin_pos=fit.fit_result_pos
sin_pos=sin_pos[*,1:*]

pos=threads.pos
pos_er=threads.err_pos
pos=pos[*,1:*]
pos_er=pos_er[*,1:*]
pos_size=size(pos)

;nan=where(pos_er EQ -1)
;pos_er(nan)=max(pos_er)


tube=0
st=0
en=0

IF n_elements(pos_size) EQ 4 THEN loopend = 0
IF n_elements(pos_size) EQ 5 THEN loopend = (pos_size(2)-1)

FOR j=0, loopend DO BEGIN
print, 'Thread start: ',sin_pos(11,j),'  Thread end: ',sin_pos(12,j)
ENDFOR


FOR j=0,loopend DO BEGIN
READ,st,PROMPT='Start? '
READ,en,PROMPT='End? '
tube=[temporary(tube),pos(st:en,j)]

ENDFOR

tube=tube[1:*]

index1=where(tube eq -1)
index0=where(tube eq 0)
index=[index1,index0]

tube(index)=0.7*max(tube)

index=index[sort(index)]

IF index(0) EQ 0 THEN index=index[1:*]
IF (n_elements(tube) EQ 228) AND (index(n_elements(index)-1) EQ 227) THEN index(n_elements(index)-1)=226


use_pairs=0
use_singles=0

last=(n_elements(index)-1)

FOR j=0, last DO BEGIN


IF j EQ 0 THEN BEGIN
IF (index(j) EQ index(j+1)-1) THEN use_pairs=[temporary(use_pairs),index(j)] ELSE BEGIN
IF (index(j) NE index(j+1)-1) THEN use_singles=[temporary(use_singles),index(j)]
ENDELSE
ENDIF

IF (j NE 0) AND (j NE last) THEN BEGIN
IF (index(j) EQ index(j+1)-1) AND (index(j) NE index(j-1)+1) THEN use_pairs=[temporary(use_pairs),index(j)]
IF (index(j) EQ index(j-1)+1) AND (index(j) NE index(j+1)-1) THEN use_pairs=[temporary(use_pairs),index(j)]
IF (index(j) NE index(j-1)+1) AND (index(j) NE index(j+1)-1) THEN use_singles=[temporary(use_singles),index(j)]
ENDIF

IF j EQ last THEN BEGIN
IF (index(j) NE index(j-1)+1) THEN use_singles=[temporary(use_singles),index(j)] ELSE BEGIN
IF (index(j) EQ index(j-1)+1) THEN use_pairs=[temporary(use_pairs),index(j)]
ENDELSE
ENDIF

ENDFOR

use_pairs=use_pairs[1:*]
use_singles=use_singles[1:*]

pairs=intarr(n_elements(use_pairs))
singles=intarr(2*n_elements(use_singles))

pairs[0:*:2]=use_pairs[0:*:2]-1
pairs[1:*:2]=use_pairs[1:*:2]+1

singles[0:*:2]=use_singles-1
singles[1:*:2]=use_singles+1


num=findgen(float(n_elements(pairs))/2.0)*2

IF pairs(n_elements(pairs)-1) EQ 229 THEN pairs(n_elements(pairs)-1)=228


FOR j=0,n_elements(num)-1 DO BEGIN
spline_p,[pairs(num(j)),pairs(num(j)+1)],[tube(pairs(num(j))),tube(pairs(num(j)+1))],xpos,ypos,interval=1.3
tube(xpos[1:(n_elements(xpos)-2)])=ypos[1:(n_elements(xpos)-2)]
ENDFOR


num=findgen(float(n_elements(singles))/2.0)*2

FOR j=0,(n_elements(num)-1) DO BEGIN
spline_p,[singles(num(j)),singles(num(j)+1)],[tube(singles(num(j))),tube(singles(num(j)+1))],xpos,ypos,interval=sqrt(2.5)
tube(xpos[1:(n_elements(xpos)-2)])=ypos[1:(n_elements(xpos)-2)]
ENDFOR

IF n_elements(tube) GT 228 THEN tube=tube[0:(n_elements(tube)-2)];remove padding check if this needs to be chnages when tube threads is more than 3

int_tube_list=[temporary(int_tube_list),tube]


FOR j=0, loopend DO BEGIN
int_amp = [temporary(int_amp),sin_pos(1,j)]
int_per = [temporary(int_per),sin_pos(2,j)]
print,sin_pos(*,j)
ENDFOR

int_amplitudes=[temporary(int_amplitudes),int_amp[1:*]]
int_periods=[temporary(int_periods),int_per[1:*]]

fit_check=''

print,'#################################'
print,'INTENSITY'
print,'Fitting peaks of structure, define peak region'
WHILE fit_check NE 'y' DO BEGIN

x=findgen(228)
plot,tube

check=''

WHILE check NE 'y' DO BEGIN

print,'Where would you like to cut?'
READ,cutst,PROMPT='Start of cut? '
READ,cuten,PROMPT='End of cut? '

res=poly_fit(x[cutst:cuten],tube[cutst:cuten],2,chisq=chi,yfit=fit,sigma=sigma)
print, 'Fit results: ',res
print,'Errors',sigma
print,'ChiSq: ',chi


loadct,2,/silent
oplot,x[cutst:cuten],fit,linestyle=1,color=100

x_cen=float(-1*res(1))/float(2*res(2))
delx=x_cen*sqrt((float(sigma(1))/float(res(1)))^2+(float(sigma(2))/float(res(2)))^2)

y_cen=res(2)*x_cen^2+res(1)*x_cen+res(0)
dely=sqrt((((2*res(2)*x_cen)+res(1))^2)*delx^2)

plots,x_cen,y_cen,psym=1,color=100
loadct,0,/silent

READ,check,PROMPT='Happy with fit? '

ENDWHILE
m=max(tube[cutst:cuten],o)
x_time=x(o)

int_time_max=[temporary(int_time_max),x_time]
int_time_pos_max=[temporary(int_time_pos_max),m]
int_time =[temporary(int_time),x_cen]
int_time_pos = [temporary(int_time_pos),y_cen]
t_xer=[temporary(t_xer),delx]
t_yer=[temporary(t_yer),dely]

READ,fit_check,PROMPT='Are you done fitting? '

ENDWHILE


print,'#################################'
print,'VELOCITY'

ts_vel = rotate(vel_ts(*,*,i),4)

y1=tube+width
y2=tube-width

szv=size(ts_vel)

FOR k=0,(szv(1)-1) DO BEGIN
FOR l=0,(szv(2)-1) DO BEGIN
IF l LT y2(k) THEN ts_vel(k,l)=0 ELSE ts_vel(k,l)=ts_vel(k,l)
ENDFOR
ENDFOR

FOR k=0,(szv(1)-1) DO BEGIN
FOR l=0,(szv(2)-1) DO BEGIN
IF l GT y1(k) THEN ts_vel(k,l)=0 ELSE IF ts_vel(k,l) EQ 0 THEN ts_vel(k,l)=ts_vel(k,l)
ENDFOR
ENDFOR

av=fltarr(szv(1))
FOR j=0,(szv(1)-1) DO BEGIN
in=where(ts_vel(j,*) NE 0,count)
av(j)=count
ENDFOR

ts_vel_sum=float(sum(ts_vel,1))/float(av)

vel_tube_list=[temporary(vel_tube_list),ts_vel_sum]


tsv_er=0

FOR j=0,(szv(1)-1) DO BEGIN
a = ts_vel(j,*)
sd=stdev(a[where(a NE 0)])
tsv_er=[temporary(tsv_er),sd]
ENDFOR

tsv_er=tsv_er[1:*]

fit_check=''

WHILE fit_check NE 'y' DO BEGIN

x=findgen(228)
plot,ts_vel_sum

check=''

WHILE check NE 'y' DO BEGIN

print,'Where would you like to cut?'
READ,cutst,PROMPT='Start of cut? '
READ,cuten,PROMPT='End of cut? '

res=poly_fit(x[cutst:cuten],ts_vel_sum[cutst:cuten],2,measure_errors=(tsv_er[cutst:cuten]),chisq=chi,yfit=fit)
print, 'Fit results: ',res
print,'Errors',sigma
print,'ChiSq: ',chi

loadct,2,/silent
oplot,x[cutst:cuten],fit,linestyle=1,color=100

x_cen=float(-1*res(1))/float(2*res(2))
vdelx=x_cen*sqrt((float(sigma(1))/float(res(1)))^2+(float(sigma(2))/float(res(2)))^2)

y_cen=res(2)*x_cen^2+res(1)*x_cen+res(0)
vdely=sqrt((((2*res(2)*x_cen)+res(1))^2)*vdelx^2)


plots,x_cen,y_cen,psym=1,color=100
loadct,0,/silent

READ,check,PROMPT='Happy with fit? '

ENDWHILE
m=max(ts_vel_sum[cutst:cuten],o)
x_time=x(o)

vel_time_max=[temporary(vel_time_max),x_time]
vel_time_pos_max=[temporary(vel_time_pos_max),m]
vel_time =[temporary(vel_time),x_cen]
vel_time_pos = [temporary(vel_time_pos),y_cen]
vel_time_xer = [temporary(vel_time_xer),vdelx]
vel_time_yer= [temporary(vel_time_yer),vdely]

READ,fit_check,PROMPT='Are you done fitting? '

ENDWHILE


velcheck=''
READ,velcheck,PROMPT='Investigate velocity structure?'
IF velcheck EQ 'y' THEN BEGIN
mg_loadct,22,/silent
tvim,ts_vel,/rct
loadct,0,/silent
READ,t,PROMPT='Where in time do you want to look?'
window,1
plot,ts_vel(t,*)
dine=''
READ,done,PROMPT='Are you done? y/n '
;Eventually add something here to do stuff to it
ENDIF

print,'################################'
print,'Sine fititng to velocity profile'

x2=findgen(228)*time
plot,x2,ts_vel_sum

fitdn=''
WHILE fitdn NE 'y' DO BEGIN


param=[min(ts_vel_sum),1.,20.,0.5,1.]
print,'Initial variables:',' constant', param[0],' amplitude',param[1],$
'period',param[2],' phase',param[3],' linear',param[4]

st=''
READ,st,PROMPT='Change start variables? y/n '

IF st EQ 'y' THEN BEGIN

READ,new2,PROMPT='Enter amplitude: '
READ,new3,PROMPT='Enter period: '
param=[param[0],new2,new3,param[3],param[4]]

ENDIF


res=mpfitfun('mysin',x2,ts_vel_sum,tsv_er,param,perror=perror,bestnorm=bestnorm,/quiet)
yy=mysin(x2,res)
loadct,2,/silent
oplot,x2,yy,linestyle=1,color=100
loadct,0,/silent

print,'%'
print,'%'
print,'Fitted variables',res
print,'Error on fits',perror
print,'Chi^2', bestnorm

READ,fitdn,PROMPT='Are you happy with the fit?'


ENDWHILE

print,res,perror
vel_amp=[temporary(vel_amp),res(1),perror(1)]
vel_per=[temporary(vel_per),res(2),perror(2)]



ENDFOR

print, 'Intensity amplitudes by times series and by thread in Mm: ',int_amplitudes[1:*]*Mm
print, 'Intesnsity periods by time series and by thread in seconds: ',int_periods[1:*]*time

print, 'Velocity amplitudes by times series and by thread in km/s: ',vel_amp[1:*];km/s?
print, 'Velocity periods by time series and by thread in seconds: ',vel_per[1:*]*time



int_info={tube_list:(int_tube_list[1:*]*Mm), int_time_max:int_time_max[1:*]*time, int_time_pos_max:int_time_pos[1:*]*Mm, int_time:int_time[1:*]*time, $
int_time_pos:int_time_pos[1:*]*Mm,time_xer:t_xer[1:*]*time,time_yer:t_yer[1:*]*Mm}

vel_info={vel_tube_list:vel_tube_list[1:*], vel_time_max:vel_time_max[1:*]*time, vel_time_pos_max:vel_time_pos_max[1:*], vel_time:vel_time[1:*]*time, $
vel_time_pos:vel_time_pos[1:*], amp:vel_amp[1:*], period:vel_per[1:*]*time,vel_time_xer:vel_time_xer[1:*]*time,vel_time_yer:vel_time_yer[1:*]*Mm}






END