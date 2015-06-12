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

pro wave_measure1,int_ts,vel_ts,int_info,vel_info
sz=size(int_ts)


Mm = (0.725/16.981891892)

time = 1.343

int_time=0
int_time_pos=0

vel_fit_res=fltarr(5)
vel_fit_er=fltarr(5)

vel_time_max=0
vel_time_pos_max=0

vel_time=0
vel_time_pos=0

vel_amplitudes=0
vel_periods=0

width=3

t=227

int_tube_list=fltarr(t)
int_terror=fltarr(t)
vel_tube_list=fltarr(227)
vel_terror=fltarr(227);chop using st en

t_xer=0
t_yer=0

vel_time_xer = 0
vel_time_yer = 0


FOR i=32,(sz(3)-1) DO BEGIN

print, 'YOU ARE MEASURING TIME SERIES NUMBER: ',i,' of ',(sz(3)-1)

int_amp=0
int_per=0

a=8.2064e-7
b=-0.00055
c=0.087

ts_in = int_ts(*,*,i)
ts_er = exp((a*ts_in^2)+(b*ts_in)+c)

track_saus,data=(-1.*ts_in),threads_fit_fg,threads,errors=ts_er,/patch


print,'#####################################'
print,'END OF FITTING BEGINING TUBE MASKING'
;print,'Enter tube bounderies for each thread to complete the structure. Do not exceed boundary limit.'

sin_pos=threads_fit_fg.fit_result_pos
sin_pos=sin_pos[*,1:*]

pos=threads_fit_fg.pos
pos_er=threads_fit_fg.err_pos
pos=pos[*,1:*]
pos_er=pos_er[*,1:*]
pos_size=size(pos)

tube=0
tube_er=0

IF N_ELEMENTS(pos_size) EQ 4 THEN loopend = 0
IF N_ELEMENTS(pos_size) EQ 5 THEN loopend = (pos_size(2)-1)

FOR j=0, loopend DO BEGIN
print, 'Thread start: ',sin_pos(11,j)*time,'  Thread end: ',sin_pos(12,j)*time
ENDFOR

IF loopend EQ 0 THEN BEGIN
tube=[temporary(tube),pos]
tube_er=[temporary(tube_er),pos_er]
tube=tube[1:*]
tube_er=tube_er[1:*]
ENDIF ELSE BEGIN

FOR j=0,loopend DO BEGIN
en=max(where(pos(*,j) NE -1))

IF j EQ 0 THEN BEGIN
tube=[temporary(tube),pos(0:en,j)]
tube_er=[temporary(tube_er),pos_er(0:en,j)]
ENDIF ELSE IF j EQ loopend THEN BEGIN
tube=[temporary(tube),pos(en1+1:*,j)]
tube_er=[temporary(tube_er),pos_er(en1+1:*,j)]

ENDIF ELSE IF (j NE 0) and (j NE loopend) THEN BEGIN
tube=[temporary(tube),pos(en1+1:en,j)]
tube_er=[temporary(tube_er),pos_er(en1+1:en,j)]
ENDIF

en1=en

ENDFOR


tube=tube[1:*]
tube_er=tube_er[1:*]


index=where(tube eq -1,count)

mxin=max(where(tube NE -1))
mnin=min(where(tube NE -1))

IF (count NE 0) THEN BEGIN

tube(index)=0.7*max(tube)

index=index[sort(index)]

index=index(where(index LT mxin AND index GT mnin))

;IF index(0) EQ 0 THEN index=index[1:*]
;IF (N_ELEMENTS(tube) EQ t+1) AND (index(N_ELEMENTS(index)-1) EQ t) THEN index(N_ELEMENTS(index)-1)=t-1

use_pairs=0
use_singles=0

last=(N_ELEMENTS(index)-1)

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

IF N_ELEMENTS(use_pairs) GT 2 THEN BEGIN
use_pairs=use_pairs[1:*]
pairs=intarr(N_ELEMENTS(use_pairs))
pairs[0:*:2]=use_pairs[0:*:2]-1
pairs[1:*:2]=use_pairs[1:*:2]+1

num=findgen(float(N_ELEMENTS(pairs))/2.0)*2

IF pairs(N_ELEMENTS(pairs)-1) EQ t+1 THEN pairs(N_ELEMENTS(pairs)-1)=t

FOR j=0,N_ELEMENTS(num)-1 DO BEGIN
spline_p,[pairs(num(j)),pairs(num(j)+1)],[tube(pairs(num(j))),tube(pairs(num(j)+1))],xpos,ypos,interval=1.3
tube(xpos[1:(N_ELEMENTS(xpos)-2)])=ypos[1:(N_ELEMENTS(xpos)-2)]
tube_er(xpos[1:(N_ELEMENTS(xpos)-2)])=ypos[1:(N_ELEMENTS(xpos)-2)]
ENDFOR

ENDIF

IF N_ELEMENTS(use_singles) GT 1 THEN BEGIN

use_singles=use_singles[1:*]
singles=intarr(2*N_ELEMENTS(use_singles))
singles[0:*:2]=use_singles-1
singles[1:*:2]=use_singles+1

num=findgen(float(N_ELEMENTS(singles))/2.0)*2

FOR j=0,(N_ELEMENTS(num)-1) DO BEGIN
spline_p,[singles(num(j)),singles(num(j)+1)],[tube(singles(num(j))),tube(singles(num(j)+1))],xpos,ypos,interval=sqrt(2.5)
tube(xpos[1:(N_ELEMENTS(xpos)-2)])=ypos[1:(N_ELEMENTS(xpos)-2)]
tube_er(xpos[1:(N_ELEMENTS(xpos)-2)])=ypos[1:(N_ELEMENTS(xpos)-2)]
ENDFOR
ENDIF


IF N_ELEMENTS(tube) GT t THEN tube=tube[0:(N_ELEMENTS(tube)-2)];remove padding check if this needs to be chnages when tube threads is more than 3

ENDIF

ENDELSE

int_tube_list=[[temporary(int_tube_list)],[tube]]
int_terror=[[temporary(int_terror)],[tube_er]]

FOR j=0, loopend DO BEGIN
int_amp = [temporary(int_amp),sin_pos(1,j)]
int_per = [temporary(int_per),sin_pos(2,j)]
ENDFOR

print,'#################################'
print,'VELOCITY'

ts_vel = rotate(vel_ts(*,*,i),4)

IF  N_ELEMENTS(ts_vel(*,0,0)) EQ 228 THEN BEGIN

ts_vel=ts_vel[1:*,*]
y1=tube[1:*]+width
y2=tube[1:*]-width
ENDIF ELSE BEGIN
y1=tube+width
y2=tube-width
ENDELSE

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

vel_tube_list=[[temporary(vel_tube_list)],[ts_vel_sum]]

tsv_er=0


FOR j=0,(szv(1)-1) DO BEGIN
a = ts_vel(j,*)
sd=stdev(a[where(a NE 0)])
if sd eq 0 then tsv_er=[temporary(tsv_er),0.001] else tsv_er=[temporary(tsv_er),sd]
ENDFOR

tsv_er=tsv_er[1:*]

vel_terror=[[temporary(vel_terror)],[tsv_er]]

look=''
while look ne 'y' do begin
plot,findgen(szv(1))*time,y1
READ,look,PROMPT='Done looking?'
endwhile

print,'################################'
print,'Sine fititng to velocity profile'

x2=findgen(szv(1))*time

fitdn=''
WHILE fitdn NE 'y' DO BEGIN

plot,x2,ts_vel_sum,xstyle=1,xtitle='Time(s)',ytitle='LOS Velocoty(km/s)'
velts=ts_vel_sum
ver=tsv_er
xx2=x2
errplot,xx2,velts-ver,velts+ver

c=''
READ,c,PROMPT='Change start of thread? y/n '
IF c EQ 'y' THEN BEGIN
READ,cutst,PROMPT='Start of thread? '
READ,cuten,PROMPT='End of thread? '
velts=ts_vel_sum[1.*cutst/time:1.*cuten/time]
ver=tsv_er[1.*cutst/time:1.*cuten/time]
xx2=x2[1.*cutst/time:1.*cuten/time]
plot,xx2,velts,xstyle=1
ENDIF

param=[velts[0],1.,20.,0.5,1.]
print,'Initial variables:',' constant', param[0],' amplitude',param[1],$
'period',param[2],' phase',param[3],' linear',param[4]

st=''
READ,st,PROMPT='Change start variables? y/n '

IF st EQ 'y' THEN BEGIN

READ,new2,PROMPT='Enter amplitude: '
READ,new3,PROMPT='Enter period: '
READ,new4,PROMPT='Enter phase: '
param=[param[0],new2,new3,new4,param[4]]

ENDIF

res=mpfitfun('mysin',xx2,velts,ver,param,perror=perror,bestnorm=bestnorm,/quiet)
IF N_ELEMENTS(res) GT 4 THEN BEGIN
yy=mysin(xx2,res)
loadct,2,/silent
oplot,xx2,yy,linestyle=1,color=100,thick=4
errplot,xx2,velts-ver,velts+ver
loadct,0,/silent

print,'%'
print,'%'
print,'Fitted variables',res
print,'Error on fits',perror
print,'Chi^2', bestnorm
ENDIF
READ,fitdn,PROMPT='Are you happy with the fit?'
 
ENDWHILE

IF N_ELEMENTS(res) GT 4 THEN BEGIN
print,res,perror
vel_fit_res=[[temporary(vel_fit_res)],[res]]
vel_fit_er=[[temporary(vel_fit_er)],[perror]]
ENDIF ELSE BEGIN
vel_fit_res=[[temporary(vel_fit_res)],[0,0,0,0,0]]
vel_fit_er=[[temporary(vel_fit_er)],[0,0,0,0,0]]
ENDELSE

;velcheck=''
;READ,velcheck,PROMPT='Investigate velocity structure?'
;IF velcheck EQ 'y' THEN BEGIN
;mg_loadct,22,/silent
;tvim,ts_vel,/rct,xtitle='Time(s)',ytitle='Displacment(Mm)',xrange=[0,226*time],yrange=[0,(szv(2)-1)*Mm]
;loadct,0,/silent
;READ,t,PROMPT='Where in time do you want to look?'
;window,1
;plot,ts_vel(t,*)
;done=''
;READ,done,PROMPT='Are you done? y/n '
;Eventually add something here to do stuff to it
;ENDIF


ENDFOR

int_info={tube_list:(int_tube_list[*,1:*]*Mm),int_tube_error: int_terror[*,1:*]*Mm}

vel_info={vel_tube_list:vel_tube_list[*,1:*],vel_tube_error:vel_terror[*,1:*],fit_param:vel_fit_res[*,1:*],fit_er:vel_fit_er[*,1:*]}

save,int_info,filename='wm_int_0191030_758_3.idl'
save,vel_info,filename='wm_vel_0191030_758_3.idl'


END