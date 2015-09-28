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

pro wave_measure2,int_ts,vel_ts,int_info,vel_info,qlook=qlook
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

t=226

int_tube_list=fltarr(t)
int_terror=fltarr(t)
vel_tube_list=fltarr(226)
vel_terror=fltarr(226)

t_xer=0
t_yer=0

vel_time_xer = 0
vel_time_yer = 0


FOR i=0,(sz(3)-1) DO BEGIN

print, 'YOU ARE MEASURING TIME SERIES NUMBER: ',i,' of ',(sz(3)-1)

a=8.2064e-7
b=-0.00055
c=0.087

ts_in = int_ts(*,*,i)
ts_er = exp((a*ts_in^2)+(b*ts_in)+c)

track_saus,data=smooth((-1.*ts_in),1),threads_fit_fg,threads,errors=ts_er,/patch


print,'#####################################'
print,'END OF FITTING BEGINING TUBE MASKING'


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
;do i need this^

use_pairs=0
use_singles=0

last=(N_ELEMENTS(index)-1)

IF N_ELEMENTS(index) GT 1 THEN BEGIN

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
ENDIF ELSE BEGIN
use_singles=[temporary(use_singles),index]
ENDELSE


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
tube_er(xpos[1:(N_ELEMENTS(xpos)-2)])=1
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
tube_er(xpos[1:(N_ELEMENTS(xpos)-2)])=sqrt(tube(singles(num(j)))^2 + tube(singles(num(j)+1))^2)/2
ENDFOR
ENDIF


IF N_ELEMENTS(tube) GT t THEN tube=tube[0:(N_ELEMENTS(tube)-2)];remove padding check if this needs to be chnages when tube threads is more than 3
ENDIF
ENDELSE

int_tube_list=[[temporary(int_tube_list)],[tube]]
int_terror=[[temporary(int_terror)],[tube_er]]


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
print,szv
print,size(y1)
print,size(y2)

mg_loadct,22,/silent
cgimage,ts_vel,/axes,xrange=[0,225]*time,yrange=[0,szv(2)-1]*Mm,/window,/KEEP_ASPECT_RATIO,clip=15,xtitle='Tine (s)',ytitle='Displacement (Mm)',charsize=0.9
cgplot,findgen(225)*time,y1*Mm,/window,/overplot;,color='black',xstyle=1,xrange=[0,225]*time
cgplot,findgen(225)*time,y2*Mm,/window,/overplot;,color='black',xstyle=1,xrange=[0,225]*time
loadct,0,/silent
pause


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

IF keyword_set(qlook) THEN BEGIN
look=''
WHILE look ne 'y' do begin
plot,findgen(szv(1))*time,y1
READ,look,PROMPT='Done looking?'
ENDWHILE
ENDIF


done=''
WHILE DONE NE 'y' DO BEGIN
mg_loadct,22,/silent
cgimage,ts_vel,xtitle='Time (s)',ytitle='Displacment (Mm)',xrange=[0,225*time],yrange=[0,(szv(2)-1)*Mm],/window,/axis,/KEEP_ASPECT_RATIO,charsize=0.9
loadct,0,/silent
READ,t,PROMPT='Where in time do you want to look?'
t=float(t)/time
cgplot,ts_vel(t,*),/window,xstyle=4,ytitle='Velocity (km/s)'
cgAxis,Xaxis=1,xrange=[0,25]*Mm,xstyle=1,/window,charsize=0.000000001
cgAxis,Xaxis=0,title='Displacement (Mm)',xrange=[0,25]*Mm,xstyle=1,/window


done=''
READ,done,PROMPT='Are you done? y/n '
ENDWHILE

ENDFOR

int_info={tube_list:(int_tube_list[*,1:*]*Mm),int_tube_error: int_terror[*,1:*]*Mm}

vel_info={vel_tube_list:vel_tube_list[*,1:*],vel_tube_error:vel_terror[*,1:*],fit_param:vel_fit_res[*,1:*],fit_er:vel_fit_er[*,1:*]}

save,int_info,filename='wm_int_6258077915_24111_4.idl'
save,vel_info,filename='wm_vel_6258077915_24111_4.idl'


END