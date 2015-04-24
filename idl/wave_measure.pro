pro wave_measure,int_ts;,vel_ts,steps


;think about amplitudes, and periods


sz=size(int_ts)
a=1.7084e-7
b=-0.001
c=2.52



Mm = (0.725/16.981891892)
sz = size(im_in)
xsz = Mm*(float(sz(1))-1.0)
ysz = Mm*(float(sz(2))-1.0)


time = 1.343


int_times=0
vel_times=0

width=2

;FOR i=0,0 DO BEGIN;(sz(3)-1) DO BEGIN

ts_in = int_ts(*,*,0)
ts_er = (a*ts_in^2)+(b*ts_in)+c

track_saus_test,data=(-1.*ts_in),errors=ts_er,fit,threads
;get start and end points of threads during this part
;dont forget the period and amplitude

print,'#####################################'
print,'END OF FITTING BEGINING TUBE MASKING'


pos=threads.pos
pos=pos[*,1:*]
pos_size=size(pos)

tube=0
st=0
en=0

;DONT exceed bounds
FOR j=0,(pos_size(2)-1) DO BEGIN

;WHILE (st lt 0) OR (en ge n_elements(pos_size(1))) DO BEGIN
READ,st,PROMPT='Start point? '
READ,en,PROMPT='End point? '
;IF (st lt 0) OR (en ge n_elements(pos_size(1))) THEN print, 'Do not exceed bounds, re-enter'
;ENDWHILE

tube=[temporary(tube),pos(st:en,j)]
ENDFOR

tube=tube[1:(n_elements(tube)-2)]

index1=where(tube eq -1)
index0=where(tube eq 0)
index=[index1,index0]

tube(index)=0.7*max(tube)

index=index[sort(index)]

IF index(0) EQ 0 THEN index=index[1:*]
IF index(n_elements(index)-1) EQ n_elements(tube) THEN index=index[0:(n_elements(index)-1)]
print, index(n_elements(index)-1)

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

;print,'index',index
;print,'use_pairs',use_pairs
;print,'use_singles',use_singles
;print,'paris',pairs
;print,'singles',singles
print,'num',num

FOR j=0,n_elements(num)-1 DO BEGIN
print,'j',j
spline_p,[pairs(num(j)),pairs(num(j)+1)],[tube(pairs(num(j))),tube(pairs(num(j)+1))],xpos,ypos,interval=1.3
print,xpos,ypos
tube(xpos[1:(n_elements(xpos)-2)])=ypos[1:(n_elements(xpos)-2)]
ENDFOR

FOR j=0,(n_elements(singles)-2) DO BEGIN
spline_p,[singles(j),singles(j+1)],[tube(singles(j)),tube(singles(j+1))],xpos,ypos,interval=sqrt(2)
tube(xpos[1:(n_elements(xpos)-2)])=ypos[1:(n_elements(xpos)-2)]
ENDFOR

plot,tube
help,tube

;ENDFOR
END


;CHECK FOR -1 AT START remove padding first then do where










;diff=en-st
;x=findgen(diff)+st+1
;y=mysin(x,pos(0:4))
;max(y,j)
;int_times=[temporary(int_times),x(j)]

;ts_vel = rotate(vel_ts(st:en,i),4)


;y1=y+width
;y2=y-width

;szv=size(ts_vel)

;FOR k=0,(szv(1)-1) DO BEGIN
;FOR l=0,(szv(2)-1) DO BEGIN
;IF l LT y2(k) THEN ts_vel(k,l)=0 ELSE ts_vel(k,l)=ts_vel(k,l)

;FOR k=0,(szv(1)-1) DO BEGIN
;FOR l=0,(szv(2)-1) DO BEGIN
;IF l GT y1(k) THEN ts_vel(k,l)=0 ELSE IF ts_vel(k,l) EQ 0 THEN ts_vel(k,l)=ts_vel(k,l)

;make a plot hee to check this

;ts_vel_sum=sum(ts_vel,1);check this is the right direction
;plot,ts_vel_sum

;tsv_er = (m1*ts_vel_sum)+c1

;param=[0,0,0,0,0]
;res=mpfitfun('mysin',x,ts_vel_sum,tsv_er,param)
;yy=mysim(x,param)
;max(yy,o)
;vel_times=[temporary(vel_times),x(o)]

;ENDFOR

;intime_diffs=int_times(1:*)-shift(int_times(1:*),0,1)
;veltime_vel=vel_times(1:*)-shift(vel_times(1:*),0,1)

;print,'Average phase velocity in intensity',mean(float(steps)/float(intime_diffs))
;print,'Average phase velocity in velocity',mean(float(steps)/float(veltime_diffs))