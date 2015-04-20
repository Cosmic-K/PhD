pro wave_measure,int_ts,vel_ts,steps

;may nee to redo some of the ts
;check start end points and length
;think about amplitudes, and periods


sz=size(int_ts)
m=
c=

m1=
c1=

int_times=0
vel_times=0

width=

FOR i=0,(sz(3)-1) DO BEGIN

ts_in = int_ts(*,*,i)
ts_er = (m*ts_in)+c

track_saus_test,data=(-1.*ts_in),errors=ts_er,threads_fit_fg


pos=threads_fit_fg.fitted_pos
pos=pos[*,1]
;take raw input for start and end position based from fits
READ,st,PROMPT='Start point? '
READ,en,PROMT='End point? '
diff=en-st
x=findgen(diff)+st+1
y=mysin(x,pos(0:4))
max(y,j)
int_times=[temporary(int_times),x(j)]

ts_vel = rotate(vel_ts(st:en,i),4)


y1=y+width
y2=y-width

szv=size(ts_vel)

FOR k=0,(szv(1)-1) DO BEGIN
FOR l=0,(szv(2)-1) DO BEGIN
IF l LT y2(k) THEN ts_vel(k,l)=0 ELSE ts_vel(k,l)=ts_vel(k,l)

FOR k=0,(szv(1)-1) DO BEGIN
FOR l=0,(szv(2)-1) DO BEGIN
IF l GT y1(k) THEN ts_vel(k,l)=0 ELSE IF ts_vel(k,l) EQ 0 THEN ts_vel(k,l)=ts_vel(k,l)

;make a plot hee to check this

ts_vel_sum=sum(ts_vel,1);check this is the right direction
;plot,ts_vel_sum

tsv_er = (m1*ts_vel_sum)+c1

param=[0,0,0,0,0]
res=mpfitfun('mysin',x,ts_vel_sum,tsv_er,param)
yy=mysim(x,param)
max(yy,o)
vel_times=[temporary(vel_times),x(o)]

ENDFOR

intime_diffs=int_times(1:*)-shift(int_times(1:*),0,1)
veltime_vel=vel_times(1:*)-shift(vel_times(1:*),0,1)

print,'Average phase velocity in intensity',mean(float(steps)/float(intime_diffs))
print,'Average phase velocity in velocity',mean(float(steps)/float(veltime_diffs))