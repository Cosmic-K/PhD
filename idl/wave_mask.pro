pro wave_measure,int_ts,vel_ts,steps

;may nee to redo some of the ts
;check start end points and length
;think about amplitudes, and periods


sz=size(int_ts)
a=1.7084e-7
b=-0.001
c=2.52


int_times=0
vel_times=0

width=

FOR i=0,(sz(3)-1) DO BEGIN

ts_in = int_ts(*,*,i)
ts_er = (a*ts_in^2)+(b*ts_in)+c

track_saus_test,data=(-1.*ts_in),errors=ts_er,fit,threads
;get start and end points of threads during this part
;dont forget the period and amplitude

pos=threads.pos
pos=pos[*,1:*]
pos_size=size(pos)

tube=0
st=0
en=0

FOR j=0,(pos_size(2)-1) DO BEGIN

READ,st,PROMPT='Start point? '
READ,en,PROMT='End point? '
tube=[temporary(tube),pos(st:en,j)]
ENDFOR

index_1=where(tube eq -1)
index_0=where(tube eq 0)
index = [index_1,index_0]

tube(index)=0.7*max(tube)

IF index(0) EQ 0 THEN index=index[1:*]


use_pairs=0
use_singles=0

FOR j=0, (n_elements(index)-1) DO BEGIN
IF (j EQ 0) AND (index(j) EQ index(j+1)-1) THEN use_pairs=[tempoary(use_pairs),index(j)] ELSE
IF (j EQ 0) AND (index(j) NE index(j+1)-1) THEN use_singles=[tempoary(use_singles),index(j)] ELSE
IF (index(j) EQ index(j+1)-1) AND (index(j) NE index(j-1)+1) THEN use_pairs=[tempoary(use_pairs),index(j)] ELSE
IF (index(j) EQ index(j-1)+1) AND (index(j) NE index(j+1)-1) THEN use_pairs=[tempoary(use_pairs),index(j)] ELSE
IF (index(j) NE index(j-1)+1) AND (index(j) NE index(j+1)-1) THEN use_singles=[tempoary(use_singles),index(j)]
ENDFOR

pairs=intarr(n_elements(use_pairs))
singles=intarr(2*n_elements(singles))

pairs[0:*:2]=use_pairs[0:*:2]-1
pairs[1:*:2]=use_pairs[1:*:2]+1

singles[0:*:2]=use_singles-1
singles[1:*:2]=use_singles+1


num=findgen(float(n_elements(pairs))/2.0)*2
FOR j=0,n_elements(num) DO BEGIN
spline_p,pairs[num(j):(num(j)+1)],tube(pairs[num(j):(num(j)+1)]),xpos,ypos,interval=sqrt((pairs(num(j)+1)-pairs(num(j)))^2+(tube(pairs(num(j)+1))-tube(pairs(num(j))))^2)
tube(xpos)=ypos
ENDFOR

FOR j=0,(n_elements(singles)-2) DO BEGIN
spline_p,singles[j:(j+1)],tube(singles[j:(j+1)]),xpos,ypos,interval=sqrt(singles(j)+1)
tube(xpos)=ypos
ENDFOR



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