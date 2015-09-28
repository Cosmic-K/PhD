pro velpeak,tubevel,tervel

time=1.343

szv=size(tubevel)

vel_time = 0
vel_time_pos = 0
vel_time_xer = 0
vel_time_yer = 0

FOR i=0,0 do begin;(szv(2)-1) DO BEGIN

ts_vel_sum=divide(tubevel[*,i],1000)
tsv_er=divide(tervel[*,i],1000)

fit_check=''

WHILE fit_check NE 'y' DO BEGIN

x=findgen(szv(1))

check=''

WHILE check NE 'y' DO BEGIN
set_plot,'ps'
device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/velpek.eps'
plot,x*time,ts_vel_sum,xtitle='Time(s)',ytitle='Velocity(km/s)',xstyle=1,xthick=2,ythick=2,thick=2
errplot,x*time,ts_vel_sum-tsv_er,ts_vel_sum+tsv_er,thick=2
;oplot,x*time,yy,linestyle=1
print,'Where would you like to cut?'
READ,cutst,PROMPT='Start of cut? '
READ,cuten,PROMPT='End of cut? '

cutst=divide(cutst,time)
cuten=divide(cuten,time)

res=poly_fit(x[cutst:cuten]*time,ts_vel_sum[cutst:cuten],2,measure_errors=(tsv_er[cutst:cuten]),chisq=chi,yfit=fit,sigma=sigma)
;print, 'Fit results:',res
;print,'Errors:',sigma
;print,'ChiSq:',chi

loadct,2,/silent
oplot,x[cutst:cuten]*time,fit,linestyle=2,thick=3,color=100

x_cen=float(-1*res(1))/float(2*res(2))
vdelx=x_cen*sqrt((float(sigma(1))/float(res(1)))^2+(float(sigma(2))/float(res(2)))^2)

y_cen=res(2)*x_cen^2+res(1)*x_cen+res(0)
vdely=sqrt((((2*res(2)*x_cen)+res(1))^2)*vdelx^2)


plots,x_cen,y_cen,psym=7,color=100,SYMSIZE=2,thick=2
loadct,0,/silent

READ,check,PROMPT='Happy with fit? '
device,/close

set_plot,'x'




ENDWHILE

vel_time =[temporary(vel_time),x_cen]
vel_time_pos = [temporary(vel_time_pos),y_cen]
vel_time_xer = [temporary(vel_time_xer),vdelx]
vel_time_yer= [temporary(vel_time_yer),vdely]

READ,fit_check,PROMPT='Are you done fitting? '

ENDWHILE
ENDFOR

vel_time = vel_time[1:*]
vel_time_pos = vel_time_pos[1:*]
vel_time_xer = vel_time_xer[1:*]
vel_time_yer= vel_time_yer[1:*]


vel_1_time=vel_time[0:*:2]
vel_2_time=vel_time[1:*:2]


vel_1_pos=vel_time_pos[0:*:2]+[0,1000,2000,3000,4000,5000,6000,7000,8000,9000]
vel_2_pos=vel_time_pos[1:*:2]+[0,1000,2000,3000,4000,5000,6000,7000,8000,9000]


xer_1=vel_time_xer[0:*:2]
xer_2=vel_time_xer[1:*:2]

yer_1=vel_time_yer[0:*:2]
yer_2=vel_time_yer[1:*:2]

vel_timpos={vel_time1:vel_1_time, vel_time2:vel_2_time,vel_pos1:vel_1_pos,vel_pos2:vel_2_pos,vxer_1:xer_1,vxer_2:xer_2,vyer_1:yer_1,vyer_2:yer_2}
help,vel_timpos,/str
save,vel_timpos,filename='vtp.idl'

END