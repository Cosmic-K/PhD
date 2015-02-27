;Krishna Mooroogen.
;Northumbria University, Solar group.
;25/02/15.
;Routine to perform simulataneuous time series plots across single feature.

;NOTE
;Make unsharped cropped image before input. Crop in time as well befor einout if necessary.
;Check input coordinates by taking summed image over time.
;Constant may need chnaging.
;Maybe first cut will be made manually.
;plot some kind of phase diagram with units

;CALL:
;cross_cut_multi,image,xx1=xx1,xx2=xx2,yy1=yy1,yy2=yy2

;PURPOSE:
;Make time series plots perpindicular across feature to create phase diagram

;INPUTS
;input image must be cropped unsharped 3d image
;starting coordinates

;OUTPUS
;Images of FOV, time series and summed fov
;Time series data 0-9 across feature line

pro cross_cut_multi,image

loadct,0

Mm = (0.725/16.981891892)
const = 2
sz = size(image)
xsz = Mm*(float(sz(1))-1.0)
ysz = Mm*(float(sz(2))-1.0)

sum_im = sum(image,2)

image=unsharp(data=image,dx=9)


set = ' '

READ,set,PROMPT='Which axis to hold constant? Input x or y. '


IF set EQ 'y' THEN BEGIN

diag_slit2,image,ts,coords=coord,x1=xx1,x2=xx2,y1=yy1,y2=yy2,/noopen
diag_slit2,image,ts1,coords=coord1,x1=(coord[0]+const),x2=(coord[1]+const),y1=coord[2],y2=coord[3],/noopen
diag_slit2,image,ts2,coords=coord2,x1=(coord1[0]+const),x2=(coord1[1]+const),y1=coord1[2],y2=coord1[3],/noopen
diag_slit2,image,ts3,coords=coord3,x1=(coord2[0]+const),x2=(coord2[1]+const),y1=coord2[2],y2=coord2[3],/noopen
diag_slit2,image,ts4,coords=coord4,x1=(coord3[0]+const),x2=(coord3[1]+const),y1=coord3[2],y2=coord3[3],/noopen
diag_slit2,image,ts5,coords=coord5,x1=(coord4[0]+const),x2=(coord4[1]+const),y1=coord4[2],y2=coord4[3],/noopen
diag_slit2,image,ts6,coords=coord6,x1=(coord5[0]+const),x2=(coord5[1]+const),y1=coord5[2],y2=coord5[3],/noopen
diag_slit2,image,ts7,coords=coord7,x1=(coord6[0]+const),x2=(coord6[1]+const),y1=coord6[2],y2=coord6[3],/noopen
diag_slit2,image,ts8,coords=coord8,x1=(coord7[0]+const),x2=(coord7[1]+const),y1=coord7[2],y2=coord7[3],/noopen
diag_slit2,image,ts9,coords=coord9,x1=(coord8[0]+const),x2=(coord8[1]+const),y1=coord8[2],y2=coord8[3],/noopen


ENDIF

IF set EQ 'x' THEN BEGIN

diag_slit2,image,ts,coords=coord,x1=xx1,x2=xx2,y1=yy1,y2=yy2,/noopen
diag_slit2,image,ts1,coords=coord1,x1=(coord[0]+const),x2=(coord[1]+const),y1=(coord[2]+const),y2=(coord[3]+const),/noopen
diag_slit2,image,ts2,coords=coord2,x1=(coord1[0]+const),x2=(coord1[1]+const),y1=(coord1[2]+const),y2=(coord1[3]+const),/noopen
diag_slit2,image,ts3,coords=coord3,x1=(coord2[0]+const),x2=(coord2[1]+const),y1=(coord2[2]+const),y2=(coord2[3]+const),/noopen
diag_slit2,image,ts4,coords=coord4,x1=(coord3[0]+const),x2=(coord3[1]+const),y1=(coord3[2]+const),y2=(coord3[3]+const),/noopen
diag_slit2,image,ts5,coords=coord5,x1=(coord4[0]+const),x2=(coord4[1]+const),y1=(coord4[2]+const),y2=(coord4[3]+const),/noopen
diag_slit2,image,ts6,coords=coord6,x1=(coord5[0]+const),x2=(coord5[1]+const),y1=(coord5[2]+const),y2=(coord5[3]+const),/noopen
diag_slit2,image,ts7,coords=coord7,x1=(coord6[0]+const),x2=(coord6[1]+const),y1=(coord6[2]+const),y2=(coord6[3]+const),/noopen
diag_slit2,image,ts8,coords=coord8,x1=(coord7[0]+const),x2=(coord7[1]+const),y1=(coord7[2]+const),y2=(coord7[3]+const),/noopen
diag_slit2,image,ts9,coords=coord9,x1=(coord8[0]+const),x2=(coord8[1]+const),y1=(coord8[2]+const),y2=(coord8[3]+const),/noopen


ENDIF



IF set EQ 'none' THEN BEGIN

diag_slit2,image,ts,coords=coord,x1=xx1,x2=xx2,y1=yy1,y2=yy2,/noopen
diag_slit2,image,ts1,coords=coord1,x1=(coord[0]+const),x2=(coord[1]+const),y1=(coord[2]+const),y2=(coord[3]+const),/noopen
diag_slit2,image,ts2,coords=coord2,x1=(coord1[0]+const),x2=(coord1[1]+const),y1=(coord1[2]+const),y2=(coord1[3]+const),/noopen
diag_slit2,image,ts3,coords=coord3,x1=(coord2[0]+const),x2=(coord2[1]+const),y1=(coord2[2]+const),y2=(coord2[3]+const),/noopen
diag_slit2,image,ts4,coords=coord4,x1=(coord3[0]+const),x2=(coord3[1]+const),y1=(coord3[2]+const),y2=(coord3[3]+const),/noopen
diag_slit2,image,ts5,coords=coord5,x1=(coord4[0]+const),x2=(coord4[1]+const),y1=(coord4[2]+const),y2=(coord4[3]+const),/noopen
diag_slit2,image,ts6,coords=coord6,x1=(coord5[0]+const),x2=(coord5[1]+const),y1=(coord5[2]+const),y2=(coord5[3]+const),/noopen
diag_slit2,image,ts7,coords=coord7,x1=(coord6[0]+const),x2=(coord6[1]+const),y1=(coord6[2]+const),y2=(coord6[3]+const),/noopen
diag_slit2,image,ts8,coords=coord8,x1=(coord7[0]+const),x2=(coord7[1]+const),y1=(coord7[2]+const),y2=(coord7[3]+const),/noopen
diag_slit2,image,ts9,coords=coord9,x1=(coord8[0]+const),x2=(coord8[1]+const),y1=(coord8[2]+const),y2=(coord8[3]+const),/noopen


ENDIF

ts=rotate(ts,3)
ts1=rotate(ts1,3)
ts2=rotate(ts2,3)
ts3=rotate(ts3,3)
ts4=rotate(ts4,3)
ts5=rotate(ts5,3)
ts6=rotate(ts6,3)
ts7=rotate(ts7,3)
ts8=rotate(ts8,3)
ts9=rotate(ts9,3)

time = 1.343
szts = size(ts)
tsxsz = time*(float(szts(1))-1.0)
tsysz = Mm*(float(szts(2))-1.0)


;plot fov

set_plot,'ps'

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/phase_vel/0206070_9095/fov_sum_0206070_9095.eps'
!Y.OMargin = [2, 8]
!X.OMargin = [2, 6]
!P.Charsize=0.60

tvim,sum_im(*,*),title='Core intensity summed over time 0206070_9095',xrange=[0,xsz],yrange=[0,ysz],xtitle='Mm',ytitle='Mm'
plots,[(Mm*coord[0]),(Mm*coord[1])],[(Mm*coord[2]),(Mm*coord[3])],color=450
plots,[(Mm*coord1[0]),(Mm*coord1[1])],[(Mm*coord1[2]),(Mm*coord1[3])],color=450
plots,[(Mm*coord2[0]),(Mm*coord2[1])],[(Mm*coord2[2]),(Mm*coord2[3])],color=450
plots,[(Mm*coord3[0]),(Mm*coord3[1])],[(Mm*coord3[2]),(Mm*coord3[3])],color=450
plots,[(Mm*coord4[0]),(Mm*coord4[1])],[(Mm*coord4[2]),(Mm*coord4[3])],color=450
plots,[(Mm*coord5[0]),(Mm*coord5[1])],[(Mm*coord5[2]),(Mm*coord5[3])],color=450
plots,[(Mm*coord6[0]),(Mm*coord6[1])],[(Mm*coord6[2]),(Mm*coord6[3])],color=450
plots,[(Mm*coord7[0]),(Mm*coord7[1])],[(Mm*coord7[2]),(Mm*coord7[3])],color=450
plots,[(Mm*coord8[0]),(Mm*coord8[1])],[(Mm*coord8[2]),(Mm*coord8[3])],color=450
plots,[(Mm*coord9[0]),(Mm*coord9[1])],[(Mm*coord9[2]),(Mm*coord9[3])],color=450

device,/close

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/phase_vel/0206070_9095/fov_0206070_9095.eps'
!Y.OMargin = [2, 8]
!X.OMargin = [2, 6]
!P.Charsize=0.60

tvim,image(*,*,0),title='Core intensity 0206070_9095',xrange=[0,xsz],yrange=[0,ysz],xtitle='Mm',ytitle='Mm'
plots,[(Mm*coord[0]),(Mm*coord[1])],[(Mm*coord[2]),(Mm*coord[3])],color=450
plots,[(Mm*coord1[0]),(Mm*coord1[1])],[(Mm*coord1[2]),(Mm*coord1[3])],color=450
plots,[(Mm*coord2[0]),(Mm*coord2[1])],[(Mm*coord2[2]),(Mm*coord2[3])],color=450
plots,[(Mm*coord3[0]),(Mm*coord3[1])],[(Mm*coord3[2]),(Mm*coord3[3])],color=450
plots,[(Mm*coord4[0]),(Mm*coord4[1])],[(Mm*coord4[2]),(Mm*coord4[3])],color=450
plots,[(Mm*coord5[0]),(Mm*coord5[1])],[(Mm*coord5[2]),(Mm*coord5[3])],color=450
plots,[(Mm*coord6[0]),(Mm*coord6[1])],[(Mm*coord6[2]),(Mm*coord6[3])],color=450
plots,[(Mm*coord7[0]),(Mm*coord7[1])],[(Mm*coord7[2]),(Mm*coord7[3])],color=450
plots,[(Mm*coord8[0]),(Mm*coord8[1])],[(Mm*coord8[2]),(Mm*coord8[3])],color=450
plots,[(Mm*coord9[0]),(Mm*coord9[1])],[(Mm*coord9[2]),(Mm*coord9[3])],color=450

device,/close

;plot timeeries

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/phase_vel/0206070_9095/time_series_0206070_9095.eps'
!Y.OMargin = [2, 8]
!X.OMargin = [2, 6]
!P.Charsize=0.60

!P.Multi = [0,2,5]

tvim,ts,aspect=15,title='0206070_9095',xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

tvim,ts1,aspect=15,title='0206070_9295',xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

tvim,ts2,aspect=15,title='0206070_9495',xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

tvim,ts3,aspect=15,title='0206070_9695',xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

tvim,ts4,aspect=15,title='0206070_9895',xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

tvim,ts5,aspect=15,title='0206070_10095',xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

tvim,ts6,aspect=15,title='0206070_10295',xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

tvim,ts7,aspect=15,title='0206070_10495',xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

tvim,ts8,aspect=15,title='0206070_10695',xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

tvim,ts9,aspect=15,title='0206070_10895',xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

!P.Multi = 0
!P.Charsize = 0
!Y.OMargin = [0, 0]
!X.OMargin = [0, 0]

device,/close

loadct,0

set_plot, 'x'

;save the time series

save,ts,description='Time series from core 0206070_ 90,95,90,65',filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_0206070_9095/ts_core_0206070_9095.idl'

save,ts1,description='Time series from core 0206070_ 92,95,92,65',filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_0206070_9095/ts_core_0206070_9295.idl'

save,ts2,description='Time series from core 0206070_ 94,95,94,65',filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_0206070_9095/ts_core_0206070_9495.idl'

save,ts3,description='Time series from core 0206070_ 96,95,96,65',filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_0206070_9095/ts_core_0206070_9695.idl'

save,ts4,description='Time series from core 0206070_ 98,95,98,65',filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_0206070_9095/ts_core_0206070_9895.idl'

save,ts5,description='Time series from core 0206070_ 100,95,100,65',filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_0206070_9095/ts_core_0206070_10095.idl'

save,ts6,description='Time series from core 0206070_ 102,95,102,65',filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_0206070_9095/ts_core_0206070_10295.idl'

save,ts7,description='Time series from core 0206070_ 104,95,104,65',filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_0206070_9095/ts_core_0206070_10495.idl'

save,ts8,description='Time series from core 0206070_ 106,95,106,65',filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_0206070_9095/ts_core_0206070_10695.idl'

save,ts9,description='Time series from core 0206070_ 108,95,108,65',filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_0206070_9095/ts_core_0206070_10895.idl'
end