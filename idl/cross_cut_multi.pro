;Krishna Mooroogen.
;Northumbria University, Solar group.
;25/02/15.
;Routine to perform simulataneuous time series plots across single feature.

;NOTEs
;Make cropped image before input. Crop in time as well before input if necessary.
;ALWAYS START CUT AT THE BOTTOM OF THE FEATURE AS THE PROGRAM WORKS UP,maybe add some cod later to work both directions by comparing coord size
;Constant may need chnaging.
;need to plot some kind of phase diagram with units
;omitted for loops at this stage for speed may experient later
;added in manually cut selection on the summed image, negated the need for input coords
;added string variable to files names
;not the most efficent as there is repeated code in diag_slit, maybe combine at some point to make diag slit3.


;CALL:
;cross_cut_multi,image=image,xx1=xx1,xx2=xx2,yy1=yy1,yy2=yy2

;PURPOSE:
;Make time series plots perpindicular across feature to create phase diagram

;INPUTS
;input image must be cropped unsharped 3d image
;starting coordinates

;OUTPUS
;Images of FOV, time series and summed fov
;Time series data 0-9 across feature line

pro cross_cut_multi,image=image

set_plot,'x'
loadct,0


!P.Multi = 0
!P.Charsize = 0
!Y.OMargin = [0, 0]
!X.OMargin = [0, 0]

Mm = (0.725/16.981891892)
const = 1
sz = size(image)
xsz = Mm*(float(sz(1))-1.0)
ysz = Mm*(float(sz(2))-1.0)

image=unsharp(data=image,dx=9)
sum_im = sum(image,2)

IF n_elements(xx1) EQ 0. THEN BEGIN
;Set plotting elements
;writing to buffer pixmap reduces on-screen jitter!

determine_window_size,xsize=xsize,ysize=ysize,openwin=openwin,/new_win
window,openwin[0],xs=xsize,ys=ysize
window_var=openwin[0]

window,xs=xsize,ys=ysize,/pixmap,/free
pixID=!d.window
tvim,sum_im
wset,window_var
device,copy=[0,0,!d.x_size,!d.y_size,0,0,pixid]


PRINT,'Select point with cursor'
CURSOR,xx1,yy1,/Data,/Down
print,  'xx1='+strtrim(string(fix(xx1)),1), ' yy1='+strtrim(string(fix(yy1)),1)

PRINT,'Select point with cursor'
IF !MOUSE.button EQ 1 THEN BEGIN
CURSOR, do_nothing_x, do_nothing_y, /UP, /WAIT
!MOUSE.button=0
ENDIF
WHILE (!MOUSE.button NE 1) DO BEGIN
CURSOR,xx2,yy2,/change
window,xs=xsize,ys=ysize,/pixmap,/free
pixID=!d.window
tvim,sum_im
wset,window_var
device,copy=[0,0,!d.x_size,!d.y_size,0,0,pixid]
PLOTS,[xx1,xx2],[yy1,yy2]
ENDWHILE
print,  'xx2='+strtrim(string(fix(xx2)),1), ' yy2='+strtrim(string(fix(yy2)),1)

xx1=fix(xx1)
yy1=fix(yy1)
xx2=fix(xx2)
yy2=fix(yy2)

ENDIF

result=file_test('/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/phase_vel/13283950_'+strtrim(xx1,1)+strtrim(yy1,1),/directory)
result1=file_test('/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_13283950_'+strtrim(xx1,1)+strtrim(yy1,1),/directory)

IF result EQ 0 THEN BEGIN
FILE_MKDIR,'/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/phase_vel/13283950_'+strtrim(xx1,1)+strtrim(yy1,1)
ENDIF

IF result EQ 0 THEN BEGIN
FILE_MKDIR,'/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_13283950_'+strtrim(xx1,1)+strtrim(yy1,1)
ENDIF


set = ' '

READ,set,PROMPT='Which axis to hold constant? Input x or y. '


IF set EQ 'y' THEN BEGIN
print,xx1,yy1
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
print,xx1,yy1
print,coord
print,coord1
print,coord2
print,coord3
print,coord4
print,coord5
print,coord6
print,coord7
print,coord8
print,coord9


ENDIF

IF set EQ 'x' THEN BEGIN
print,xx1,yy1
diag_slit2,image,ts,coords=coord,x1=xx1,x2=xx2,y1=yy1,y2=yy2,/noopen
diag_slit2,image,ts1,coords=coord1,x1=(coord[0]),x2=(coord[1]),y1=(coord[2]+const),y2=(coord[3]+const),/noopen
diag_slit2,image,ts2,coords=coord2,x1=(coord1[0]),x2=(coord1[1]),y1=(coord1[2]+const),y2=(coord1[3]+const),/noopen
diag_slit2,image,ts3,coords=coord3,x1=(coord2[0]),x2=(coord2[1]),y1=(coord2[2]+const),y2=(coord2[3]+const),/noopen
diag_slit2,image,ts4,coords=coord4,x1=(coord3[0]),x2=(coord3[1]),y1=(coord3[2]+const),y2=(coord3[3]+const),/noopen
diag_slit2,image,ts5,coords=coord5,x1=(coord4[0]),x2=(coord4[1]),y1=(coord4[2]+const),y2=(coord4[3]+const),/noopen
diag_slit2,image,ts6,coords=coord6,x1=(coord5[0]),x2=(coord5[1]),y1=(coord5[2]+const),y2=(coord5[3]+const),/noopen
diag_slit2,image,ts7,coords=coord7,x1=(coord6[0]),x2=(coord6[1]),y1=(coord6[2]+const),y2=(coord6[3]+const),/noopen
diag_slit2,image,ts8,coords=coord8,x1=(coord7[0]),x2=(coord7[1]),y1=(coord7[2]+const),y2=(coord7[3]+const),/noopen
diag_slit2,image,ts9,coords=coord9,x1=(coord8[0]),x2=(coord8[1]),y1=(coord8[2]+const),y2=(coord8[3]+const),/noopen
print,xx1,yy1
print,coord
print,coord1
print,coord2
print,coord3
print,coord4
print,coord5
print,coord6
print,coord7
print,coord8
print,coord9
ENDIF



IF set EQ 'none' THEN BEGIN
print,xx1,yy1
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
print,xx1,yy1
print,coord
print,coord1
print,coord2
print,coord3
print,coord4
print,coord5
print,coord6
print,coord7
print,coord8
print,coord9
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

coord=fix(coord)
coord1=fix(coord1)
coord2=fix(coord2)
coord3=fix(coord3)
coord4=fix(coord4)
coord5=fix(coord5)
coord6=fix(coord6)
coord7=fix(coord7)
coord8=fix(coord8)
coord9=fix(coord9)

time = 1.343
szts = size(ts)
tsxsz = time*(float(szts(1))-1.0)
tsysz = Mm*(float(szts(2))-1.0)

xx1=fix(xx1)
yy1=fix(yy1)

;plot fov

set_plot,'ps'

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/phase_vel/13283950_'+strtrim(xx1,1)+strtrim(yy1,1)+'/fov_sum_13283950_'+strtrim(xx1,1)+strtrim(yy1,1)+'.eps'
!Y.OMargin = [2, 8]
!X.OMargin = [2, 6]
!P.Charsize=0.60

tvim,sum_im(*,*),title='Core intensity summed over time 13283950_'+strtrim(coord(0),1)+strtrim(coord(1),1),xrange=[0,xsz],yrange=[0,ysz],xtitle='Mm',ytitle='Mm'
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

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/phase_vel/13283950_'+strtrim(xx1,1)+strtrim(yy1,1)+'/fov_13283950_'+strtrim(xx1,1)+strtrim(yy1,1)+'.eps'
!Y.OMargin = [2, 8]
!X.OMargin = [2, 6]
!P.Charsize=0.60

tvim,image(*,*,0),title='Core intensity 13283950_'+strtrim(coord(0),1)+strtrim(coord(1),1),xrange=[0,xsz],yrange=[0,ysz],xtitle='Mm',ytitle='Mm'
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

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/phase_vel/13283950_'+strtrim(xx1,1)+strtrim(yy1,1)+'/time_series_13283950_'+strtrim(xx1,1)+strtrim(yy1,1)+'.eps'


!Y.OMargin = [2, 8]
!X.OMargin = [2, 6]
!P.Charsize=0.60

!P.Multi = [0,2,5]

tvim,ts,aspect=15,title='13283950_'+strtrim(coord(0),1)+strtrim(coord(1),1),xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

tvim,ts1,aspect=15,title='13283950_'+strtrim(coord1(0),1)+strtrim(coord1(1),1),xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

tvim,ts2,aspect=15,title='13283950_'+strtrim(coord2(0),1)+strtrim(coord2(1),1),xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

tvim,ts3,aspect=15,title='13283950_'+strtrim(coord3(0),1)+strtrim(coord3(1),1),xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

tvim,ts4,aspect=15,title='13283950_'+strtrim(coord4(0),1)+strtrim(coord4(1),1),xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

tvim,ts5,aspect=15,title='13283950_'+strtrim(coord5(0),1)+strtrim(coord5(1),1),xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

tvim,ts6,aspect=15,title='13283950_'+strtrim(coord6(0),1)+strtrim(coord6(1),1),xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

tvim,ts7,aspect=15,title='13283950_'+strtrim(coord7(0),1)+strtrim(coord7(1),1),xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

tvim,ts8,aspect=15,title='13283950_'+strtrim(coord8(0),1)+strtrim(coord8(1),1),xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

tvim,ts9,aspect=15,title='13283950_'+strtrim(coord9(0),1)+strtrim(coord9(1),1),xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'

!P.Multi = 0
!P.Charsize = 0
!Y.OMargin = [0, 0]
!X.OMargin = [0, 0]

device,/close

loadct,0

set_plot, 'x'

;save the time series

save,ts,description='Time series from core 13283950_'+strtrim(coord(0),1)+strtrim(coord(1),1),filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_13283950_'+strtrim(xx1,1)+strtrim(yy1,1)+'/ts_core_13283950_'+strtrim(coord(0),1)+strtrim(coord(1),1)+'.idl'

save,ts1,description='Time series from core 13283950_'+strtrim(coord1(0),1)+strtrim(coord1(1),1),filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_13283950_'+strtrim(xx1,1)+strtrim(yy1,1)+'/ts_core_13283950_'+strtrim(coord1(0),1)+strtrim(coord1(1),1)+'.idl'

save,ts2,description='Time series from core 13283950_'+strtrim(coord2(0),1)+strtrim(coord2(1),1),filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_13283950_'+strtrim(xx1,1)+strtrim(yy1,1)+'/ts_core_13283950_'+strtrim(coord2(0),1)+strtrim(coord2(1),1)+'.idl'

save,ts3,description='Time series from core 13283950_'+strtrim(coord3(0),1)+strtrim(coord3(1),1),filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_13283950_'+strtrim(xx1,1)+strtrim(yy1,1)+'/ts_core_13283950_'+strtrim(coord3(0),1)+strtrim(coord3(1),1)+'.idl'

save,ts4,description='Time series from core 13283950_'+strtrim(coord4(0),1)+strtrim(coord4(1),1),filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_13283950_'+strtrim(xx1,1)+strtrim(yy1,1)+'/ts_core_13283950_'+strtrim(coord4(0),1)+strtrim(coord4(1),1)+'.idl'

save,ts5,description='Time series from core 13283950_'+strtrim(coord5(0),1)+strtrim(coord5(1),1),filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_13283950_'+strtrim(xx1,1)+strtrim(yy1,1)+'/ts_core_13283950_'+strtrim(coord5(0),1)+strtrim(coord5(1),1)+'.idl'

save,ts6,description='Time series from core 13283950_'+strtrim(coord6(0),1)+strtrim(coord6(1),1),filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_13283950_'+strtrim(xx1,1)+strtrim(yy1,1)+'/ts_core_13283950_'+strtrim(coord6(0),1)+strtrim(coord6(1),1)+'.idl'

save,ts7,description='Time series from core 13283950_'+strtrim(coord7(0),1)+strtrim(coord7(1),1),filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_13283950_'+strtrim(xx1,1)+strtrim(yy1,1)+'/ts_core_13283950_'+strtrim(coord7(0),1)+strtrim(coord7(1),1)+'.idl'

save,ts8,description='Time series from core 13283950_'+strtrim(coord8(0),1)+strtrim(coord8(1),1),filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_13283950_'+strtrim(xx1,1)+strtrim(yy1,1)+'/ts_core_13283950_'+strtrim(coord8(0),1)+strtrim(coord8(1),1)+'.idl'

save,ts9,description='Time series from core 13283950_'+strtrim(coord9(0),1)+strtrim(coord9(1),1),filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/st_perp/time_series_13283950_'+strtrim(xx1,1)+strtrim(yy1,1)+'/ts_core_13283950_'+strtrim(coord9(0),1)+strtrim(coord9(1),1)+'.idl'
END