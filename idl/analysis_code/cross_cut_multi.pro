;Krishna Mooroogen.
;Northumbria University, Solar group.
;krishna.mooroogen@northumbria.ac.uk
;PhD supervisor RIchard Morton


;NOTES
;--------------------------------------------------------------------------------
;Make cropped data before input. Crop in time as well before input if necessary.
;need to plot some kind of phase diagram with units


;CALL:
;---------------------------------------------------------------------------------
;cross_cut_multi,data_in=data_in,data_vel=data_vel,xx1=xx1,xx2=xx2,yy1=yy1,yy2=yy2

;PURPOSE:
;---------------------------------------------------------------------------------
;Routine to perform simulataneuous time series plots across single feature.
;Make time series plots perpindicular across feature to create phase diagram
;Make time series of features in b intensity and velocity, or any er two input files
;plot and save datas

;INPUTS
;----------------------------------------------------------------------------------
;input data must be cropped 3d data
;input velocity data
;z is a dummy, just so that the line define works as xx1 gets reasigned, when coords are put in manually set z to any integer.
;z option only good for singe time slice run erwise it will use the same coords, might chnage later
;starting coordinates optional
;WATCH OUT FOR EDGE EFFECTS ON THE DATA IMAGE MAY NOT SHOW UP ON AVERAGE IMAGE

;OUTPUS
;----------------------------------------------------------------------------------
;datas of FOV, time series and summed fov
;Time series data 0-9 across feature line


pro cross_cut_multi,im_in=im_in,im_vel=im_vel,z=z,xx1=xx1,xx2=xx2,yy1=yy1,yy2=yy2

;SETTING PLOTTING MARGINS
;---------------------------------------------------------------------------------
set_plot,'x'
loadct,0
!P.Multi = 0
!P.Charsize = 0
!Y.OMargin = [0, 0]
!X.OMargin = [0, 0]

;CONSTANTS
;---------------------------------------------------------------------------------

Mm = (0.725/16.981891892)
sz = size(im_in)
xsz = Mm*(float(sz(1))-1.0)
ysz = Mm*(float(sz(2))-1.0)
time = 1.343
time_stamps=[0,227,455,681,908,1116]



;BEGINING TO LOOP OVER TIME INTERVALS IN TIME STAMPS
;----------------------------------------------------------------------------------

FOR i=0,0 DO BEGIN

;CHNAGE DATA NAME SO OUTPUT NOT CHNAGED
;
data_in = im_in

;INITIAL PLOTTING SETUP AND TIME AVERAGING
;---------------------------------------------------------------------------------

IF i EQ 0 THEN BEGIN
data_in=unsharp(data=data_in(*,*,time_stamps(i):time_stamps(i+1)),dx=14)
ENDIF ELSE BEGIN
data_in=unsharp(data=data_in(*,*,(time_stamps(i)+1):time_stamps(i+1)),dx=14)
ENDELSE

sum_im = sum(data_in,2)


;check for second data

IF n_elements(im_vel) NE 0. THEN BEGIN
data_vel = im_vel
IF i EQ 0 THEN BEGIN
data_vel=data_vel(*,*,time_stamps(i):time_stamps(i+1))
ENDIF ELSE BEGIN
data_vel=data_vel(*,*,(time_stamps(i)+1):time_stamps(i+1))
ENDELSE
sum_im1 = sum(data_vel,2)
ENDIF


check='N'

IF n_elements(z) EQ 0. THEN BEGIN
WHILE check NE 'Y' DO BEGIN
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

;SELECTING STRCUTURE GUIDLINE
;------------------------------------------------------------------------------------------------
PRINT,'Select point with cursor'
CURSOR,xx1,yy1,/Data,/Down
print,  'xx1='+strtrim(string(fix(xx1)),1), ' yy1='+strtrim(string(fix(yy1)),1)

PRINT,'Select point with cursor'
IF !MOUSE.button EQ 1 THEN BEGIN
CURSOR, do_ning_x, do_ning_y, /UP, /WAIT
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

READ,check,PROMPT='Are you happy with the line? Y/N   '

wdelete,window_var
ENDWHILE
ENDIF


xx1=fix(xx1)
yy1=fix(yy1)
xx2=fix(xx2)
yy2=fix(yy2)

px1=xx1
py1=yy1
px2=xx2
py2=yy2

;CHECKING SAVE FILE LOCATION
;---------------------------------------------------------------------------------------------------------------

result=file_test('/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/seismol/80967689_'+strtrim(px1,1)+strtrim(py1,1)+'_'+strtrim(i,1),/directory)
result1=file_test('/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/ts_seis/time_series_'+strtrim(px1,1)+strtrim(py1,1)+'_'+strtrim(i,1),/directory)

IF result EQ 0 THEN FILE_MKDIR,'/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/seismol/80967689_'+strtrim(px1,1)+strtrim(py1,1)+'_'+strtrim(i,1)


IF result EQ 0 THEN FILE_MKDIR,'/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/ts_seis/time_series_80967689_'+strtrim(px1,1)+strtrim(py1,1)+'_'+strtrim(i,1)


;MAKING TIME SERIES USING RICKS WAVE TRACKING
;---------------------------------------------------------------------------------------------------------------
;time series will be output 3d x,t number of slits already rotated
;coords will be the coords of the line that follows the feature


sep = ' '
n_ts = ' '
length = ' '


READ,sep,PROMPT='%Cross cut seperation number? Default is 2.  '
READ,n_ts,PROMPT='%Number of time series? Default is 10.  '
READ,length,PROMPT='%Half length of time series? Default is 10.  '

IF sep eq 0 THEN sep = 2
IF n_ts eq 0 THEN n_ts = 10
IF length eq 0 THEN length = 10

sep = fix(sep)
sep2=sep
sep3=sep
n_ts = fix(n_ts)
len = fix(length)
len1=len


wave_track,sep,n_ts,len,data_in,out_ts,p_coord=pcoords,arr=arr,x1=xx1,x2=xx2,y1=yy1,y2=yy2,actsep=actsep,/noopen

tcoords=arr
;pcoords1=pcoords

IF n_elements(im_vel) NE 0 THEN BEGIN
dt_vel=data_vel
;CHECK SEP VALUE HERE
wave_track,sep2,n_ts,len1,data_vel,out_ts_vel,p_coord=pcoords1,x1=pcoords(0), x2=pcoords(1),y1=pcoords(2),y2=pcoords(3),/noopen
wave_track,sep3,n_ts,(fix(len1/2)+5),dt_vel,out_ts_vel_con,x1=pcoords1(0), x2=pcoords1(1),y1=pcoords1(2),y2=pcoords1(3),/noopen

ENDIF

;PLOTTING

szts = size(out_ts)
tsxsz = time*(float(szts(2))-1.0)
tsysz = Mm*(float(szts(1))-1.0)

;PLOTTING FOV'S AND SAVING T/D PLOTS
;--------------------------------------------------------------------------------------------------------------------------------------------------------'
set_plot,'ps'

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/roi2.eps
;Data/seismol/80967689_'+strtrim(px1,1)+strtrim(py1,1)+'_'+strtrim(i,1)+'/fov_sum_80967689_'+strtrim(px1,1)+strtrim(py1,1)+'.eps'

!Y.OMargin = [5, 3]
!X.OMargin = [5, 2]

tvim,sum_im(*,*),/noaxis
axis,0,xaxis=0,charsize=1.5,xrange=[0,xsz],xtitle='Displacement (Mm)',xthick=3,charthick=4
axis,0,yaxis=0,charsize=1.5,yrange=[0,ysz],ytitle='Displacement (Mm)',ythick=3,charthick=4
axis,xaxis=1,xthick=3,XTICKFORMAT="(A1)"
axis,yaxis=1,ythick=3,YTICKFORMAT="(A1)"

plots,[(px1),(px2)],[(py1),(py2)],color=300,thick=3
FOR j=0,(szts(3)-1) DO BEGIN
plots,[(tcoords[0,j]),(tcoords[1,j])],[(tcoords[2,j]),(tcoords[3,j])],color=450,thick=3
cgtext,tcoords[0,j]+0.1,tcoords[2,j],strtrim((j+1),1),charsize=0.7, charthick=3,/data
ENDFOR

device,/close

stop

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/seismol/80967689_'+strtrim(px1,1)+strtrim(py1,1)+'_'+strtrim(i,1)+'/fov_80967689_'+strtrim(px1,1)+strtrim(py1,1)+'.eps'
!Y.OMargin = [2, 8]
!X.OMargin = [2, 6]
!P.Charsize=0.60

tvim,data_in(*,*,0),xrange=[0,xsz],yrange=[0,ysz],xtitle='Mm',ytitle='Mm'
plots,[(Mm*px1),(Mm*px2)],[(Mm*py1),(Mm*py2)],color=450
FOR k=0,(szts(3)-1) DO BEGIN
plots,[(Mm*tcoords[0,k]),(Mm*tcoords[1,k])],[(Mm*tcoords[2,k]),(Mm*tcoords[3,k])],color=450

ENDFOR

device,/close

;If velocity

IF n_elements(im_vel) NE 0 THEN BEGIN

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/seismol/80967689_'+strtrim(px1,1)+strtrim(py1,1)+'_'+strtrim(i,1)+'/fov_sum_vel_80967689_'+strtrim(px1,1)+strtrim(py1,1)+'.eps'

!Y.OMargin = [2, 8]
!X.OMargin = [2, 6]
!P.Charsize=0.60

mg_loadct,22

tvim,sum_im1(*,*),xrange=[0,xsz],yrange=[0,ysz],xtitle='Mm',ytitle='Mm',/rct
plots,[(Mm*px1),(Mm*px2)],[(Mm*py1),(Mm*py2)]
FOR h=0,(szts(3)-1) DO BEGIN
plots,[(Mm*tcoords[0,h]),(Mm*tcoords[1,h])],[(Mm*tcoords[2,h]),(Mm*tcoords[3,h])];,color=450
cgtext,Mm*tcoords[0,h]+0.1,Mm*tcoords[2,h],strtrim((h+1),1),/data
ENDFOR

device,/close

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/seismol/80967689_'+strtrim(px1,1)+strtrim(py1,1)+'_'+strtrim(i,1)+'/fov_vel_80967689_'+strtrim(px1,1)+strtrim(py1,1)+'.eps'


!Y.OMargin = [2, 8]
!X.OMargin = [2, 6]
!P.Charsize=0.60

tvim,data_vel(*,*,0),title='Core velocity, 80967689_'+strtrim(px1,1)+strtrim(py1,1),xrange=[0,xsz],yrange=[0,ysz],xtitle='Mm',ytitle='Mm',/rct
plots,[(Mm*px1),(Mm*px2)],[(Mm*py1),(Mm*py2)]
FOR g=0,(szts(3)-1) DO BEGIN
plots,[(Mm*tcoords[0,g]),(Mm*tcoords[1,g])],[(Mm*tcoords[2,g]),(Mm*tcoords[3,g])];,color=450
ENDFOR

device,/close
ENDIF

loadct,0

;plot timeeries

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/seismol/80967689_'+strtrim(px1,1)+strtrim(py1,1)+'_'+strtrim(i,1)+'/time_series_80967689_'+strtrim(px1,1)+strtrim(py1,1)+'.eps'


!Y.OMargin = [2, 8]
!X.OMargin = [2, 6]
!P.Charsize=0.60

ans=evod(n_ts)

IF ans EQ 0 THEN BEGIN
!P.Multi = [0,2,n_ts]
ENDIF ELSE BEGIN
!P.Multi = [0,2,(n_ts+1)]
ENDELSE

FOR f=0,(n_ts-1) DO BEGIN
tvim,rotate(out_ts(*,*,f),3),aspect=15,title='Intensity 80967689_'+strtrim(tcoords(0,f),1)+strtrim(tcoords(2,f),1),xrange=[0,tsxsz],yrange=[0,tsysz],xtitle='Seconds',ytitle='Mm'
ENDFOR

!P.Multi = 0
!P.Charsize = 0
!Y.OMargin = [0, 0]
!X.OMargin = [0, 0]

device,/close


IF n_elements(im_vel) NE 0 THEN BEGIN

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/seismol/80967689_'+strtrim(px1,1)+strtrim(py1,1)+'_'+strtrim(i,1)+'/time_series_vel_80967689_'+strtrim(px1,1)+strtrim(py1,1)+'.eps'


!Y.OMargin = [2, 10]
!X.OMargin = [2, 10]
!P.Charsize=0.60

ans=evod(n_ts)

IF ans EQ 0 THEN BEGIN
!P.Multi = [0,2,n_ts]
ENDIF ELSE BEGIN
!P.Multi = [0,2,(n_ts+1)]
ENDELSE

perc = 0.10

FOR e=0,(n_ts-1) DO BEGIN

mask=out_ts(*,*,e) lt -35

mg_loadct,22
tvim,rotate(mask*out_ts_vel(*,*,e),3),aspect=15,title=strtrim(tcoords(0,e),1)+strtrim(tcoords(2,e),1),range=[(perc*float(min(out_ts_vel(*,*,e)))),(perc*float(max(out_ts_vel(*,*,e))))],/rct,/noaxis
loadct,0,/silent
axis,0,xaxis=0,charsize=0.6,xrange=[0,tsxsz],xtitle='Seconds'
axis,0,yaxis=0,charsize=0.6,yrange=[0,tsysz],ytitle='Mm'

ENDFOR

!P.Multi = 0
!P.Charsize = 0
!Y.OMargin = [0, 0]
!X.OMargin = [0, 0]

device,/close
ENDIF

loadct,0

set_plot, 'x'

;SAVING T/D DIAGARAMS IN ARRAY
;--------------------------------------------------------------------------------------------------------------------------------------------------------------

save,out_ts,description='Time series from intensity core 80967689_'+strtrim(px1,1)+strtrim(py1,1)+strtrim(px2,1)+strtrim(py2,1)+' number of slits '+strtrim(n_ts,1)+' steps '+strtrim(actsep,1),filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/ts_seis/time_series_80967689_'+strtrim(px1,1)+strtrim(py1,1)+'_'+strtrim(i,1)+'/ts_core_80967689_'+strtrim(px1,1)+strtrim(py1,1)+'.idl'

IF n_elements(im_vel) NE 0 THEN BEGIN

save,out_ts_vel,description='Time series from velocity core 80967689_'+strtrim(px1,1)+strtrim(py1,1)+strtrim(px2,1)+strtrim(py2,1)+' number of slits '+strtrim(n_ts,1)+' steps '+strtrim(actsep,1),filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/ts_seis/time_series_80967689_'+strtrim(px1,1)+strtrim(py1,1)+'_'+strtrim(i,1)+'/ts_vel_80967689_'+strtrim(px1,1)+strtrim(py1,1)+'.idl'

save,out_ts_vel_con,description='Time series from velocity core context 80967689_'+strtrim(px1,1)+strtrim(py1,1)+strtrim(px2,1)+strtrim(py2,1)+' number of slits '+strtrim(n_ts,1)+' steps '+strtrim(actsep,1),filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/ts_seis/time_series_80967689_'+strtrim(px1,1)+strtrim(py1,1)+'_'+strtrim(i,1)+'/ts_vel_con_80967689_'+strtrim(px1,1)+strtrim(py1,1)+'.idl'
ENDIF

ENDFOR
END