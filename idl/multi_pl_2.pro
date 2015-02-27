;Krishna Mooroogen.
;Northumbria University, Solar group.
;19/01/15.
;Routine to perform simulataneuous time series plots.

;NOTE
;maybe colorbar,maybe 0,725Mm = 1 arcsecond with 2% error, 1 pixel = 0.0592 then 1 pixel = 0.725/16.891891892 Mm
;maybe look at atrous on velocity,and or on dops, what kind of filtering, getting colours right
;DONT FORGET TO CHNAGE FILENAMES and COORDS BETWEEN EACH RUN


;CALL:
;multi_pl_2,tsint,vel=vel,dopp362=dopp362,dopp504=dopp504,dopp906=dopp906,tsvel,tsdop3,tsdop5,tsdop9

;PURPOSE:
;   Make time series plots of the same location for intensity/velocity/doppler images

;INPUTS
;       intsy = croped intensity image
;       vel = croped velcocity image
;       dop3 = croped doppler -362 imaged
;       dop5 = cropped doppler -504 image
;       dop9 = cropped doppler -906 image

;OUTPUS
;       tsint = times series for intensity
;       tsvel = time series for velocity
;       tsdop3 = time series for doppler -.362
;       tsdop5 = time series for doppler -.504
;       tsdop9 = time series for doppler -.906

;time is presented in 5min intevals, chnage as desired
;pixels secs
;223    300
;446    600
;670    900
;893    1200
;1116   1500

pro multi_pl_2,tsint,vel=vel,dop362=dop362,dop504=dop504,dop906=dop906,tsvel,tsdop3,tsdop5,tsdop9

loadct,0
!P.Multi = 0
set_plot, 'x'

sz=size(tsint)
ysz=float(sz(2))
xsz=float(sz(1))


Mm=(0.725/16.981891892)
time=1.343 ;every 300s 30s holes

t1=0
t2=(xsz-1)

width = 9
x11 = 600
x22 = 800
y11 = 700
y22 = 960

xcc1 = 55
xcc2 = 88
ycc1 = 105
ycc2 = 122

xrmax=(t2*time)
yrmax=Mm*(ysz-1.0)

cut = -35

;VARIABLES TO CHANGE AT DIFFERENT POSITIONS AS REQUIRED

unsh_vel = unsharp(data=vel(x11:x22,y11:y22,*),dx=20)
unsh_dop3 = unsharp(data=dop362(x11:x22,y11:y22,*),dx=width)
unsh_dop5 = unsharp(data=dop504(x11:x22,y11:y22,*),dx=width)
unsh_dop9 = unsharp(data=dop906(x11:x22,y11:y22,*),dx=width)

diag_slit2,unsh_vel,tsvel,x1=xcc1,x2=xcc2,y1=ycc1,y2=ycc2,coords=coord,noopen=noopen
wdelete,0
diag_slit2,unsh_dop3,tsdop3,x1=coord[0],x2=coord[1],y1=coord[2],y2=coord[3],noopen=noopen
wdelete,0
diag_slit2,unsh_dop5,tsdop5,x1=coord[0],x2=coord[1],y1=coord[2],y2=coord[3],noopen=noopen
wdelete,0
diag_slit2,unsh_dop9,tsdop9,x1=coord[0],x2=coord[1],y1=coord[2],y2=coord[3],noopen=noopen
wdelete,0

tsvel=rotate(tsvel,3)
tsdop3=rotate(tsdop3,3)
tsdop5=rotate(tsdop5,3)
tsdop9=rotate(tsdop9,3)

mask = tsint(t1:t2,*) lt cut
set_plot,'ps'
device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/strong/time_series_60807096_55105/time_series_60807096_55105_com.eps'
!Y.OMargin = [2, 8]
!X.OMargin = [2, 6]
!P.Charsize=0.60

!P.Multi = [0,2,5]

tvim,tsint(t1:t2,*),aspect=15,/noaxis
cgtext,0.265,0.88,'Intensity',/normal,alignment=0.5,charsize=0.55
cgtext,0.03,0.85,'Mm',/normal,alignment=0.5,charsize=0.5,orientation=90
axis,0,xaxis=0,charsize=0.8,xrange=[0,xrmax],xst=1,xtitle='Seconds'
axis,0,yaxis=0,charsize=0.5,yrange=[0,yrmax],yst=1

tvim,mask*tsint(t1:t2,*),aspect=15,/noaxis
cgtext,0.745,0.88,'Intensity Mask',/normal,alignment=0.5,charsize=0.55
cgtext,0.515,0.85,'Mm',/normal,alignment=0.5,charsize=0.5,orientation=90
axis,0,xaxis=0,charsize=0.8,xrange=[0,xrmax],xst=1,xtitle='Seconds'
axis,0,yaxis=0,charsize=0.5,yrange=[0,yrmax],yst=1

mg_loadct,22
tvim,tsvel(t1:t2,*),aspect=15,range=[-1000,1500],/rct,/noaxis
loadct,0,/silent
cgtext,0.265,0.7,'Velocity',/normal,alignment=0.5,charsize=0.55
cgtext,0.03,0.665,'Mm',/normal,alignment=0.5,charsize=0.5,orientation=90
axis,0,xaxis=0,charsize=0.8,xrange=[0,xrmax],xst=1,xtitle='Seconds'
axis,0,yaxis=0,charsize=0.5,yrange=[0,yrmax],yst=1

mg_loadct,22
tvim,mask*tsvel(t1:t2,*),aspect=15,range=[-1000,1500],/rct,/noaxis
loadct,0,/silent
cgtext,0.745,0.7,'Velocity Mask',/normal,alignment=0.5,charsize=0.55
cgtext,0.515,0.665,'Mm',/normal,alignment=0.5,charsize=0.5,orientation=90
axis,0,xaxis=0,charsize=0.8,xrange=[0,xrmax],xst=1,xtitle='Seconds'
axis,0,yaxis=0,charsize=0.5,yrange=[0,yrmax],yst=1

mg_loadct,22
tvim,tsdop3(t1:t2,*),aspect=15,range=[-200,200],/rct,/noaxis
loadct,0,/silent
cgtext,0.263,0.515,'Doppler -.362 16.5 km/s',/normal,alignment=0.5,charsize=0.55
cgtext,0.03,0.48,'Mm',/normal,alignment=0.5,charsize=0.5,orientation=90
axis,0,xaxis=0,charsize=0.8,xrange=[0,xrmax],xst=1,xtitle='Seconds'
axis,0,yaxis=0,charsize=0.5,yrange=[0,yrmax],yst=1

mg_loadct,22
tvim,mask*tsdop3(t1:t2,*),aspect=15,range=[-200,200],/rct,/noaxis
loadct,0,/silent
cgtext,0.745,0.515,'Doppler Mask -.362 16.5 km/s',/normal,alignment=0.5,charsize=0.55
cgtext,0.515,0.48,'Mm',/normal,alignment=0.5,charsize=0.5,orientation=90
axis,0,xaxis=0,charsize=0.8,xrange=[0,xrmax],xst=1,xtitle='Seconds'
axis,0,yaxis=0,charsize=0.5,yrange=[0,yrmax],yst=1

mg_loadct,22
tvim,tsdop5(t1:t2,*),aspect=15,range=[-200,200],/rct,/noaxis
loadct,0,/silent
cgtext,0.265,0.329,'Doppler -.504 24.8 km/s',/normal,alignment=0.5,charsize=0.55
cgtext,0.03,0.3,'Mm',/normal,alignment=0.5,charsize=0.5,orientation=90
axis,0,xaxis=0,charsize=0.8,xrange=[0,xrmax],xst=1,xtitle='Seconds'
axis,0,yaxis=0,charsize=0.5,yrange=[0,yrmax],yst=1

mg_loadct,22
tvim,mask*tsdop5(t1:t2,*),aspect=15,range=[-200,200],/rct,/noaxis
loadct,0,/silent
cgtext,0.515,0.3,'Mm',/normal,alignment=0.5,charsize=0.5,orientation=90
cgtext,0.745,0.329,'Doppler Mask -.504 24.8 km/s',/normal,alignment=0.5,charsize=0.55
axis,0,xaxis=0,charsize=0.8,xrange=[0,xrmax],xst=1,xtitle='Seconds'
axis,0,yaxis=0,charsize=0.5,yrange=[0,yrmax],yst=1

mg_loadct,22
tvim,tsdop9(t1:t2,*),aspect=15,range=[-40,60],/rct,/noaxis
loadct,0,/silent
cgtext,0.265,0.15,'Doppler -.906 41.4 km/s',/normal,alignment=0.5,charsize=0.55
cgtext,0.03,0.115,'Mm',/normal,alignment=0.5,charsize=0.5,orientation=90
axis,0,xaxis=0,charsize=0.8,xrange=[0,xrmax],xst=1,xtitle='Seconds'
axis,0,yaxis=0,charsize=0.5,yrange=[0,yrmax],yst=1

mg_loadct,22
tvim,mask*tsdop9(t1:t2,*),aspect=15,range=[-40,60],/rct,/noaxis
loadct,0,/silent
cgtext,0.745,0.15,'Doppler Mask-.906 41.4 km/s',/normal,alignment=0.5,charsize=0.55
cgtext,0.515,0.115,'Mm',/normal,alignment=0.5,charsize=0.5,orientation=90
axis,0,xaxis=0,charsize=0.8,xrange=[0,xrmax],xst=1,xtitle='Seconds'
axis,0,yaxis=0,charsize=0.5,yrange=[0,yrmax],yst=1

!P.Multi = 0
!P.Charsize = 0
!Y.OMargin = [0, 0]
!X.OMargin = [0, 0]

device,/close

loadct,0

set_plot, 'x'

save,tsvel,description='Time series from Velocity core ',filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/strong/time_series_60807096_55105/ts_vel_60807096_55105.idl'
save,tsdop3,description='Time series from Doppler 362 ',filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/strong/time_series_60807096_55105/ts_dop3_60807096_55105.idl'
save,tsdop5,description='Time series from Doppler 504 ',filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/strong/time_series_60807096_55105/ts_dop5_60807096_55105.idl'
save,tsdop9,description='Time series from Doppler 906 ',filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/timeseries/strong/time_series_60807096_55105/ts_dop9_60807096_55105.idl'

return

END
