;Krishna Mooroogen.
;Northumbria University, Solar group.
;19/01/15.
;Routine to perform simulataneuous time series plots.

;NOTE
;   Before starting routine, etsablish where we want to make cross cuts
;   on intensity i.e. where to crop image, input a array, make unsharped/masked/time series

;CALL:
;multi_pl,intsy=intsy,vel=vel,dopp362=dopp362,dopp504=dopp504,dopp906=dopp906,tsint=tsint,tsvel=tsvel,tsdop3=tsdop3,tsdop5=tsdop5,tsdop9=tsdop9

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


pro multi_pl,intsy=intsy,vel=vel,dopp362=dopp362,dopp504=dopp504,dopp906=dopp906,tsvel=tsvel,tsdop3=tsdop3,tsdop5=tsdop5,tsdop9=tsdop9

!P.Multi = 0

set_plot, 'x'

t1=0
t2=1100

width = 9
x1 = 480
x2 = 600
y1 = 600
y2 = 765

cut = -35

;VARIABLES TO CHANGE AT DIFFERENT POSITIONS AS REQUIRED


unsh_int = unsharp(data=intsy(x1:x2,y1:y2,*),dx=width)
unsh_vel = unsharp(data=vel(x1:x2,y1:y2,*),dx=width)
unsh_dop3 = unsharp(data=dopp362(x1:x2,y1:y2,*),dx=width)
unsh_dop5 = unsharp(data=dopp504(x1:x2,y1:y2,*),dx=width)
unsh_dop9 = unsharp(data=dopp906(x1:x2,y1:y2,*),dx=width)

diag_slit2,unsh_int,tsint,coords=coord
wdelete,0
diag_slit2,unsh_vel,tsvel,x1=coord[0],x2=coord[1],y1=coord[2],y2=coord[3],noopen=noopen
wdelete,0
diag_slit2,unsh_dop3,tsdop3,x1=coord[0],x2=coord[1],y1=coord[2],y2=coord[3],noopen=noopen
wdelete,0
diag_slit2,unsh_dop5,tsdop5,x1=coord[0],x2=coord[1],y1=coord[2],y2=coord[3],noopen=noopen
wdelete,0
diag_slit2,unsh_dop9,tsdop9,x1=coord[0],x2=coord[1],y1=coord[2],y2=coord[3],noopen=noopen
wdelete,0


mask = tsint lt cut
;clip vlos
;look up device idl for coloring rick says 70 or 73
;maybe make masks for all and do a big comparison
;include aspect keyword to stretch


set_plot,'ps'
device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/time_series_.eps'

!Y.OMargin = [2, 8]
!X.OMargin = [2, 6]
!P.Charsize=0.60

!P.Multi = [0,2,5]

tvim,rotate(tsint(*,t1:t2),3),pcharsize=0.6,xrange=[t1,t2],title='Intensity',aspect=10
tvim,rotate(mask*tsint(*,t1:t2),3),pcharsize=0.6,xrange=[t1,t2],title='Intensity Mask',aspect=10
mg_loadct,22,
tvim,rotate(tsvel(*,t1:t2),3),pcharsize=0.6,xrange=[t1,t2],title='Velocity',aspect=10,range=[-2000,1800];,clip=[10],/rct
tvim,rotate(mask*tsvel(*,t1:t2),3),pcharsize=0.6,xrange=[t1,t2],title='Velocity Mask',aspect=10,range=[-2000,1800];,clip=[10],/rct
loadct,0
tvim,rotate(tsdop3(*,t1:t2),3),pcharsize=0.6,xrange=[t1,t2],title='Doppler -.362 16.5 km/s',aspect=10
tvim,rotate(mask*tsdop3(*,t1:t2),3),pcharsize=0.6,xrange=[t1,t2],title='Doppler Mask -.362 16.5 km/s',aspect=10
tvim,rotate(tsdop5(*,t1:t2),3),pcharsize=0.6,xrange=[t1,t2],title='Doppler -.504 248.2 km/s',aspect=10
tvim,rotate(mask*tsdop5(*,t1:t2),3),pcharsize=0.6,xrange=[t1,t2],title='Doppler Mask -.504 24.8 km/s',aspect=10
tvim,rotate(tsdop9(*,t1:t2),3),pcharsize=0.6,xrange=[t1,t2],title='Doppler -.906 413.5 km/s',aspect=10
tvim,rotate(mask*tsdop9(*,t1:t2),3),pcharsize=0.6,xrange=[t1,t2],title='Doppler Mask -.906 41.4 km/s',aspect=10




!P.Multi = 0
!P.Charsize = 0
!Y.OMargin = [0, 0]
!X.OMargin = [0, 0]

device,/close

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/cross_cuts_.eps'

!P.Multi = [0,2,3]

loadct,0

tvim,(unsh_int(*,*,0)),title='Intesnity'
p1 = !P & x1 = !X & y1 = !Y
!P = p1 & !X = x1 & !Y = y1
plots,[coord[0],coord[1]],[coord[2],coord[3]],color=450


mg_loadct,22

tvim,unsh_vel(*,*,0),title='Velocity',range=[-2000,1800];,clip=[10],/rct
p2 = !P & x2 = !X & y2 = !Y
!P = p2 & !X = x2 & !Y = y2
plots,[coord[0],coord[1]],[coord[2],coord[3]],color=450

loadct,0

tvim,unsh_dop3(*,*,0),title='Doppler -.362 16.5 km/s'
p3 = !P & x3 = !X & y3 = !Y
!P = p3 & !X = x3 & !Y = y3
plots,[coord[0],coord[1]],[coord[2],coord[3]],color=450


tvim,unsh_dop5(*,*,0),title='Doppler -.504 24.8 km/s'
p4 = !P & x4 = !X & y4 = !Y
!P = p4 & !X = x4 & !Y = y4
plots,[coord[0],coord[1]],[coord[2],coord[3]],color=450


tvim,unsh_dop9(*,*,0),title='Doppler -.906 41.4 km/s'
p5 = !P & x5 = !X & y5 = !Y
!P = p5 & !X = x5 & !Y = y5
plots,[coord[0],coord[1]],[coord[2],coord[3]],color=450

!P.Multi = 0

device,/close

set_plot, 'x'


return

END
