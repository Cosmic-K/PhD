;Krishna Mooroogen.
;Northumbria University, Solar group.
;02/12/14.
;Routine to plot several masked images of features in different image types.


;Assume data is a data cube for now add additional handling later.
;Might be worth adding peak det to estimate width and mean.
;Add plot option to switch between plots?
;multiplots,overploting histograms


;CALL: mask_pl,int,vel,dopp3,dopp5,dopp9

;PURPOSE: Make plots of unsharped images with masking on and off to compare intensity feature regions
;         with vel/doppler images

;INPUTS:
;
;


pro mask_pl,int,vel,dopp362,dopp504,dopp906,unsh_int,unsh_vel,unsh_dop3,unsh_dop5,unsh_dop9

width = 9
x1 = 190
x2 = 290
y1 = 320
y2 = 400

cut = -65

unsh_int = unsharp(data=int(x1:x2,y1:y2,*),dx=width)
unsh_vel = unsharp(data=vel(x1:x2,y1:y2,*),dx=width)
unsh_dop3 = unsharp(data=dopp362(x1:x2,y1:y2,0:1133),dx=width)
unsh_dop5 = unsharp(data=dopp504(x1:x2,y1:y2,0:1133),dx=width)
unsh_dop9 = unsharp(data=dopp906(x1:x2,y1:y2,0:1133),dx=width)

mask = unsh_int lt cut

set_plot,'ps'

device,/encapsul,/color,filename='vel_mask_.eps'
!P.Multi = [0,2,1]

tvim,unsh_vel(*,*,0)
tvim,mask*unsh_vel(*,*,0)

cgtext,0.5,0.1,'Velocity',alignment=0.5,/normal,charsize=0.60


!P.Multi = 0

device,/close

device,/encapsul,/color,filename='dop3_mask_.eps'
!P.Multi = [0,2,1]

tvim,unsh_dop3(*,*,0)
tvim,mask*unsh_dop3(*,*,0)

cgtext,0.5,0.1,'Doppler -.362 165.5 km/s',alignment=0.5,/normal,charsize=0.60

!P.Multi = 0

device,/close

device,/encapsul,/color,filename='dop5_mask_.eps'
!P.Multi = [0,2,1]

tvim,unsh_dop5(*,*,0)
tvim,mask*unsh_dop5(*,*,0)

cgtext,0.5,0.1,'Doppler -.504 248.2 km/s',alignment=0.5,/normal,charsize=0.60


!P.Multi = 0

device,/close

device,/encapsul,/color,filename='dop9_mask_.eps'
!P.Multi = [0,2,1]

tvim,unsh_dop9(*,*,0)
tvim,mask*unsh_dop9(*,*,0)

cgtext,0.5,0.1,'Doppler -.906 413.5 km/s',alignment=0.5,/normal,charsize=0.60

!P.Multi = 0

device,/close

set_plot, 'x'

return

END
