;Krishna Mooroogen.
;Northumbria University, Solar group.
;02/12/14.
;Routine to perform sanity check on errors using model fitting
;noise versus intesnity between time sections

;Assume data is a data cube for now add additional handling later.
;Might be worth adding peak det to estimate width and mean.

;PURPOSE: Compare noise estimates in astronomical data with modelled noise to check for discprencies.

;INPUTS: data: 3D data cube of from x,y,t.

pro error_3,dat

time_stamps=[0,227,455,681,908,1116]


FOR i=0,0 DO BEGIN
;maybe unsharp it so its equivilant to cross cut data,cut off edges
;change time stamp

;dat_im=unsharp(data=dat(5:950,15:950,time_stamps(i):time_stamps(i+1)),dx=9)

dat_im=dat(5:950,15:950,time_stamps(i):time_stamps(i+1))


;sum data over time, 20 frames omitting first frame to avoid time boundries
sum_dat_im=float(sum(dat_im(*,*,1:21),2))/20.0

sz=size(dat_im)

;FRAME BY FRAME pixel variance
;calulating diference
total_frame=fltarr((sz(1)-1),sz(2),20)

FOR j=1, 20 DO BEGIN
total_frame(*,*,(j-1))=dat_im(1:*,*,(j))-shift(dat_im(1:*,*,j),0,1)
ENDFOR

rms_noise=fltarr(sz(1),sz(2))
;take rms of each pixel in time
FOR j=0, (sz(1)-2) DO BEGIN
FOR k=0, (sz(2)-1) DO BEGIN
rms_noise(j,k)=rms(total_frame(j,k,*))
ENDFOR
ENDFOR

hd2 = hist_2d(sum_dat_im,rms_noise)
hd2 = alog(hd2>1)
;tvim,hd2
;hd2=fix(hd2(2000:5000,0:500))
;tvim,hd2
sz=size(hd2)
x=0
y=0

FOR i=0,(sz(1)-1) DO BEGIN
FOR j=0,(sz(2)-1) DO BEGIN
IF hd2(i,j) NE 0 THEN BEGIN
x=[temporary(x),i]
y=[temporary(y),j]
ENDIF
ENDFOR
ENDFOR

cgScatter2D,x,y,/ylog,xtitle='Average intensity',ytitle='rms pixel to pixel variation'
ENDFOR
END
