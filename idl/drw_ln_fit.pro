pro drw_ln_fit,data

determine_window_size,xsize=xsize,ysize=ysize,openwin=openwin,/new_win
window,openwin[0],xs=xsize,ys=ysize
window_var=openwin[0]
ENDIF ELSE BEGIN
wset,win_id
window_var=win_id
ENDELSE
window,xs=xsize,ys=ysize,/pixmap,/free
pixID=!d.window
tvim,data[*,*,fn]
wset,window_var
device,copy=[0,0,!d.x_size,!d.y_size,0,0,pixid]


PRINT,'Select point with cursor'
CURSOR,x1,y1,/Data,/Down
print,  'x1='+strtrim(string(fix(x1)),1), ' y1='+strtrim(string(fix(y1)),1)
PRINT,'Select point with cursor'

IF !MOUSE.button EQ 1 THEN BEGIN
CURSOR, do_nothing_x, do_nothing_y, /UP, /WAIT
!MOUSE.button=0
ENDIF
WHILE (!MOUSE.button NE 1) DO BEGIN
CURSOR,x2,y2,/change
window,xs=xsize,ys=ysize,/pixmap,/free
pixID=!d.window
tvim,data[*,*,fn]
wset,window_var
device,copy=[0,0,!d.x_size,!d.y_size,0,0,pixid]
PLOTS,[x1,x2],[y1,y2]
ENDWHILE
print,  'x2='+strtrim(string(fix(x2)),1), ' y2='+strtrim(string(fix(y2)),1)

x1=fix(x1)
x2=fix(x2)
y1=fix(y1)
y2=fix(y2)

;USE INTERPOLATION - increase array by a factor of 2x2x1
;expanded=fltarr(2.*nx,2.*ny,zsize,/nozero)
;FOR i=0,zsize-1 DO expanded[0,0,i]=congrid(reform(data[0:nx-1,0:ny-1,i]),2.*nx,2.*ny,cubic=-0.5)

x1=2.*x1 & x2=2.*x2 & y1=2.*y1 & y2=2.*y2



m=(1.0*(y2-y1))/(1.0*(x2-x1))
c=y1-m*x1