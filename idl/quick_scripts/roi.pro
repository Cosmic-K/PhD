sz=size(core)
Mm = (0.725/16.981891892)
xsz = Mm*(float(sz(1))-1.0)
ysz = Mm*(float(sz(2))-1.0)

set_plot,'ps'

device,/encapsul,bits_per_pixel=8 ,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/roi.eps'

tvim,core[*,*,0],/noaxis

axis,0,xaxis=0,xtitle='Displacement(Mm)',xrange=[0,xsz],charsize=1.5,charthick=4,xthick=3
axis,0,yaxis=0,ytitle='Displacment(Mm)',yrange=[0,ysz],charsize=1.5,charthick=4,ythick=3
axis,xaxis=1,xthick=3,XTICKFORMAT="(A1)"
axis,yaxis=1,ythick=3,YTICKFORMAT="(A1)"

oplot,[150,270],[400,400],color=200,thick=3
oplot,[150,270],[500,500],color=200,thick=3
oplot,[150,150],[400,500],color=200,thick=3
oplot,[270,270],[400,500],color=200,thick=3

device,/close

;device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/roi2.eps'
;tvim,unsharp(data=core[100:220,400:500],dx=15),/noaxis

;axis,0,xaxis=0,xtitle='Displacement(Mm)',xrange=[0,120]*Mm,charsize=1.5,charthick=4,xthick=3
;axis,0,yaxis=0,ytitle='Displacment(Mm)',yrange=[0,100]*Mm,charsize=1.5,charthick=4,ythick=3
;axis,xaxis=1,xthick=3,XTICKFORMAT="(A1)"
;axis,yaxis=1,ythick=3,YTICKFORMAT="(A1)"

;device,/close

;set_plot,'x'

