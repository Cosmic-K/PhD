sz=size(core)
Mm = (0.725/16.981891892)
xsz = Mm*(float(sz(1))-1.0)
ysz = Mm*(float(sz(2))-1.0)

set_plot,'ps'

device,/encapsul,bits_per_pixel=8 ,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/roi_im.eps'
tvim,core[*,*,0],xtitle='Displacement(Mm)',ytitle='Displacment(Mm)',xrange=[0,xsz],yrange=[0,ysz]
oplot,[100,220]*Mm,[400,400]*Mm,color=200,thick=2
oplot,[100,220]*Mm,[500,500]*Mm,color=200,thick=2
oplot,[100,100]*Mm,[400,500]*Mm,color=200,thick=2
oplot,[220,220]*Mm,[400,500]*Mm,color=200,thick=2

device,/close

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/roi2_im.eps'
tvim,unsharp(data=core[100:220,400:500],dx=15),xtitle='Displacement(Mm)',ytitle='Displacement(Mm)',xrange=[0,120]*Mm,yrange=[0,100]*Mm
device,/close

set_plot,'x'

