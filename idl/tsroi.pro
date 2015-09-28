sz=size(out_ts(*,*,0))
sz1=size(OUT_TS_VEL_CON(*,*,0))
time = 1.343
Mm = (0.725/16.981891892)
xsz = time*(float(sz(2))-1.0)
ysz = Mm*(float(sz(1))-1.0)
ysz1 = Mm*(float(sz1(1))-1.0)
x=findgen(162)*time
x2=findgen(163)*time
set_plot,'ps'

device,/encapsul,bits_per_pixel=8,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/tsroi.eps'

tvim,rotate(out_ts(*,66:*,0),4),aspect=10,xrange=[0,162]*time,yrange=[0,ysz], xtitle='Time(s)',ytitle='Displacement(Mm)',pcharsize=0.65
oplot, x[0:*:10],tube_list[66:*:10,0],psym=1,color=300,symsize=1

device,/close

y1=tube_list[*,0]+(8*Mm)
y2=tube_list[*,0]+(2*Mm)

device,/encapsul,bits_per_pixel=8,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/veltsroi.eps

mg_loadct,22,/silent
tvim,rotate(OUT_TS_VEL_CON(*,66:*,0),4),aspect=10,xrange=[0,162]*time,yrange=[0,ysz1],range=[-2000,2000],/rct,pcharsize=0.59,/noaxis
loadct,0,/silent
axis,0,xaxis=0,charsize=0.59,xrange=[0,162]*time,xst=1,xtitle='Time(s)'
axis,0,yaxis=0,charsize=0.59,yrange=[0,ysz1],yst=1,ytitle='Displacement(Mm)'

plots,[0,162]*time,[25,25]*Mm,linestyle=1,color=0,thick=3
plots,[0,162]*time,[4,4]*Mm,linestyle=1,color=0,thick=3
oplot,x2,y1[65:*],color=0,thick=2
oplot,x2,y2[65:*],color=0,thick=2


device,/close
set_plot,'x'