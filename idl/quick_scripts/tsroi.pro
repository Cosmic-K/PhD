sz=size(out_ts(*,*,0))
time = 1.343
Mm = (0.725/16.981891892)
xsz = time*(float(sz(2))-1.0)
ysz = Mm*(float(sz(1))-1.0)
x=findgen(208)
set_plot,'ps'

device,/encapsul,bits_per_pixel=8,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/tsroi.eps'

!X.MARGIN=[15,3]
;!Y.MARGIN=[9,2]


tvim,rotate(out_ts(*,*,11),4),aspect=10,/noaxis
axis,0,xaxis=0,charsize=1,xrange=[0,xsz],xtitle='Time(s)',xthick=3,charthick=4
axis,0,yaxis=0,charsize=0.8,yrange=[0,ysz],ytitle='Displacement(Mm)',ythick=3,charthick=4
axis,xaxis=1,xthick=3,XTICKFORMAT="(A1)"
axis,yaxis=1,ythick=3,YTICKFORMAT="(A1)"


oplot, x[0:*:10],tube_list[0:*:10,0]/Mm,psym=1,color=300,symsize=1,thick=2

device,/close



device,/close
set_plot,'x'