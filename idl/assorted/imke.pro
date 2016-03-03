set_plot,'ps'
device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/firstplot.eps'
tvim,total(dat[*,*,0:*],3),/noaxis
axis,0,xaxis=0,charsize=0.8,xthick=1,charthick=2
axis,0,yaxis=0,charsize=0.8,ythick=1,charthick=2
axis,xaxis=1,xthick=1,XTICKFORMAT="(A1)"
axis,yaxis=1,ythick=1,YTICKFORMAT="(A1)"
cgtext,19,92,'Sum of the images from time=0 to time=227',charsize=1.2,charthick=2,/data
device,/close


device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/secondplot.eps'
tvim,total(dat[*,*,0:*],3),/noaxis
axis,0,xaxis=0,charsize=0.8,xthick=1,charthick=2
axis,0,yaxis=0,charsize=0.8,ythick=1,charthick=2
axis,xaxis=1,xthick=1,XTICKFORMAT="(A1)"
axis,yaxis=1,ythick=1,YTICKFORMAT="(A1)"
cgtext,19,92,'Sum of the images from time=0 to time=227',charsize=1.2,charthick=2,/data

plots,g[0,0],g[0,1],psym=2,color=250
plots,g[1,0],g[1,1],psym=2,color=250
plots,g[2,0],g[2,1],psym=2,color=250
plots,g[3,0],g[3,1],psym=2,color=250
plots,g[4,0],g[4,1],psym=2,color=250
plots,g[5,0],g[5,1],psym=2,color=250

oplot,c[*,0],c[*,1],color=250
device,/close


device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/unsh.eps'
tvim,unsharp(data=total(dat[*,*,0:*],3),dx=10),/noaxis
axis,0,xaxis=0,charsize=0.8,xthick=1,charthick=2
axis,0,yaxis=0,charsize=0.8,ythick=1,charthick=2
axis,xaxis=1,xthick=1,XTICKFORMAT="(A1)"
axis,yaxis=1,ythick=1,YTICKFORMAT="(A1)"
cgtext,19,92,'Sum of the images from time=0 to time=227',charsize=1.2,charthick=2,/data
plots,g[0,0],g[0,1],psym=2,color=250
plots,g[1,0],g[1,1],psym=2,color=250
plots,g[2,0],g[2,1],psym=2,color=250
plots,g[3,0],g[3,1],psym=2,color=250
plots,g[4,0],g[4,1],psym=2,color=250
plots,g[5,0],g[5,1],psym=2,color=250
oplot,c[*,0],c[*,1],color=250
device,/close





device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/vec.eps'
tvim,total(dat[*,*,0:*],3),/noaxis
axis,0,xaxis=0,charsize=0.8,xthick=1,charthick=2
axis,0,yaxis=0,charsize=0.8,ythick=1,charthick=2
axis,xaxis=1,xthick=1,XTICKFORMAT="(A1)"
axis,yaxis=1,ythick=1,YTICKFORMAT="(A1)"
cgtext,19,92,'Sum of the images from time=0 to time=227',charsize=1.2,charthick=2,/data

plots,g[0,0],g[0,1],psym=2,color=250
plots,g[1,0],g[1,1],psym=2,color=250
plots,g[2,0],g[2,1],psym=2,color=250
plots,g[3,0],g[3,1],psym=2,color=250
plots,g[4,0],g[4,1],psym=2,color=250
plots,g[5,0],g[5,1],psym=2,color=250

oplot,c[*,0],c[*,1],color=250
partvelvec,norm[*,3],norm[*,4],norm[*,0],norm[*,1],/over,VecColor='green'

device,/close


device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/td.eps'
tvim,rotate(t[*,*,5],3),aspect=10,/noaxis
axis,0,xaxis=0,charsize=0.8,xthick=1,charthick=2
axis,0,yaxis=0,charsize=0.8,ythick=1,charthick=2
axis,xaxis=1,xthick=1,XTICKFORMAT="(A1)"
axis,yaxis=1,ythick=1,YTICKFORMAT="(A1)"
device,/close

set_plot,'x'
