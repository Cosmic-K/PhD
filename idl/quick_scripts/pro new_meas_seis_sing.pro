PRO new_meas_seis_sing,files

Mm = (0.725/16.981891892)
km=Mm*1000.0
time=1.343

FOR i=0, n_elements(files)-1 DO BEGIN

restore,files[i]
var=threads_fit_fg.fit_result_pos
var=var[*,1:1]

IF n_elements(var) EQ 18 THEN BEGIN

DOF=(var(12)-var(11))-5
chi=var(10)
redchi=float(chi)/float(dof)
ENDIF

IF n_elements(var) EQ 19 THEN BEGIN
;print,'poo'
DOF=var(11)
chi=var(10)
redchi=float(chi)/float(dof)
ENDIF

ad=[temporary(ad),abs(var(1))]
ade=[temporary(ade),(sqrt((var(6)^2)*redchi))]

ENDFOR
ad=ad[1:*]
ade=ade[1:*]

amps_norm = ad/ad(0)
rho=reform((1/amps_norm)^4)
rho_er = reform(rho*sqrt((4*amp_er/amps)^2+(4*amp_er(0)/amps(0))^2))
x=findgen(n_elements(rho))*km


!X.MARGIN=[10,3]
!Y.MARGIN=[4,2]

cgwindow,'cgplot',x,rho,xtitle='Height along structure (km)',ytitle='$\rho$/$\rho$0',charsize=1.5 ,charthick=1,xthick=1.5,ythick=1.5,thick=1.5,yrange=[-0.5,3],xrange=[-0.02,0.4],xstyle=1,ystyle=1,/nodata
ycon=exp(-x/250)


cgplot,x,ycon,/window,/overplot,thick=2,color='red',linestyle=3
cgplot,x,rho,/window,/overplot,thick=2,color='blue'
cgerrplot,x,rho+rho_er,rho-rho_er,/addcmd,thick=1,width=0.005,color='blue'


END