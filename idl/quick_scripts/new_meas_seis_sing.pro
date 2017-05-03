PRO new_meas_seis_sing,files

Mm = (0.725/16.981891892)
km=Mm*1000.0
time=1.343
ad=fltarr(111)
ade=fltarr(111)

FOR i=0, n_elements(files)-1 DO BEGIN

restore,files[i]
var=threads_fit_fg.fit_result_pos
var=var[where(var ne 0)]

IF n_elements(var) EQ 14 THEN BEGIN

DOF=(var(12)-var(11))-5
chi=var(10)
redchi=float(chi)/float(dof)
ENDIF

IF n_elements(var) EQ 15 THEN BEGIN
;print,'poo'
DOF=var(11)
chi=var(10)
redchi=float(chi)/float(dof)
ENDIF


ad[i]=abs(var(1))
ade[i]=(sqrt((var(6)^2)*redchi))
ENDFOR



amps_norm = ad/ad(0)
rho=reform((1/amps_norm)^4)
rho_er = reform(rho*sqrt((4*abs(ade)/abs(ad)^2+(4*abs(ade(0))/abs(ad(0))^2))))

rho=[rho[0:29],!Values.F_NAN,rho[30:60],!Values.F_NAN,rho[61:64],!Values.F_NAN,rho[65:*]]
rho_er=[rho_er[0:29],!Values.F_NAN,rho_er[30:60],!Values.F_NAN,rho_er[61:64],!Values.F_NAN,rho_er[65:*]]
print,rho_er
x=findgen(n_elements(rho))*km


!X.MARGIN=[10,3]
!Y.MARGIN=[4,2]

;cgwindow,'cgplot',x,rho,xtitle='Height along structure (km)',ytitle='$\rho$/$\rho$0',charsize=1.5 ,charthick=1,xthick=1.5,ythick=1.5,thick=1.5,xstyle=1,ystyle=1,/nodata
ycon=exp(-x/250)

p1=plot(x,ycon,thick=2,color='red',linestyle=3,/ylog)
;p2=plot(x,rho,/current,/overplot,thick=2,color='blue')
p3=errorplot(x,rho,rho_er,/current,/overplot,linestyle=6,symbol='*',/ylog)
;


END