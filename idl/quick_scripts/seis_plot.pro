pro seis_plot,n


indx=[1,2,5,6,8,9,10,11,12,14,15,17,21,22,25,26,27,28]

;18,3,20

Mm = (0.725/16.981891892)
r_seis=read_table('r_n/seis_r_1.txt')

r=r_seis[0,*]
r_er=r_seis[1,*]

x=findgen(n_elements(r))*Mm


!X.MARGIN=[10,3]
!Y.MARGIN=[4,2]


cgwindow,'cgplot',x,r,xtitle='Height along structure (Mm)',ytitle='R/R0',charsize=1.5 ,charthick=1,xthick=1.7,ythick=1.7,thick=1.5,yrange=[0.5,3.5],xrange=[-0.05,0.45],xstyle=1,/nodata



FOR i=0,n_elements(indx)-1 DO BEGIN
loadct,13,/silent

r_seis=read_table('r_n/seis_r_'+strtrim(indx[i],1)+'.txt')
r=r_seis[0,*]
r_er=r_seis[1,*]
x=findgen(n_elements(r))*Mm

cgplot,x,r,/window,/overplot,thick=2,color=i*14
cgerrplot,x,r+r_er,r-r_er,/addcmd,thick=1,width=0.005,color=i*14


ENDFOR
;print,'B'
b_seis=read_table('b_n/seis_b_1.txt')

b=b_seis[0,*]
b_er=b_seis[1,*]

x=findgen(n_elements(b))*Mm


!X.MARGIN=[10,3]
!Y.MARGIN=[4,2]


cgwindow,'cgplot',x,b,xtitle='Height along structure (Mm)',ytitle='B/B0',charsize=1.5 ,charthick=1,thick=2,yrange=[-0.2,4],xrange=[-0.02,0.45],/nodata,xstyle=1,xthick=1.7,ythick=1.7


FOR i=0,n_elements(indx)-1 DO BEGIN
loadct,13,/silent

b_seis=read_table('b_n/seis_b_'+strtrim(indx[i],1)+'.txt')

b=b_seis[0,*]
b_er=b_seis[1,*]

x=findgen(n_elements(b))*Mm

cgplot,x,b,/window,/overplot,thick=2,color=i*14
cgerrplot,x,b+b_er,b-b_er,/addcmd,thick=1,width=0.005,color=i*14



ENDFOR


rho_seis=read_table('rho_n/seis_rho_1.txt')

rho=rho_seis[0,*]
rho_er=rho_seis[1,*]

x=findgen(n_elements(rho))*Mm

;print,'rho'
!X.MARGIN=[10,3]
!Y.MARGIN=[4,2]


cgwindow,'cgplot',x,rho,xtitle='Height along structure (Mm)',ytitle='$\rho$/$\rho$0',charsize=1.5 ,charthick=1,xthick=1.5,ythick=1.5,thick=1.5,yrange=[-1,5],xrange=[-0.02,0.45],xstyle=1,/nodata


FOR i=0,n_elements(indx)-1 DO BEGIN
loadct,13,/silent

rho_seis=read_table('rho_n/seis_rho_'+strtrim(indx[i],1)+'.txt')

rho=rho_seis[0,*]
rho_er=rho_seis[1,*]

cgplot,x,rho,/window,/overplot,thick=2,color=i*14
cgerrplot,x,rho+rho_er,rho-rho_er,/addcmd,thick=1,width=0.005,color=i*14

ENDFOR



END