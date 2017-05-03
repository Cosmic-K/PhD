pro seis_plot,n


;indx=[1,2,5,6,8,9,10,11,12,14,15,17,18,20,21,22,25,26,27,28]
;3
indx=[2,5,10,11,12,15,21,25,27,28];pos grd
;1
;indx=[1,6,8,14,17,22,26];neg grd
;col=['blue','green','orange','dark goldenrod','sky blue','pink','purple']
col=['blue','green','orange','dark goldenrod','sky blue','pink','purple','forest green','rosy brown','charcoal','indian red']

Mm = (0.725/16.981891892)
r_seis=read_table('r_n/seis_r_1.txt')

r=r_seis[0,*]
r_er=r_seis[1,*]

x=findgen(n_elements(r))*Mm


!X.MARGIN=[10,3]
!Y.MARGIN=[4,2]


cgwindow,'cgplot',x,r,xtitle='Height along structure (Mm)',ytitle='R/R0',charsize=1.5 ,charthick=1,xthick=1.7,ythick=1.7,thick=1.5,yrange=[0.7,3.2],xrange=[-0.01,0.4],xstyle=1,ystyle=1,/nodata
;cgerrplot,x,r+r_er,r-r_er,/addcmd,thick=0.01,width=0.005


FOR i=0,n_elements(indx)-1 DO BEGIN
loadct,13,/silent

r_seis=read_table('r_n/seis_r_'+strtrim(indx[i],1)+'.txt')
r=r_seis[0,*]
r_er=r_seis[1,*]


m=moment(r)

;IF mean(r) gt 7*m[1] THEN BEGIN

;print,indx[i]
cgplot,x,r,/window,/overplot,thick=2,color=col[i]
cgerrplot,x,r+r_er,r-r_er,/addcmd,thick=1,width=0.005,color=col[i]

;ENDIF

ENDFOR

;print,'B'
b_seis=read_table('b_n/seis_b_1.txt')

b=b_seis[0,*]
b_er=b_seis[1,*]

x=findgen(n_elements(b))*Mm


!X.MARGIN=[10,3]
!Y.MARGIN=[4,2]


cgwindow,'cgplot',x,b,xtitle='Height along structure (Mm)',ytitle='B/B0',charsize=1.5 ,charthick=1,thick=2,yrange=[0,2],xrange=[-0.02,0.4],/nodata,xstyle=1,xthick=1.7,ythick=1.7
;cgerrplot,x,b+b_er,b-b_er,/addcmd,thick=0.01,width=0.005


FOR i=0,n_elements(indx)-1 DO BEGIN
loadct,13,/silent

b_seis=read_table('b_n/seis_b_'+strtrim(indx[i],1)+'.txt')

b=b_seis[0,*]
b_er=b_seis[1,*]

m=moment(b)
;IF mean(b) gt 5*m[1] THEN BEGIN
;print,indx[i]
cgplot,x,b,/window,/overplot,thick=2,color=col[i]
cgerrplot,x,b+b_er,b-b_er,/addcmd,thick=1,width=0.005,color=col[i]

;ENDIF

ENDFOR


rho_seis=read_table('rho_n/seis_rho_1.txt')

rho=rho_seis[0,*]
rho_er=rho_seis[1,*]

x=findgen(n_elements(rho))*Mm

;print,'rho'
!X.MARGIN=[10,3]
!Y.MARGIN=[4,2]


cgwindow,'cgplot',x,rho,xtitle='Height along structure (Mm)',ytitle='$\rho$/$\rho$0',charsize=1.5 ,charthick=1,xthick=1.5,ythick=1.5,thick=1.5,yrange=[-0.5,3],xrange=[-0.02,0.4],xstyle=1,ystyle=1,/nodata
;cgerrplot,x,rho+rho_er,rho-rho_er,/addcmd,thick=0.01,width=0.005
xcon=findgen(40)*Mm
ycon=exp(-xcon/0.25)

FOR i=0,n_elements(indx)-1 DO BEGIN
loadct,13,/silent

rho_seis=read_table('rho_n/seis_rho_'+strtrim(indx[i],1)+'.txt')

rho=rho_seis[0,*]
rho_er=rho_seis[1,*]


;IF mean(rho) gt 5*m[1] THEN BEGIN
;print,indx[i]
cgplot,xcon,ycon,/window,/overplot,thick=2,color='red',linestyle=3
cgplot,x,rho,/window,/overplot,thick=2,color=col[i]
cgerrplot,x,rho+rho_er,rho-rho_er,/addcmd,thick=1,width=0.005,color=col[i]
;ENDIF
ENDFOR



END