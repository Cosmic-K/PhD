pro seis_plot2,n


Mm = (0.725/16.981891892)

set_plot,'ps'


FOR i=1,28 DO BEGIN


!X.MARGIN=[15,3]
!Y.MARGIN=[4,2]

r_seis=read_table('r/seis_r_'+strtrim(i,1)+'.txt')

r=r_seis[0,*]
r_er=r_seis[1,*]

x=findgen(n_elements(r))*Mm

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/images/seis/r/r_seis'+strtrim(i,1)+'.ps'


plot,x,r,xtitle='Height along structure (Mm)',ytitle='R/R_0',charsize=1 ,charthick=1,xthick=1.5,ythick=1.5,thick=1.5
errplot,x,r+r_er,r-r_er,/addcmd,thick=0.01,width=0.005

device,/close

ENDFOR


FOR i=1,28 DO BEGIN


!X.MARGIN=[15,3]
!Y.MARGIN=[4,2]


b_seis=read_table('b/seis_b_'+strtrim(i,1)+'.txt')

b=b_seis[0,*]
b_er=b_seis[1,*]

x=findgen(n_elements(b))*Mm

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/images/seis/b/b_seis'+strtrim(i,1)+'.ps'


plot,x,b,xtitle='Height along structure (Mm)',ytitle='B/B_0',charsize=1 ,charthick=1,xthick=1.5,ythick=1.5,thick=1.5
errplot,x,b+b_er,b-b_er,/addcmd,thick=0.01,width=0.005

device,/close

ENDFOR

FOR i=1,28 DO BEGIN


!X.MARGIN=[15,3]
!Y.MARGIN=[4,2]


rho_seis=read_table('rho/seis_rho_'+strtrim(i,1)+'.txt')

rho=rho_seis[0,*]
rho_er=rho_seis[1,*]

x=findgen(n_elements(rho))*Mm

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/images/seis/rho/rho_seis'+strtrim(i,1)+'.ps'


plot,x,rho,xtitle='Height along structure (Mm)',ytitle='Rho/Rho_0',charsize=1 ,charthick=1,xthick=1.5,ythick=1.5,thick=1.5
errplot,x,rho+rho_er,rho-rho_er,/addcmd,thick=0.01,width=0.005

device,/close

ENDFOR
set_plot,'x'

END