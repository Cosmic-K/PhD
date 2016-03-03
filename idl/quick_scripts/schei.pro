pro schei,n,grad_r,grad_b,grad_rho

indx=[1,2,5,6,8,9,10,11,12,14,15,17,21,22,25,26,27,28]


grad_r=fltarr(18,2)
grad_b=fltarr(18,2)
grad_rho=fltarr(18,2)

Mm = (0.725/16.981891892)
r_seis=read_table('r_n/seis_r_1.txt')

r=r_seis[0,*]
r_er=r_seis[1,*]

x=findgen(n_elements(r))*Mm


!X.MARGIN=[10,3]
!Y.MARGIN=[4,2]


cgwindow,'cgplot',x,alog(r),xtitle='Height along structure (Mm)',ytitle='ln(R/R0)',charsize=1.5 ,charthick=1,xthick=1.7,ythick=1.7,thick=1.5,yrange=[-0.6,1.2],xrange=[-0.05,0.45],xstyle=1,/nodata



FOR i=0,n_elements(indx)-1 DO BEGIN


loadct,13,/silent

r_seis=read_table('r_n/seis_r_'+strtrim(indx[i],1)+'.txt')
r=reform(r_seis[0,*])
r_er=reform(r_seis[1,*])
x=findgen(n_elements(r))*Mm

res=linfit(x,alog(r),measure_errors=alog(r_er),sigma=sig,prob=p)
p=[res(0),res(1)]
param = mpfitfun('lin_func_fit',x,alog(r), alog(r_er),p,/quiet,dof=dof,perror=perr,bestnorm=chi)
fit=lin_func_fit(x,param)

redchi=chi/dof
print,'red chi R ',strtrim(redchi,1)

res=linfit(x,alog(r),measure_errors=alog(r_er),sigma=sig,prob=p,yfit=fit)
IF p GT 0.1 THEN BEGIN
cgplot,x,fit,/overplot,/window,linestyle=2,color=i*14
cgplot,x,alog(r),/window,/overplot,thick=2,color=i*14
cgerrplot,x,alog(r+r_er),alog(r-r_er),/addcmd,thick=1,width=0.005,color=i*14


grad_r[i,0]=1./res(1)
grad_r[i,1]=1./sig(1)

grad_b[i,0]=(1./res(1))/(2.)
grad_b[i,1]=(1./sig(1))/(2.)

grad_rho[i,0]=(1./res(1))/(4.)
grad_rho[i,1]=(1./sig(1))/(4.)

endif
ENDFOR

;print,'B'
;b_seis=read_table('b_n/seis_b_1.txt')

;b=b_seis[0,*]
;b_er=b_seis[1,*]

;x=findgen(n_elements(b))*Mm


;!X.MARGIN=[10,3]
;!Y.MARGIN=[4,2]


;cgwindow,'cgplot',x,alog(b),xtitle='Height along structure (Mm)',ytitle='ln(B/B0)',charsize=1.5 ,charthick=1,thick=2,yrange=[-3,2],xrange=[-0.02,0.45],/nodata,xstyle=1,xthick=1.7,ythick=1.7



;FOR i=0,n_elements(indx)-1 DO BEGIN
;loadct,13,/silent

;b_seis=read_table('b_n/seis_b_'+strtrim(indx[i],1)+'.txt')

;b=reform(b_seis[0,*])
;b_er=reform(b_seis[1,*])

;x=findgen(n_elements(b))*Mm

;res=linfit(x,alog(b),measure_errors=alog(b_er),sigma=sig,prob=p)
;p=[res(0),res(1)]
;param = mpfitfun('lin_func_fit',x,alog(b), alog(b_er),p,/quiet,dof=dof,perror=perr,bestnorm=chi)
;fit=lin_func_fit(x,param)

;redchi=chi/dof
;print,'red chi B ',strtrim(redchi,1)

;IF p GT 0.1 THEN BEGIN
;cgplot,x,fit,/overplot,/window,linestyle=2,color=i*14
;cgplot,x,alog(b),/window,/overplot,thick=2,color=i*14
;cgerrplot,x,alog(b+b_er),alog(b-b_er),/addcmd,thick=1,width=0.005,color=i*14
;ENDIF

;grad_b[i,0]=res(1)
;grad_b[i,1]=sig(1)

;ENDFOR


;rho_seis=read_table('rho_n/seis_rho_1.txt')

;rho=rho_seis[0,*]
;rho_er=rho_seis[1,*]

;x=findgen(n_elements(rho))*Mm


;!X.MARGIN=[10,3]
;!Y.MARGIN=[4,2]


;cgwindow,'cgplot',x,alog(rho),xtitle='Height along structure (Mm)',ytitle='ln($\rho$/$\rho$0)',charsize=1.5 ,charthick=1,xthick=1.5,ythick=1.5,thick=1.5,yrange=[-5,3],xrange=[-0.02,0.45],xstyle=1,/nodata



;FOR i=0,n_elements(indx)-1 DO BEGIN
;loadct,13,/silent

;rho_seis=read_table('rho_n/seis_rho_'+strtrim(indx[i],1)+'.txt')

;rho=reform(rho_seis[0,*])
;rho_er=reform(rho_seis[1,*])

;x=findgen(n_elements(rho))*Mm

;res=linfit(x,alog(rho),measure_errors=alog(rho_er),sigma=sig,prob=p)
;p=[res(0),res(1)]
;param = mpfitfun('lin_func_fit',x,alog(rho), alog(rho_er),p,/quiet,dof=dof,perror=perr,bestnorm=chi)
;fit=lin_func_fit(x,param)

;redchi=chi/dof
;print,'red chi Rho ',strtrim(redchi,1)

;IF p GT 0.1 THEN BEGIN
;cgplot,x,fit,/overplot,/window,linestyle=2,color=i*14
;cgplot,x,alog(rho),/window,/overplot,thick=2,color=i*14
;cgerrplot,x,alog(rho+rho_er),alog(rho-rho_er),/addcmd,thick=1,width=0.005,color=i*14
;ENDIF

;grad_rho[i,0]=res(1)
;grad_rho[i,1]=sig(1)

;ENDFOR



END