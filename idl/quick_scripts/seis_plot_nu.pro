PRO seis_plot_nu,n,debug=debug

;increasing/decreaing amp 
;indx=[2,5,10,11,12,15,21,25,27,28];pos grd
;indx=[1,6,8,14,17,22,26];neg grd
;



;prop direction 
indxneg=[12,14,15,17,22,25,26,27];neg prop
indxpos=[1,2,5,6,8,10,11,21,28];pos prop
;

col=['blue','forest green','chocolate','pink','orange','gold','firebrick','indigo','cyan'];pos prop
Mm = (0.725/16.981891892)

xcon=findgen(40)*Mm
ycon=exp(-xcon/0.25)

g_rho_pos=fltarr(9,2)
g_rho_neg=fltarr(8,2)

;p1=plot(xcon,ycon,/over,/curr,color='black',linestyle=2,layout=[2,1,1])
;par_in = replicate({fixed:0, limited:[0,0], limits:[0.D,0.D]},2)
;par_in(0).fixed = 1

;par_in(0).limited(0) = 1
;par_in(0).limits(0) = 0.5
;par_in(0).limited(1) = 1
;par_in(0).limits(1) = 1.5

FOR i=0,n_elements(indxpos)-1 DO BEGIN

rho_seis=read_table('rho_n/seis_rho_'+strtrim(indxpos[i],1)+'.txt')
rho=rho_seis[0,*]
rho_er=rho_seis[1,*]
x=findgen(n_elements(rho))*Mm


;p=errorplot(x,rho,rho_er,xtitle='Distance (Mm)',ytitle='$\rho$/$\rho_{0}$',$

;yrange=[-0.5,4],xrange=[-0.02,0.8],layout=[2,1,1],FONT_NAME='Helvetica',/curr,/over,color=col[i],errorbar_color=col[i])

res=linfit(x,alog(rho),measure_errors=(rho_er/rho),sigma=sig,prob=p,yfit=fit)

;res = mpfitfun('myshexp',x,rho,rho_er,[1,0.06],dof=dof,perror=per,bestnorm=chi,parinfo=par_in,/quiet)


IF keyword_set(debug) THEN BEGIN
;print,chi/dof
print,i
;print,res(1)
print,(1./res(1))*1000
p=plot(x,alog(rho),thick=2,color=col[i])
p=plot(x,fit,linestyle=2,/over,/curr)
;p=plot(x,myshexp(x,res),linestyle=2,/over,/curr)
;pause
p.close
ENDIF

g_rho_pos[i,0]=(1./res(1))
g_rho_pos[i,1]= (1./res(1))*(sig(1)/res(1));((-1./(res(1)^2))*(sig(1)))

;g_rho_pos[i,0]=res(1)
;g_rho_pos[i,1]=per(1)


ENDFOR


ycon=reverse(reform(ycon/ycon[-1]))

;p2=plot(xcon,alog(ycon),/curr,color='black',linestyle=2,layout=[2,1,2],yrange=[-2,5],xrange=[-0.02,0.65],xtitle='Distance (Mm)',ytitle='ln $\rho$/$\rho_{n}$',FONT_NAME='Helvetica')

FOR i=0,n_elements(indxneg)-1 DO BEGIN

rho_seis=read_table('rho_n/seis_rho_'+strtrim(indxneg[i],1)+'.txt')

r=rho_seis[0,*]
e=rho_seis[1,*]
rho=r/r[-1]

rho_er=rho*sqrt((e/r)^2+(e[-1]/r[-1])^2)

x=findgen(n_elements(r))*Mm

;p3=errorplot(x,alog(reverse(reform(rho))),reverse(reform(rho_er/rho)),layout=[2,1,2],/curr,/over,color=col[i],errorbar_color=col[i])
;res = mpfitfun('myshexp',x,r,e,[1,0.06],dof=dof,perror=per,bestnorm=chi,parinfo=par_in,/quiet)

res=linfit(x,alog(r),sigma=sig,measure_errors=(e/r),prob=p,yfit=fit)


IF keyword_set(debug) THEN BEGIN
print,i
print,(1./res(1))*1000
p=plot(x,alog(r),thick=2,color=col[i])
p=plot(x,fit,linestyle=2,/over,/curr)
;p=plot(x,myshexp(x,res),linestyle=2,/over,/curr)
;pause
p.close
ENDIF
g_rho_neg[i,0]=(1./res(1))
g_rho_neg[i,1]=(1./res(1))*(sig(1)/res(1));((-1./(res(1)^2))*(sig(1)))

;g_rho_neg[i,0]=res(1)
;g_rho_neg[i,1]=per(1)
ENDFOR

print,g_rho_pos*1000.
print,g_rho_neg*1000.


END