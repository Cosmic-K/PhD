pro resample_chi,f,ch,probs,ref_ts_pls_noiz,fit2,err


;f='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/kinkwavemeasha/measured/13283752_2440_0/13283752_2440_0_2.idl'
;f='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/kinkwavemeasha/measured/10154058_11536_0/10154058_11536_0_0.idl'
;f='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/kinkwavemeasha/measured/10154058_60149_3/10154058_60149_3_0.idl'

restore,f
pos=threads_fit_fg.pos
pos=pos[*,1:*]
ix=where(pos ne -1)
pos=pos(ix)

Mm = (0.725/16.981891892)

pos_er=threads_fit_fg.err_pos
pos_er=pos_er[*,1:*]
pos_er=pos_er(ix)


var=threads_fit_fg.fit_result_pos
var=var[*,1:*]

aln_er=make_array(n_elements(pos_er),value=0.164214)

err=sqrt(pos_er^2+aln_er^2)

pos=pos[5:147]
err=err[5:147]


test_noise=randomn(seed,n_elements(pos),500)

mat_col=rebin(pos,n_elements(pos),500)

mat_sig_col=rebin(err,n_elements(pos),500)

noise=(test_noise*mat_sig_col)
ref_ts_pls_noiz=noise+mat_col

x=findgen(n_elements(pos))



amp=fltarr(500)
per=fltarr(500)

amp2=fltarr(500)
per2=fltarr(500)

a_err=fltarr(500)
p_err=fltarr(500)


a2_err=fltarr(500)
p2_err=fltarr(500)

;d=fltarr(500)

ch=fltarr(500)
dst=fltarr(500)
probs=fltarr(500)


Rt=fltarr(500)

;p=[var[0],var[1],var[2],var[3],var[4],5,10,50,var[3]]
p=[var[0],var[1],var[2],var[3],var[4]]


FOR j=0,499 DO BEGIN
param = mpfitfun('mysin',x, ref_ts_pls_noiz[*,j], err,p,/quiet,dof=dof,perror=perr,bestnorm=chi)
fit=mysin(x,param)


residual=(ref_ts_pls_noiz[*,j]-fit)/err


ksone,residual,'mynorm', Ds, prob,/plot,/window,/_extra, xtitle='X',ytitle='Probability',xthick=2,ythick=2,charthick=1,charsize=1.5,color='red',thick=1

print,'ds: ',strtrim(ds,1),' prob: ',strtrim(prob,1)

dst[j]=ds
probs[j]=prob

amp[j]=param[1]
per[j]=param[2]

;amp2[j]=param[6]
;per2[j]=param[7]

a_err[j]=perr[1]
p_err[j]=perr[2]

;a2_err[j]=perr[6]
;p2_err[j]=perr[7]

;d[j]=dof
ch[j]=chi


cgplot,x*1.343,ref_ts_pls_noiz[*,j]*Mm,xtitle='Time (s)',ytitle='Displacement (Mm)',yrange=[min(ref_ts_pls_noiz[*,499]*Mm),max(ref_ts_pls_noiz[*,499]*Mm)+0.2],charsize=1.5,xthick=2,ythick=2,thick=1,charthick=1
cgplot,x*1.343,fit*Mm,color='red',/overplot,thick=2,linestyle=2

;cgplot,x*1.343,residual,xtitle='Time (s)',ytitle='Residuals',charsize=1.5,xthick=2,ythick=2,thick=1,charthick=1


res_pos=where(residual GT 0)
res_neg=where(residual LT 0)

bin=fltarr(228)
bin[res_pos]=1
bin[res_neg]=0


pr=r_test(bin, R = r, N0 = n0, N1 = n1)
Rt[j]=pr[1]
;must be larger than 0.05 for random
ENDFOR




cgwindow,'cgplot',x*1.343,ref_ts_pls_noiz[*,499]*Mm,xtitle='Time (s)',ytitle='Distance (Mm)',yrange=[min(ref_ts_pls_noiz[*,499]*Mm),0.6],charsize=1.5,xthick=2,ythick=2,thick=1,charthick=1
cgerrplot,x*1.343,ref_ts_pls_noiz[*,499]*Mm+err*Mm,ref_ts_pls_noiz[*,499]*Mm-err*Mm,/addcmd
cgplot,x*1.343,fit*Mm,color='red',/overplot,/window,thick=2,linestyle=2

fit2=fit

cgwindow,'cgplot',x*1.343,residual,xtitle='Time (s)',ytitle='Residuals',charsize=1.5,xthick=2,ythick=2,thick=1,charthick=1


;constrain_amp=amp;[where(amp lt 20)]
;constrain_per=per[where((per lt 200) and (per gt 0))]


cghistoplot,ch,histdata=hd,locations=loc
fit=gaussfit(loc+20,hd,coeff,sigma=errors,chisq=chi,nterm=3)

cgwindow
cghistoplot,ch,/window,thick=1,/fill,polycolor='Cornflower Blue',datacolorname='black',xstyle=4,ystyle=4
cgAxis, YAxis=0,title='Density',/window,ythick=2,charthick=1,charsize=1.5
cgAxis, XAxis=0,title='Chi^2',/window,xthick=2,charthick=1,charsize=1.5
cgAxis, YAxis=1,/window,ythick=2,YTICKFORMAT="(A1)"
cgAxis, XAxis=1,/window,xthick=2,XTICKFORMAT="(A1)"

cgplot,loc+20,fit,/window,/overplot
vline,coeff(1),color='red',linestyle=2,thick=3,/win

save,dst,filename='ks_stat.idl'
save,probs,filename='signifprob.idl'

;cghistoplot,Rt,histdata=hd,locations=loc
;print,Rt[499]
;print,r

END