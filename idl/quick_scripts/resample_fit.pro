pro resample_fit,f

;restore,'/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/kinkwavemeasha/measured/13283752_2440_0/13283752_2440_0_5.idl'

restore,f
pos=threads_fit_fg.pos
pos=pos[*,1:*]
pos=pos[where(pos ne -1)]

pos_er=threads_fit_fg.err_pos
pos_er=pos_er[*,1:*]
pos_er=pos_er[where(pos ne -1)]

var=threads_fit_fg.fit_result_pos
var=var[*,1:*]

test_noise=randomn(seed,n_elements(pos),500)

mat_col=rebin(pos,n_elements(pos),500)

mat_sig_col=rebin(pos_er,n_elements(pos),500)

noise=(test_noise*mat_sig_col)
ref_ts_pls_noiz=noise+mat_col

x=findgen(n_elements(pos))


amp=fltarr(500)
per=fltarr(500)
a_err=fltarr(500)
p_err=fltarr(500)
d=fltarr(500)
ch=fltarr(500)

FOR j=0,499 DO BEGIN
param = mpfitfun('mysin',x, ref_ts_pls_noiz[*,j], pos_er,var[0:4],/quiet,dof=dof,perror=perr,bestnorm=chi)

amp[j]=param[1]
per[j]=param[2]
a_err[j]=perr[1]
p_err[j]=perr[2]
d[j]=dof
ch[j]=chi

ENDFOR


constrain_amp=amp;[where(amp lt 20)]
constrain_per=per[where((per lt 200) and (per gt 0))]


plothist,constrain_amp,xhist,yhist,/autobin,/noplot
est=[max(yhist),mean(amp),0.01]
fit=gaussfit(xhist,yhist,coeff,estimates=est,sigma=errors,chisq=chi,nterm=3)
cghistoplot,constrain_amp,/window,xtitle='Amplitudes'
cgplot,xhist,fit,/window,/overplot

print,coeff
print,total(a_err,1)/500.0
print,total(sqrt(a_err^2*(ch/d)),1)/500.0

;plothist,constrain_per,xhist,yhist,/autobin,/noplot
;est=[max(yhist),median(amp),0.1]
;fit=gaussfit(xhist,yhist,coeff,estimates=est,sigma=errors,chisq=chi,nterm=3)
;cghistoplot,constrain_per,/window,xtitle='Period'
;cgplot,xhist,fit,/window,/overplot

;print,coeff
;print,p_err/500
;print,sqrt(p_er^2*(ch/d))/500


END