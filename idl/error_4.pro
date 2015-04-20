;Krishna Mooroogen.
;Northumbria University, Solar group.
;02/12/14.
;Routine to perform sanity check on errors using model fitting
;noise versus intesnity between time sections

;Assume data is a data cube for now add additional handling later.
;Might be worth adding peak det to estimate width and mean.

;PURPOSE: Compare noise estimates in astronomical data with modelled noise to check for discprencies.

;INPUTS: data: 3D data cube of from x,y,t.

pro error_4,dat

time_stamps=[0,227,455,681,908,1116]


;maybe unsharp it so its equivilant to cross cut data,cut off edges
;change time stamp

dat_im=dat(5:950,15:950,time_stamps(0):time_stamps(1))


;sum data over time, 20 frames omitting first frame to avoid time boundries
sum_dat_im=float(sum(dat_im(*,*,1:21),2))/20.0

sz=size(dat_im)

;FRAME BY FRAME pixel variance
;calulating diference

s=size(dat_im)

decomp=fltarr(s(1),s(2),23)

FOR j=0,22 DO BEGIN
atrous,1.*dat_im(*,*,j),decomposition=d,n_scales=1;,/speed
decomp(*,*,j)=d(*,*,1)-unsharp(data=d(*,*,1))
ENDFOR


rms_noise=fltarr(s(1),s(2))
;take rms of each pixel in time
FOR j=0, (s(1)-2) DO BEGIN
FOR k=0, (s(2)-1) DO BEGIN
rms_noise(j,k)=rms(decomp(j,k,*))
ENDFOR
ENDFOR

bsx=5

bsy=0.01

ln_rms=alog10(rms_noise>1)
hd2 = hist_2d(sum_dat_im,ln_rms,bin1=fix(bsx),bin2=0.01)

sz=size(hd2)

;print,sz
x=0
mean_av=0
av_er=0

FOR i=0, (sz(1)-1) DO BEGIN

ind=where(hd2[i,*] ne 0,count)

IF count gt 100 THEN BEGIN

x=[temporary(x),i]

a=hd2[i,*]
b=a[*]

szb=size(b)

x2=findgen(szb(1))

y=b[where(b ne 0)]
xx=x2[where(b ne 0)]
xxb=xx*bsy

;plot,xxb,y,psym=1,ytitle='No.events',xtitle='log(RMS)';,charsize=0.7

err=sqrt(abs(y))

est=[max(y,o),1,1]

fit=gaussfit(xxb,y,coeff,sigma=error,estimates=est,nterms=3,measure_errors=err,chisq=chi)

;loadct,2,/silent
;oplot,xxb,fit,color=100
;errplot,xxb,y-err,y+err
;loadct,0,/silent
;cgtext,5,(max(y)/2),'red chi = '+strtrim(chi,1),CHARSIZE=0.9,/data

av_er=[temporary(av_er),error(1)]
mean_av=[temporary(mean_av),coeff(1)]


ENDIF ELSE BEGIN

ENDELSE


ENDFOR



xb=x[1:*]*bsx

mean_av=mean_av[1:*]
av_er=av_er[1:*]

help,mean_av

;use=where(mean_av lt 5*sqrt((moment(mean_av))[1]))
use=where((mean_av lt 100) and (mean_av gt 0) )


set_plot,'ps'

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/density_pl_atun_bin.eps'

tvim,hd2^0.3,xrange=[0,bsx*sz(1)],yrange=[0,bsy*sz(2)],xtitle='Averaged intenstiy',ytitle='log(RMS) atrous/unsharp',/rct
loadct,2,/silent
oplot,xb(use),mean_av(use),psym=1,color=100
loadct,0,/silent

device,/close

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/lin_pl_atun_bin.eps'

plot,xb(use),mean_av(use),psym=1,xtitle='Averaged intenstiy',ytitle='Mean log(RMS) atrous/unsharp';,/ylog

res=linfit(xb(use),mean_av(use),chisqr=chi,covar=cov,sigma=sig,measure_errors=av_er(use))
;res=poly_fit(xb(use),mean_av(use),2,measure_errors=av_er(use))
;start=[0.1,1]
;res=mpfitfun('func',xb(use),mean_av(use),av_er(use),start)

m=res(1)
c=res(0)
;c=res(2)


loadct,2,/silent
oplot,xb(use),(m*xb(use)+c),color=100
;errplot,xb(use),(mean_av(use)-av_er(use)),(mean_av(use)+av_er(use))
loadct,0,/silent

print,res

m=string(m,Format='(D0.5)')
;b=string(b,Format='(D0.3)')
c=string(c,Format='(D0.5)')

cgtext,3000,0.6,'y='+strtrim(m,1)+'x + '+strtrim(c,1),/data
device,/close
set_plot,'x'


END