;Krishna Mooroogen.
;Northumbria University, Solar group.
;02/12/14.
;Routine to perform sanity check on errors using model fitting
;noise versus intesnity between time sections

;Assume data is a data cube for now add additional handling later.
;Might be worth adding peak det to estimate width and mean.

;PURPOSE: Compare noise estimates in astronomical data with modelled noise to check for discprencies.

;INPUTS: data: 3D data cube of from x,y,t.

pro error_1,dat

time_stamps=[0,227,455,681,908,1116]

;maybe unsharp it so its equivilant to cross cut data,cut off edges
;change time stamp
dat_im=dat(5:950,15:950,time_stamps(0):time_stamps(0+1))

;sum data over time, 20 frames omitting first frame to avoid time boundries
sum_dat_im=float(sum(dat_im(*,*,1:21),2))/20.0

sz=size(dat_im)

;FRAME BY FRAME
;calulating diference
total_frame=fltarr(sz(1),sz(2),20)

FOR j=1, 20 DO BEGIN
total_frame(*,*,(j-1))=dat_im(*,*,j)-dat_im(*,*,(j+1))
ENDFOR

rms_noise=fltarr(sz(1),sz(2))
;take rms of each pixel in time
FOR j=0, (sz(1)-1) DO BEGIN
FOR k=0, (sz(2)-1) DO BEGIN
rms_noise(j,k)=rms(total_frame(j,k,*))
ENDFOR
ENDFOR


bsx=50

bsy=0.02

ln_rms=alog10(rms_noise>1)
hd2 = hist_2d(sum_dat_im,rms_noise,bin1=fix(bsx),bin2=0.01)

sz=size(hd2)

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

est=[max(y,o),1.5,1]

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


help, mean_av

xb=x[1:*]*bsx
mean_av=mean_av[1:*]
av_er=av_er[1:*]

;use=where(mean_av lt 5*sqrt((moment(mean_av))[1]))
use=where((mean_av lt 1000) and (mean_av gt 0) )


set_plot,'ps'

;device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/density_velpl_f2f.eps'

tvim,hd2^0.3,xrange=[0,bsx*sz(1)],yrange=[0,bsy*sz(2)],xtitle='Averaged intenstiy',ytitle='log(RMS) frame to frame',/rct
;loadct,2,/silent
;oplot,xb(use),mean_av(use),psym=1,color=80
;loadct,0,/silent

;device,/close

;device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/lin_velpl_f2f.eps'

;plot,xb(use),mean_av(use),psym=1,xtitle='Averaged intenstiy',ytitle='Mean log(RMS) frame to frame';,/ylog
;res=linfit(xb(use),mean_av(use),chisqr=chi,covar=cov,sigma=sig,measure_errors=av_er(use))
;c=res(0)
;m=res(1)
;loadct,2,/silent
;oplot,xb(use),(m*xb(use)+c),color=200
;errplot,xb(use),(mean_av(use)-av_er(use)),(mean_av(use)+av_er(use))
;loadct,0,/silent
;m=string(m,Format='(D0.5)')
;c=string(c,Format='(D0.3)')

;cgtext,3000,1,'y='+strtrim(m,1)+'x+'+strtrim(c,1),/data
;device,/close
;set_plot,'x'

END