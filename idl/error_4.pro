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

time_stamps=[0,229,460,689,920,1149]


;maybe unsharp it so its equivilant to cross cut data,cut off edges
;change time stamp

dat_im=dat(0:969,10:990,time_stamps(0):time_stamps(1))
;dat_im=dat(5:954,15:950,time_stamps(0):time_stamps(1))

;sum data over time, 40 frames omitting first frame to avoid time boundries
sum_dat_im=float(sum(dat_im(*,*,1:40),2))/41.0

sz=size(dat_im)

;atrous/unsharp pixel variance
;calulating diference

s=size(dat_im)

decomp=fltarr(s(1),s(2),41)

FOR j=0,40 DO BEGIN
atrous,1.*dat_im(*,*,j),decomposition=d,n_scales=1
decomp(*,*,j)=unsharp(data=d(*,*,1))
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


ln_rms = rms_noise;alog10(rms_noise >1)
hd2 = hist_2d(sum_dat_im,ln_rms,bin1=bsx,bin2=bsy,max2=10)

sz=size(hd2)

;print,sz
x=0
mean_av=0
av_er=0

;tvim,hd2^0.4,xrange=[0,bsx*sz(1)],yrange=[0,bsy*sz(2)],xtitle='Averaged intensity',ytitle='RMS atrous/unsharp',/rct


FOR i=long(0),400 DO BEGIN;

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

err=sqrt(abs(y))

;plot,(xxb+1),y,psym=1,ytitle='No.events',xtitle='log(RMS)'
est=[max(y),1.8,1]


;fit=gaussfit(xxb,y,coeff,sigma=error,estimates=est,nterms=3,measure_errors=err,chisq=chi)
res=mpfitfun('logno',(xxb+1),y,err,est,perror=error,yfit=fit,/quiet)
;print,res(1)

;loadct,2,/silent
;oplot,(xxb+1),fit,color=100
;errplot,(xxb+1),y-err,y+err
;loadct,0,/silent

;cgtext,5,(max(y)/2),'red chi = '+strtrim(chi,1),CHARSIZE=0.9,/data

av_er=[temporary(av_er),error(1)]
mean_av=[temporary(mean_av),res(1)]


ENDIF ELSE BEGIN

ENDELSE

ENDFOR

help,mean_av

xb=x[1:*]*bsx

mean_av=mean_av[1:*]
av_er=av_er[1:*]

;use=where(mean_av lt 5*sqrt((moment(mean_av))[1]))
use=where((mean_av lt 100) and (mean_av gt 0) )


set_plot,'ps'
device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/density_pl_atun.eps'

tvim,hd2^0.4,xrange=[0,bsx*sz(1)],yrange=[0,bsy*sz(2)],xtitle='Averaged intenstiy',ytitle='RMS atrous/unsharp',/rct,pcharsize=0.5
loadct,2,/silent
oplot,xb(use),mean_av(use),psym=1,color=100
loadct,0,/silent

device,/close

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/lin_pl_atun.eps'

plot,xb(use),mean_av(use),psym=1,xtitle='Averaged intensity',ytitle='Mean RMS atrous/unsharp';,/ylog

;res=linfit(xb(use),mean_av(use),chisqr=chi,covar=cov,sigma=sig,measure_errors=av_er(use))
res=poly_fit(xb(use),mean_av(use),2,measure_errors=av_er(use),yfit=fit,chisq=chi)

c=res(0)
b=res(1)
a=res(2)

loadct,2,/silent
oplot,xb(use),fit,color=100
errplot,xb(use),(mean_av(use)-av_er(use)),(mean_av(use)+av_er(use))
loadct,0,/silent

;print,res
;print,float(chi)/(n_elements(xb(use))-3-1)

m=string(a,Format='(D0.3)')
b=string(b,Format='(D0.3)')
c=string(c,Format='(D0.3)')

cgtext,1200,0.4,'y='+strtrim(a,1)+'x^2+'+strtrim(b,a)+'x+'+strtrim(c,1),/data
device,/close
set_plot,'x'

END