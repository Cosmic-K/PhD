;Krishna Mooroogen.
;Northumbria University, Solar group.
;krishna.mooroogen@northumbria.ac.uk
;PhD supervisor Richard Morton


;NOTES
;-------------------------------------------------------------------------------------------------
;Assume data is a data cube for now add additional handling later.
;Might be worth adding peak det to estimate width and mean.

;PURPOSE:
;------------------------------------------------------------------------------------------------
;Compare noise estimates in astronomical data with modelled noise to check for discprencies.
;Routine to perform sanity check on errors using model fitting
;noise versus intesnity between time sections

;INPUTS:
;-----------------------------------------------------------------------------------------------
;data: 3D data cube of from x,y,t.

;OUTPUTS
;-----------------------------------------------------------------------------------------------
;Plots error coorelations and funciton of relationship

pro error_test_ca,dat,hd2,xxb,xb,use,mean_av,av_er,fit,fit2,y,err,bsx,bsy,rms_noise,c,sum_dat_im


;CONSTANTS
;-----------------------------------------------------------------------------------------------

time_stamps=[0,229,460,689,920,1149]
;change time stamp

;CROPPING
;---------------------------------------------------------------------------
dat_im=dat(10:955,25:985,*)

;AVERAGING
;---------------------------------------------------------------------------
;sum data over time, 40 frames omitting first frame to avoid time boundries
sum_dat_im=float(sum(dat_im(*,*,1:40),2))/41.0


;ESTIMATING NOISE
;-------------------------------------------------------------------------
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
;FOR j=0, (s(1)-2) DO BEGIN
;FOR k=0, (s(2)-1) DO BEGIN
rms_noise=rms(decomp,dimension=3)
;ENDFOR
;ENDFOR

;5
;0.01

bsx=5
bsy=0.4

;JOINT PROB DIST OF DATA AND NOISE
;-----------------------------------------------------------------------------

ln_rms = float(rms_noise)


hd2 = hist_2d(sum_dat_im,ln_rms,bin1=bsx,bin2=bsy,max2=80,min1=1200,max1=13000)

sz=size(hd2)

x=0
mean_av=0
av_er=0


;tvim,hd2^0.7,xrange=[0,bsx*sz(1)],yrange=[0,bsy*sz(2)],xtitle='Averaged intensity',ytitle='RMS atrous/unsharp',/rct
;help,hd2
;pause

;EXTARCTING DATA FROM 2D HISTOGRAM AND FITTING LOG NORMAL TO DISTRIBUTION BINS
;--------------------------------------------------------------------------------
;
;
;1700
FOR i=long(800),1700 DO BEGIN

ind=where(hd2[i,*] ne 0,count)

IF count gt 100 THEN BEGIN
print,i

x=[temporary(x),i]

b=reform(hd2[i,*])

szb=size(b)

x2=findgen(szb(1))

y=b[where(b ne 0)]
xx=x2[where(b ne 0)]
xxb=xx*bsy

err=sqrt(abs(y))
;set_plot,'ps'
;device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/gfit.eps'

;!X.MARGIN=[20,3]
;!Y.MARGIN=[7,2]

plot,alog(xxb[1:*]),y[1:*],ytitle='No.events',xtitle='log(RMS noise)',xthick=3,ythick=3,thick=3,charthick=4,charsize=1.5

est=[max(y,o),alog(xxb(o)),1]

fit=gaussfit(alog(xxb[1:*]),y[1:*],coeff,sigma=error,estimates=est,nterms=3,measure_errors=err[1:*],chisq=chi)

;res=mpfitfun('logno',(xxb+1),y,err,est,perror=error,yfit=fit,/quiet)


loadct,2,/silent
oplot,alog(xxb[1:*]),fit,color=100,thick=2
errplot,alog(xxb[1:*]),y[1:*]-err[1:*],y[1:*]+err[1:*],thick=2
mu = coeff(1);exp(coeff(1)+(0.5*(coeff(2)^2)))
er_mu = error(1);sqrt(((exp(coeff(1)^2))*(error(1)^2))+((coeff(2)*exp(0.5*coeff(2)^2))^2*(error(2)^2)))
;sigmasq = (exp(coeff(2)^2)-1)*exp(2*coeff(1)+coeff(2)^2)
vline,coeff(1),color=100,linestyle=1,thick=4
loadct,0,/silent

;device,/close
;set_plot,'x'

;cgtext,5,(max(y)/2),'red chi = '+strtrim(chi,1),CHARSIZE=0.9,/data

av_er=[temporary(av_er),er_mu]
mean_av=[temporary(mean_av),mu]

ENDIF ELSE BEGIN

ENDELSE

ENDFOR

;device,/close

xb=x[1:*]*bsx

mean_av=mean_av[1:*]
av_er=av_er[1:*]

;FILTERING BAD FITS
;------------------------------------------------------------------------------------------------------
;use=where(mean_av lt 5*sqrt((moment(mean_av))[1]))
use=where((mean_av lt 100) and (mean_av gt 0) )


;PLOTTING MEANS OF DISTRIBUTION AND FITTING FUCNTION
;------------------------------------------------------------------------------------------------------
;set_plot,'ps'

;device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/logrm_ca_refit.eps'

!X.MARGIN=[15,3]
!Y.MARGIN=[4,2]

plot,xb[0:*:3],mean_av[0:*:3],psym=1,xtitle='Averaged intensity',ytitle='log(Mean RMS noise)',xthick=3,ythick=3,thick=2.5,charthick=4,charsize=0.8,yrange=[1.7,3]

p=[3.5e-8,-0.00034,3]

;res=mpfitfun('lin_func_fit2',xb(use),mean_av(use),av_er(use),p,/quiet,dof=dof,perror=perr,bestnorm=chi)

res=poly_fit(xb(use),mean_av(use),2,measure_errors=av_er(use),yfit=fit2,chisq=chi)

;fit=lin_func_fit2(xb(use),res)

a=res(0)
b=res(1)
c=res(2)

;d=res(3)
c=coeff(1)
loadct,2,/silent
oplot,xb(use),fit2,color=100,thick=2.5
errplot,xb[0:*:3],(mean_av[0:*:3]-av_er[0:*:3]),(mean_av[0:*:3]+av_er[0:*:3]),thick=2
loadct,0,/silent

print,res
;print,float(chi)/(n_elements(xb(use))-3-1)

;a=string(a,Format='(3F0)')
;b=string(b,Format='(3F0)')
;c=string(c,Format='(3F0)')
;d=string(c,Format='(3F0)')

;cgtext,5100,1.3,'y='+strtrim(a,1)+'x^2'+strtrim(b,a)+'x+'+strtrim(c,1)+'x^0.5 +'+strtrim(d,1),/data,charsize=0.7
;device,/close

;print,chi

;set_plot,'x'
;plot,xb(use),(mean_av(use)-fit)

;residual=(mean_av(use)-fit)
;res_pos=where(residual GT 0)
;res_neg=where(residual LT 0)
;bin=fltarr(n_elements(residual))
;bin[res_pos]=1
;bin[res_neg]=0


;pr=r_test(bin, R = r, N0 = n0, N1 = n1)

;print,pr[1]
;set_plot,'ps'


;device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/density_ca3.eps'

;!X.MARGIN=[14,7]
;!Y.MARGIN=[9,2]


;tvim,hd2^0.7,xrange=[0,bsx*sz(1)],yrange=[0,bsy*sz(2)],/rct,pcharsize=0.5,/noaxis,aspect=1
;axis,0,xaxis=0,charsize=0.8,xrange=[0,bsx*sz(1)],xtitle='Averaged intenstiy',xthick=3,charthick=4
;axis,0,yaxis=0,charsize=0.8,yrange=[0,bsy*sz(2)],ytitle='RMS noise',ythick=3,charthick=4
;axis,xaxis=1,xthick=3,XTICKFORMAT="(A1)"
;axis,yaxis=1,ythick=3,YTICKFORMAT="(A1)"
;loadct,2,/silent
;oplot,xb(use),mean_av(use),linestyle=2,color=100,thick=4
;loadct,0,/silent

;device,/close

;set_plot,'x'

END