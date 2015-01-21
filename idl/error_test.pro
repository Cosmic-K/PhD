;Krishna Mooroogen.
;Northumbria University, Solar group.
;02/12/14.
;Routine to perform sanity check on errors.


;Assume data is a data cube for now add additional handling later.
;Might be worth adding peak det to estimate width and mean.
;Add plot option to switch between plots?
;multiplots,overploting histograms


;CALL: error_test,data=data,darks=darks,flats=flats

;PURPOSE: Compare noise estimates in astronomical data with modelled noise to check for discprencies.

;INPUTS: data: 3D data cube of from x,y,t.
;        darks: 2D array dark frame data.
;        flat: 2D array flat frame data.


pro error_test, data=data,darks=darks,flat=flat


;Darks and flats cropped to match the cropped data,

;darks=darks(40:1003,42:1003)
;flat=flat(40:1003,42:1003)

;data=(data*23.0)

sz=size(data)


;METHOD 1 OF NOISE EST

;POISSON NOISE

poisson_noise = fltarr(sz(1),sz(2),10)


;POSSION NOISE OVER TEN FRAMES FOR TIME VARIENCE

FOR i=0,9 DO BEGIN

poisson_noise(*,*,i) = poidev(data(*,*,i))-data(*,*,i)

ENDFOR



;SIGMA ESTIMATE FOR DARKS

plothist, darks,xhist,yhist,/AUTOBIN,/NOPLOT

est=[max(yhist,I),I,5]

fit = gaussfit(xhist,yhist,coeff,sigma=error, estimates=est,nterms=3)

sigma_dark=coeff(2)


a=string(coeff(2))
a=strtrim(a,1)
b=string(error(2))
b=strtrim(b,1)

print,'METHOD 1.'
print,'MODELLED NOISE FROM FLAT, DARKS AND POIDEV COMPARED WITH FRAME BY FRAME SUBTRACTION.'

print,'Sigma_dark:'+' '+a+' '+'+/-'+' '+b



;SIGMA ESTIMATE FOR FLAT

plothist, flat,xhist,yhist,/AUTOBIN,/NOPLOT

est=[max(yhist,I),150,50]

fit = gaussfit(xhist,yhist,coeff,sigma=error, estimates=est,nterms=3)

sigma_flat=coeff(2)

a=string(coeff(2))
a=strtrim(a,1)
b=string(error(2))
b=strtrim(b,1)

print, 'Sigma flat:'+' '+a+' '+'+/-'+' '+b




;ADDING DARK AND FLAT IN QUADRATURE THEN ADDING GAUSSIAN NOISE TO POSSION NOISE

sigma_tot = sqrt((sigma_dark^2)+(sigma_flat^2))


print, 'Sigma total:', sigma_tot


;NOISE OVER TEN FRAMES

gauss_noisefd = fltarr(sz(1),sz(2),10)

FOR i=0,9 DO BEGIN

gauss_noisefd(*,*,i)=sigma_tot*randomn(seed, sz(1),sz(2))

ENDFOR

tot_noise_av = poisson_noise+gauss_noisefd

plothist, tot_noise_av,xhist,yhist,/fill,fcolor='blue',color='blue',axiscolor='black',xtitle='Total data noise over 10 frames.',ytitle='Frequency.',/autobin,charsize=1,/noplot

est=[max(yhist),0,200]

fit = gaussfit(xhist,yhist,coeff,sigma=error, estimates=est,nterms=3)

;loadct,2

;oplot,xhist,fit,color='100',thick=7,linestyle=1

a=string(coeff(2))
a=strtrim(a,1)
b=string(error(2))
b=strtrim(b,1)

print,'Sigma from 10 frame modelled data noise.'+' '+a+' '+'+/-'+' '+b



;FRAME BY FRAME

total_frame=fltarr(sz(1),sz(2),10)

FOR i=0, 9 DO BEGIN

total_frame(*,*,i)=data(*,*,i)-data(*,*,(i+1))

ENDFOR

plothist, total_frame,xhist,yhist,/AUTOBIN, /fill,fcolor='blue',color='blue',axiscolor='black',xtitle=' Frame diff over 10 frames.',ytitle='Frequency.',charsize=1,/noplot

est=[max(yhist),0,150]

fit = gaussfit(xhist,yhist,coeff,sigma=error, estimates=est,nterms=3)

;loadct,2

;oplot,xhist,fit,color='100',thick=7,linestyle=1


a=string(coeff(2))
a=strtrim(a,1)
b=string(error(2))
b=strtrim(b,1)


print,'Sigma from histogram gaussian fit of total 10 frame difference:'+' '+a+' '+'+/-'+' '+b


;!P.Multi = [0,3,3]
;FOR i=1,9 DO BEGIN
;plothist,tot_noise_av(*,*,i),color='blue' ;poisson
;plothist,total_frame(*,*,i),/overplot ;stark
;ENDFOR
;!P.Multi = 0



;METHOD 2 OF NOISE ESTIMATE

print, 'METHOD 2'
print, 'MODELLED NOISE FROM DATA POISSON ERRORS, DARK AND FLAT COMPARED WITH STARK METHOD(&ATROUS)'


;MODELLED GAUSSIAN NOISE FROM DATA-STARK

fil_im = fltarr(sz(1),sz(2),10)

FOR i=0, 9 DO BEGIN

fil_im(*,*,i) = data(*,*,i)-smooth(data(*,*,i),[2,2],/edge_truncate)

ENDFOR

plothist, fil_im,xhist1,yhist1,axiscolor='black',xtitle='Filted plot intensity.',ytitle='Frequency.',/autobin,/noplot,charsize=1

est1=[max(yhist1),0,10]

fit1 = gaussfit(xhist1,yhist1,coeff1,sigma=error1, estimates=est1,nterms=3)

;loadct,2

;oplot,xhist1,fit1,color='100',thick=7,linestyle=1


;MODELLED NOISE FROM POSSION PART

;Dont need to root poisson as it will be squared

;ADDING DARKS AND FLATS




mod_noise=fltarr(sz(1),sz(2),10)

FOR i=0,9 DO BEGIN

tot_sigma=sqrt(abs(data(*,*,i))+sigma_dark^2+sigma_flat^2)

mod_noise(*,*,i)=tot_sigma*randomn(seed,sz(1),sz(2))

ENDFOR



plothist, mod_noise,xhist,yhist,axiscolor='black',xtitle='Total data noise single frame.',ytitle='Frequency.',/autobin,/noplot,charsize=1

est=[max(yhist),0,10]

fit = gaussfit(xhist,yhist,coeff,sigma=error, estimates=est,nterms=3)

;oplot,xhist,fit,color='100',thick=7,linestyle=1

a=string(coeff(2))
a=strtrim(a,1)
b=string(error(2))
b=strtrim(b,1)
print,'Sigma from modelled noise:'+' '+a+' '+'+/-'+' '+b
a=string(coeff1(2))
a=strtrim(a,1)
b=string(error1(2))
b=strtrim(b,1)
print,'Sigma from filtered image:'+' '+a+' '+'+/-'+' '+b

;window,1
;!P.Multi = [0,3,3]
;FOR i=1,9 DO BEGIN
;plothist,mod_noise(*,*,i),color='blue' ;poisson
;plothist,fil_im(*,*,i),/overplot ;stark
;ENDFOR
;!P.Multi = 0


;ATROUS METHOD

print, 'ATROUS'

atrous, (1.*data(*,*,0)),decomposition=decomposition,/speed

decomp=decomposition(*,*,7)

plothist, decomp,xhist, yhist, /AUTOBIN,/noplot,/fill,fcolor='blue',color='blue',axiscolor='black',xtitle=‘Atrous filtered intensity number’,ytitle=‘frequency’,

est=[max(yhist),0,10]

fit = gaussfit(xhist,yhist,coeff,estimates=est,sigma=er,nterms=3)

;loadct,2

;oplot, xhist,fit, color=‘100’,linestyle=1,thick=7

a=string(coeff(2))
a=strtrim(a,1)
b=string(er(2))
b=strtrim(b,1)

print, 'Sigma from atrous'+' '+a+' '+'+/-'+' '+b

END
