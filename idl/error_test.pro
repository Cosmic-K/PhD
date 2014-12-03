;Krishna Mooroogen.
;Northumbria University, Solar group.
;02/12/14.
;Routine to perform sanity check on errors.


;Assume data is a data cube for now add additional handling later.
;Might be worth adding peak det to estimate width and mean.
;Edge tracker to cut edges of darks and flats.
;Subject to chnage.
;Add plot option to switch between plots?

;CALL: error_test,data,darks,flats

;PURPOSE: Compare errors in astronomical data with time series(frame to frame in time dimension) data to check for discprencies.

;INPUTS: data: 3D data cube of from x,y,t
;        darks: 2D array dark frame data.
;        flat: 2D array flat frame data.


pro error_test, data,darks,flat



noisey_im = poidev(data(*,*,0))

poisson_noise = noisey_im-data(*,*,0)

sz=size(poisson_noise)




;sigma estimate for darks

plothist, darks,xhist,yhist,/AUTOBIN,/NOPLOT

est=[max(yhist,I),I,5]

fit = gaussfit(xhist,yhist,coeff,sigma=error, estimates=est,nterms=3)

sigma_dark = coeff[2]

print,'sigma_dark:',sigma_dark





;sigma estimate for flat

plothist, flat(47:980,25:990),xhist,yhist,/AUTOBIN,/NOPLOT

est=[max(yhist,I),150,50]; why 'I' here is not correct I dont know


fit = gaussfit(xhist,yhist,coeff,sigma=error, estimates=est,nterms=3)

sigma_flat = coeff[2]

print, 'sigma flat:', sigma_flat




;Adding dark and flat sigma in quadrature

sigma_tot = sqrt((sigma_dark^2)+(sigma_flat)^2)

print, 'sigma total:',sigma_tot

gauss_noisefd = sigma_tot*randomn(seed, sz(1),sz(2))

tot_noise=gauss_noisefd+poisson_noise

plothist, tot_noise,xhist,yhist,/fill,fcolor='blue',color='blue',axiscolor='black',xtitle='Total noise intensity.',ytitle='Frequency.',/autobin,charsize=1,/noplot





;Frame by frame

;Averaged sigma over ten frames

sd_arr=fltarr(10)
errors=fltarr(10)

FOR i=0, 9 DO BEGIN

    diff=data(*,*,i)-data(*,*,(i+1))

    plothist, diff,xhist,yhist,/NOPLOT,/AUTOBIN

    est=[max(yhist),0,150]

    fit = gaussfit(xhist,yhist,coeff,sigma=error, estimates=est,nterms=3)

    sd_arr[i]=coeff[2]
    errors[i]=error[2]

ENDFOR

sd_av = mean(sd_arr)
sd_er = mean(errors)

print,'Average sdev from histogram gaussian fit of frame difference:',sd_av,'+\-',sd_er


;Averaged and total sum

diff=data(*,*,0)-data(*,*,(1))

FOR i=1, 9 DO BEGIN

diff=diff+(data(*,*,i)-data(*,*,(i+1)))

ENDFOR




;Average over 10

av_diff = diff/10

plothist, av_diff,xhist,yhist,/AUTOBIN,/fill,fcolor='blue',color='blue',axiscolor='black',xtitle='Averaged over 10 frame diff.',ytitle='Frequency.',charsize=1,/noplot

est=[max(yhist),0,50]

fit = gaussfit(xhist,yhist,coeff,sigma=error, estimates=est,nterms=3)

loadct,2

oplot,xhist,fit,color='100',thick=7,linestyle=1

sd_av=coeff[2]
sd_er=error[2]

print,'Standard dev from histogram of gaussian fit of averaged 10 frame difference:',sd_av,'+\-',sd_er






;total

plothist, diff,xhist,yhist,/AUTOBIN, /fill,fcolor='blue',color='blue',axiscolor='black',xtitle='Total frame diff.',ytitle='Frequency.',charsize=1

est=[max(yhist),0,150]

fit = gaussfit(xhist,yhist,coeff,sigma=error, estimates=est,nterms=3)

loadct,2

oplot,xhist,fit,color='100',thick=7,linestyle=1

sd_av=coeff[2]
sd_er=error[2]

print,'sdev from histogram gaussian fit of total 10 frame difference:',sd_av,'+\-',sd_er


;device,/encapsul,/color,filename='diff_histo.eps'
;plothist,diff,/fill,fcolor='blue',color='blue',axiscolor='black',xtitle='Difference number.',ytitle='Fequency.',/autobin,charsize=1
;loadct,2
;oplot,xhist,fit,color='100',thick=7,linestyle=1
;al_legend, ['Histogram of frame difference','Gaussian fit'],psym=[4,4],colors=['blue','red']
;device,/close
;set_plot,'x'

end
