;Krishna Mooroogen.
;Northumbria University, Solar group.
;13/11/14.
;Program to perform sanity check on errors.


;Assume data is a data cube for now add additional handling later.
;Additonal noise can be made using the noise function or randomn, which is more appropriate to use?
;In the case of randomn which sd? sd as calculated from stdev or possion error? from data or from hist of data?
;add condition when only darks or flats is known.

pro error_test, data,darks=darks,flat=flat,gain=gain

;set_plot,'ps'
;device,/encapsul,/color,filename='histo.eps'

;need to add gain later.

IF n_elements(gain) eq 0 THEN BEGIN
gain = 1
ENDIF

photon_er = sqrt(abs(gain*data(*,*,0)))


;estimate for darks and flats

IF n_elements(darks) eq 0 THEN BEGIN

;add_noise = fix(noise(data(*,*,0)))

darks = photon_er*randomn(seed,n_elements(data(*,*,0)))
flat = darks


ENDIF

;total estimate for data error.

data_er = sqrt((photon_er^2)+(darks^2)+(flat^2))


;Calculate avaerage sd of data from histogram.

sd_arr=fltarr(10)
errors=fltarr(10)

FOR i=0, 9 DO BEGIN

    diff=data(*,*,i)-data(*,*,(i+1))

    plothist, diff,xhist,yhist,/NOPLOT

    est=[max(yhist),0,150]

    fit = gaussfit(xhist,yhist,coeff,sigma=error, estimates=est,nterms=3)

    sd_arr[i]=coeff[2]
    errors[i]=error[2]

ENDFOR

sd_av = mean(sd_arr)
data_er_av = mean(data_er)
sd_er = mean(errors)

print,'Histogram sdev as a percentage of average data error:',(sd_av/data_er_av)*100;,format='(d0.2)'
print,'Discrepancy:',(data_er_av-sd_av);,format='(d0.2)'
print,'Average sdev from histogram gaussina fit:',sd_av,'+\-',sd_er;,format='(d0.2)'
print,'Average error from data:',data_er_av;,format='(d0.2)'
;fix the format
;fix ps
plothist,diff,/fill,fcolor='blue',color='blue',axiscolor='black',title='Histrogram of frame difference with gaussian fit.',xtitle='Difference number.',ytitle='Fequency.'
loadct,2
oplot,xhist,fit,color='100',thick=7,linestyle=1

al_legend, ['Histogram of frame difference','Gaussian fit'],psym=[4,4],colors=['blue','red']

;device,/close
;set_plot,'x'

return

end
