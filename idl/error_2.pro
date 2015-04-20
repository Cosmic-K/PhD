;Krishna Mooroogen.
;Northumbria University, Solar group.
;23/3/15.
;Routine to perform sanity check on errors.

;Assume data is a data cube for now add additional handling later.
;note to self add one back

;CALL: error_2,data=data,darks=darks,flats=flats

;PURPOSE: Compare noise estimates in astronomical data with modelled noise.

;INPUTS: data: 3D data cube of from x,y,t.
;        darks: 2D array dark frame data.
;        flat: 2D array flat frame data.


pro error_2, data,darks,flats

;Darks and flats cropped to match the cropped data,
;try gain divided and gain multiplied

data1=data(5:950,15:950,0:20)
darks1=darks(30:950,20:950)
flats1=flats(30:950,30:980)

data_sum=float(sum(data1,2))/21.

data1=data_sum

;calculating possion noise
pois_noise=sqrt(abs(data1))

;ADDING DARK AND FLAT IN QUADRATURE THEN ADDING GAUSSIAN NOISE TO POSSION NOISE

;SIGMA ESTIMATE FOR DARKS

plothist, darks1,xhist,yhist,/AUTOBIN,/fill,fcolor='blue',color='blue',axiscolor='black',xtitle='Darks',/noplot

est=[max(yhist),160,2]

fit = gaussfit(xhist,yhist,coeff,sigma=error, estimates=est,nterms=3,CHISQ=chi)
loadct,1,/silent
oplot,xhist,fit,linestyle=2,color=250
loadct,0,/silent
sigma_dark=coeff(2)

a=string(coeff(2))
a=strtrim(a,1)
b=string(error(2))
b=strtrim(b,1)

print,'Sigma_dark:'+' '+a+' '+'+/-'+' '+b
print,'Min dark',min(darks1)
print,'Max dark',max(darks1)
print,'chi',chi

;SIGMA ESTIMATE FOR FLAT

plothist, flats1,xhist,yhist,/AUTOBIN,/fill,fcolor='blue',color='blue',axiscolor='white',xtitle='Flats',/noplot

est=[max(yhist),170,7]

fit = gaussfit(xhist,yhist,coeff,sigma=error, estimates=est,nterms=3,CHISQ=chi)
loadct,1,/silent
;oplot,xhist,fit,linestyle=2,color=200
loadct,0,/silent
sigma_flat=coeff(2)

a=string(coeff(2))
a=strtrim(a,1)
b=string(error(2))
b=strtrim(b,1)

print, 'Sigma flat:'+' '+a+' '+'+/-'+' '+b
print,'Min flat',min(flats1)
print,'Max flat',max(flats1)
print,'chi',chi

tot_noise = sqrt((pois_noise^2)+(sigma_dark^2)+(sqrt(abs(flats))^2))


;model noise from fit
x=data_sum

;consts for later
c=1.112

model_noise=(0.00013*x)+c
cgscatter2d,model_noise,tot_noise,xtitle='modelled noise',ytitle='poisson + dark + flat'

END