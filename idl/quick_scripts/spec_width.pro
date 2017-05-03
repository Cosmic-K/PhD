;Test code for measureing doppler wdith of spectral profile 
; will perform fit over 1 frame can be chnaged to do more but will take longer depending on fram number 
;gauss estimates will need changing. 

pro spec_width,data,wave,wid_map,debug=debug,est=est

	sz=size(data)
    nx= sz(1)
	ny= sz(2)
	nw= sz(3)
	;nt= sz(4)
	;2.8,0.13,8542 ca 
	;ha 2.4, 0.2 6563
	;ha mon 0.1,0.5,6563.0
    ;ca mom 0.5,0.1,8542.0
	
	;nx=251
	;ny=151
	;nw=19

	wid_map = fltarr(nx,ny)
	x=wave
  	

	FOR  j=0, ny-1 DO BEGIN 
	FOR  i=0, nx-1 DO BEGIN
	y=reform(data[i,j,0:nw-1])
	cent=total(double(y)^2*x)/total(double(y)^2)
	IF n_elements(est) NE 0 THEN estimates=[max(y),min(y),est(0),est(1),est(2)] ELSE $ 
	estimates=[max(y),min(y),2.8,0.13,8542.0]
	f=voigtfit(x,y,coeff,guess=estimates,/quiet)

	IF keyword_set(debug) THEN BEGIN
	plot,x,y
	oplot,x,f,linestyle=2
	print,i,j
	print,coeff
	pause
	ENDIF
	;IF (coeff(3) gt 1) OR (coeff(3) lt 1e-4) THEN wid_map[i,j]=0.15 ELSE 
	wid_map[i,j]=coeff(3)
	ENDFOR
	ENDFOR


END