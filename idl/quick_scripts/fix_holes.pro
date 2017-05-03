;+
;Author: Krishna Mooroogen
;Institution: Northumbria University 
;Date: 16/05/16
;Description: Code to interpolate time frame gaps in time distance data 
;Inputs: 2D time distance data and time array of points to interpolate
;Output: interpolated time distance data
;make sure that the time distance data is orientated so that the time is on the x axis
;alternatively if t input is set (lenght of time) code will orient data for you
;needs changng 
;-

function fix_holes, data, time,t=t

    sz=size(data)

    IF N_ELEMENTS(t) NE 0 THEN BEGIN
    	orin=where(sz eq t)
    	IF orin eq 2 THEN data=rotate(data,3)
    	y=fingen(sz(1))
    ENDIF ELSE BEGIN
    	y=fingen(sz(1))
    ENDELSE

	mindat=min(data)
	IF mindat GT 0 THEN mindat=-1. ELSE mindat=mindat-0.1*sqrt((moment(data))[1])
    
	interp=interpolate(data,time,y,cubic=-0.5,missing=mindat,/grid)
	data_out=[data[0:time[0]-1,*],interp,data[time[-1]+1:*,*]]

return,data_out
END