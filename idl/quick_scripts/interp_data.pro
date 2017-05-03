;+
;Author: Krishna Mooroogen
;Institution: Northumbria University 
;Date: 16/05/16
;Description: Code to interpolate time frame gaps and uneven cadence 
;Inputs: 3D time distance data and time array of points to interpolate
;Output: interpolated time distance data
;t1 current irregular time 
;t2 required regular time
;-

pro interp_data, data, t1 ,t2, data_out
	sz=size(data)
	nt=n_elements(t2)
	fin=nt-1
	data_out=fltarr(sz(1),sz(2),15,nt)

FOR j=0,sz(3)-1 DO BEGIN

	FOR i=0, sz(2)-1 DO BEGIN
		FOR k=0, sz(1)-1 DO BEGIN

		data_out[k,i,j,0:fin]=interpol(data[k,i,j,0:sz(4)-1],t1,t2,/spline)

		ENDFOR
	ENDFOR
ENDFOR

END