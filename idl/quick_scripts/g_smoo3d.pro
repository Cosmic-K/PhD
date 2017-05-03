;FUNCTION: performs gauss_smooth over 3 dimensions
;INPUTS: needs to be 3d array where 3rd dimension is time 
;OUTPUTS: 3d array of same size as input smoothed
;w is the sigma over which to smooth 

function g_smoo3d,data,w

	sz = SIZE(data)
	sm_out = fltarr(sz(1),sz(2),sz(3))

	FOR i=0, sz(3)-1 DO BEGIN
	
	sm_out[*,*,i] = gauss_smooth(data[*,*,i],w,/edge_truncate)
	
	ENDFOR

	RETURN, sm_out
END