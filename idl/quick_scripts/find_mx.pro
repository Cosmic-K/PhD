FUNCTION find_mx, data, num_max,dim=dim
	;default dimension is 1 

	sz=SIZE(data)
	dt=data

	IF KEYWORD_SET(dim) EQ 0 THEN dim=1  
	 
	FOR i=0, num_max-1 DO BEGIN
		mx=max(dt,k,dimension=dim,/NAN,/absolute)
		IF n_elements(mx_list) eq 0 THEN mx_list=mx ELSE mx_list=[temporary(mx_list),mx]
		IF n_elements(mxk_list) eq 0 THEN mxk_list=k ELSE mxk_list=[temporary(mxk_list),k]
		dt=dt[where(dt lt mx)]
	ENDFOR

RETURN, [[[mx_list],[mxk_list]]]
END