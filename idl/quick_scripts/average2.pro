;takes 3d data and averages spatially

FUNCTION average2, data
	
	sz = size(data)

	av=total(data,1)/sz(1)
	av1=total(av,1)/sz(2)

	RETURN, av1


END 