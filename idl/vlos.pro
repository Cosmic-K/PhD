;Function to calculate velocity from wavelength(nm) redshift

FUNCTION vlos,lambda,delta_lambda

c=3E8
z=float(delta_lambda)/(lambda)
vel=c*(float((z+1)^2 -1)/float((z+1)^2 +1))
print, vel
return, vel
END