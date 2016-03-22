;Krishna Mooroogen
;Northumbria University
;krishna.mooroogen@northumbria.ac.uk
;PhD supervisor Richard Morton
;
;PURPOSE
;-----------------------------------------------------------
;Function to calculate velocity from wavelength(nm) redshift

;INPUTS
;----------------------------------------------------------
;LAMBDA -- rest wavelength in nm
;DELTA_LAMBDA -- chnage in wavelength

FUNCTION vlos,lambda,delta_lambda

;CONSTANTS
;----------------------------------------------------------
c=2.99E8

;SPD OF LIGHT

;CALCULATING REDSHIFT
;----------------------------------------------------------
z=float(delta_lambda)/(lambda)

;CALCULATING VELOCITY
;----------------------------------------------------------
vel=c*(float((z+1)^2 -1)/float((z+1)^2 +1))

;DISPLAY AND RETURN VELOCITY
;----------------------------------------------------------

;print, vel
return, vel

END
;w1-w2/w1*c