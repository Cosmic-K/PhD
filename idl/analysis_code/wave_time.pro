;Krishna Mooroogen
;Northumbria University
;krishna.mooroogen@northumbria.ac.uk
;PhD supervisor RIchard Morton
;---------------------------------------------------------------------
;
;NOTES
;
;Seems to work
;May want to switch time and wave round
;Is single pixel enough?
;
;PURPOSE
;---------------------------------------------------------------------
;
;Makes wavelength time diagram
;
;
;DESCRIPTION
;Makes wavelength time diagram of single pixel
;---------------------------------------------------------------------


;INPUTS
;---------------------------------------------------------------------
;DATA -- DATA_IMAGE(x,y,lam,t)
;OUTPUTS
;NOT YET DEFINED
;

pro wave_time,data,wt

;DEFINE CONSTANTS
;---------------------------------------------------------------------
Mm = (0.725/16.981891892)
wt = fltarr(n_elements(data[0,0,*,0]),n_elements(data[0,0,0,*]))
zsize=n_elements(data[0,0,0,*])

;INTITAL PLOTTING
;---------------------------------------------------------------------
cgimage,data(*,*,0,0),/window,/axes,output='ps',/keep_aspect_ratio

;PICKING LOCATION
;---------------------------------------------------------------------
pick,x,y,/window
cgplot,x,y,/window,psym=1,/overplot,color='yellow'

;CREATING WAVE TIME ARRAY
;---------------------------------------------------------------------

x=round(x)
y=round(y)

FOR i=0,zsize-1  DO BEGIN
wt(*,i)=reform(data(x,y,*,i))
ENDFoR

END