;Krishna Mooroogen
;Northumbria University
;krishna.mooroogen@northumbria.ac.uk
;PhD supervisor RIchard Morton
;---------------------------------------------------------------------
;
;NOTES
;Radius and filenames to be check and changed as needed
;Cureenlty sunspot centre arbutarilly picked
;May use COG method or fititng to find centre
;
;
;
;PURPOSE
;---------------------------------------------------------------------
;Sunspot radial time/distance tracker
;Makes t/d diagram as fucntion of radius outward around sunspot centre
;
;
;DESCRIPTION
;Using set list of radii (worked out from data)
;Circles are overplotted on the the image (anlges in radians)
;Coordiates of circle points sent to tracking routine for interping
;Based on ricks old code for tracking fibrial structures in straight line
;Double loop used to chnage radii and angle in one go.
;
;---------------------------------------------------------------------


;INPUTS
;---------------------------------------------------------------------
;DATA -- DATA_IMAGE(x,y,t)
;
;OUTPUTS
;---------------------------------------------------------------------
;series of saved .idl file containing t/d diagram
;

pro ss_track,data

;DEFINE CONSTANTS
;---------------------------------------------------------------------
Mm = (0.725/16.981891892)
r=[20,40,60,80,100,120,140,160,180,200,220,240,260,280] ; this may need modification
num_r=n_elements(r)
angle=findgen(361)*(!PI/180)
tot_x=0
tot_y=0

zsize=n_elements(data[0,0,*])
slit_lnth=round(2*!PI*r) ; may not be right

r_index=findgen(num_r+1)*361

;INTITAL PLOTTING
;---------------------------------------------------------------------
cgimage,data(*,*,0),/window,/axes,output='ps',/keep_aspect_ratio

;PICKING SUNSPOT CENTRE
;---------------------------------------------------------------------
pick,x,y,/window
cgplot,x,y,/window,psym=1,/overplot,color='yellow'

;PLOTTING CIRLCES AND GETTING X/Y COORDINATES
;---------------------------------------------------------------------

FOR j=0,(num_r-1) DO BEGIN
x1=0
y1=0
FOR i=0,360 DO BEGIN
x1=[temporary(x1),(x+r(j)*cos(angle(i)))]
y1=[temporary(y1),(y+r(j)*sin(angle(i)))]
ENDFOR
cgplot,x1[1:*],y1[1:*],/window,/overplot,color='white'
tot_x=[temporary(tot_x),x1[1:*]]
tot_y=[temporary(tot_y),y1[1:*]]
ENDFOR

tot_x=tot_x[1:*]
tot_y=tot_y[1:*]


;SENDING COORDS TO TRACKING TO MAKE T/D
;---------------------------------------------------------------------

mindat=min(data)
IF mindat GT 0 THEN mindat=-1. ELSE mindat=mindat-0.1*sqrt((moment(data))[1])

FOR i=0, (num_r-1) DO BEGIN
data_o=fltarr(slit_lnth[i],zsize)

xx=congrid(tot_x[r_index(i):(r_index(i+1)-1)],slit_lnth[i],cubic=-0.5)
yy=congrid(tot_y[r_index(i):(r_index(i+1)-1)],slit_lnth[i],cubic=-0.5)

FOR j=0, (zsize-1) DO BEGIN

data_o[*,j]=interpolate(reform(data[*,*,j]),xx,yy,cubic=-0.5,missing=mindat)

ENDFOR

save,data_o,description='radial time distance diagram with radius '+strtrim(i,1),filename='ss_test_'+strtrim(i,1)+'.idl'

ENDFOR



END