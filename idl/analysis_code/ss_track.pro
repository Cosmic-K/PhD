;Krishna Mooroogen
;Northumbria University
;krishna.mooroogen@northumbria.ac.uk
;PhD supervisor RIchard Morton
;---------------------------------------------------------------------
;
;NOTES
;#send coords to wave_track code to interp and make t/d
;Cureenlty sunspot centre arbutarilly picked
;May use COG method or fititng to find centre
;Output likely strcuture or array of time/distance
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
;TBC
;---------------------------------------------------------------------


;INPUTS
;---------------------------------------------------------------------
;DATA -- DATA_IMAGE(x,y,t)
;OUTPUTS
;NOT YET DEFINED
;

pro ss_track,data

;DEFINE CONSTANTS
;---------------------------------------------------------------------
Mm = (0.725/16.981891892)
sz = size(data)
r=[100,150,200,250,300] ; this may need modification
angle=findgen(361)*(!PI/180)
tot_x=0
tot_y=0

;INTITAL PLOTTING
;---------------------------------------------------------------------
cgimage,data,/window,/axes,output='ps',/keep_aspect_ratio

;PICKING SUNSPOT CENTRE
;---------------------------------------------------------------------
pick,x,y,/window
cgplot,x,y,/window,psym=1,/overplot,color='yellow'

;PLOTTING CIRLCES AND GETTING X/Y COORDINATES
;---------------------------------------------------------------------

FOR j=0,4 DO BEGIN
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


;SENDING COORDS TO TRACKING
;---------------------------------------------------------------------

r_100=transpose([[tot_x[0:360]],[tot_y[0:360]]])
r_150=transpose([[tot_x[361:721]],[tot_y[361:721]]])
r_200=transpose([[tot_x[722:1082]],[tot_y[722:1082]]])
r_250=transpose([[tot_x[1083:1443]],[tot_y[1083:1443]]])
r_300=transpose([[tot_x[1444:1804]],[tot_y[1444:1804]]])
;temporary what could be better is to send coords in 361 pairs straight to t/d part in a loop

END