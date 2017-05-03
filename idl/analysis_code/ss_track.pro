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
;SHOULD ADD FEATURE TO GET TD COORDS OUT TO REPRODUCE MAyBE ALSO IN PUT CENTRE RATHER THAN CLICK 
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
;DATA -- DATA_IMAGE(xcen,ycen,t)
;
;OUTPUTS
;---------------------------------------------------------------------
;series of saved .idl file containing t/d diagram
;

pro ss_track,data,data_o,xcen=xcen,ycen=ycen,nw=nw


;DEFINE CONSTANTS
;---------------------------------------------------------------------
;Mm = (0.725/16.981891892)
r=[20,40,60,80,100,120,140,160,180,200,220,240,260,280,300,320,340,360] ; this may need modification
num_r=n_elements(r)
props={dimension:[800,800],marg:[0.2,0.2,0.2,0.2],FONT_NAME:'Helvetica'}
Mm = (0.725/16.981891892)

tot_xrad=0
tot_yrad=0

slit_lnth=round(2*!PI*r) ; may not be right

app=(360*(!PI/180))/float(slit_lnth)


r_index=total([0,slit_lnth],/cumulative)

;CHECKING DATA TyPE IF 3D OR 4D
;---------------------------------------------------------------------
IF n_elements(size(data)) EQ 6 THEN BEGIN
zsize=n_elements(data[0,0,*])
;INTITAL PLOTTING
;---------------------------------------------------------------------
tvim,data(*,*,0)

;PICKING SUNSPOT CENTRE
;---------------------------------------------------------------------
IF n_elements(xcen) EQ 0 THEN pick,xcen,ycen;should print out position
wdelete
im=image(-9000>total(data[*,*,0:3],3)<9000,_extra=props,rgb_table=70,title='Ca II doppler velocity')
a1=axis('x',location='bottom',coord_transform=[0,mm],title='Distance (Mm)',tickFONT_NAME='Helvetica')
a2=axis('y',location='left',coord_transform=[0,mm],title='Distance (Mm)',tickFONT_NAME='Helvetica')
a3=axis('x',location='top',coord_transform=[0,mm],showtext=0,ticklayout=1)
a4=axis('y',location='right',coord_transform=[0,mm],showtext=0,ticklayout=1)

p=plot([xcen,0,0]>0,[ycen,0,0]>0,linestyle=6,/curr,/overplot,symbol='+',color='yellow')

;PLOTTING CIRLCES AND GETTING xcen/ycen COORDINATES
;---------------------------------------------------------------------

FOR j=0,(num_r-1) DO BEGIN
x_rad=0
y_rad=0
sl=slit_lnth(j)
angle=findgen(slit_lnth(j))*app(j)

FOR i=0, sl-1 DO BEGIN
x_rad=[temporary(x_rad),(xcen+r(j)*cos(angle(i)))]
y_rad=[temporary(y_rad),(ycen+r(j)*sin(angle(i)))]

ENDFOR

p1=plot(x_rad[1:*],y_rad[1:*],/curr,/overplot,color='white')
tot_xrad=[temporary(tot_xrad),x_rad[1:*]]
tot_yrad=[temporary(tot_yrad),y_rad[1:*]]
ENDFOR

tot_xrad=tot_xrad[1:*]
tot_yrad=tot_yrad[1:*]


;SENDING COORDS TO TRACKING TO MAKE T/D
;---------------------------------------------------------------------

mindat=min(data)
IF mindat GT 0 THEN mindat=-1. ELSE mindat=mindat-0.1*sqrt((moment(data))[1])

FOR i=0, num_r-1 DO BEGIN
data_o=fltarr(slit_lnth[i],zsize)

;x=congrid(tot_xrad[r_index(i):(r_index(i+1)-1)],slit_lnth[i],cubic=-0.5)
;y=congrid(tot_yrad[r_index(i):(r_index(i+1)-1)],slit_lnth[i],cubic=-0.5)

x=tot_xrad[r_index(i):(r_index(i+1)-1)]
y=tot_yrad[r_index(i):(r_index(i+1)-1)]

FOR j=0, (zsize-1) DO BEGIN

data_o[*,j]=interpolate(reform(data[*,*,j]),x,y,cubic=-0.5,missing=mindat)

ENDFOR

save,data_o,description='radial time distance diagram with radius '+strtrim(i,1),filename='ss_test_'+strtrim(i,1)+'.idl'

ENDFOR

ENDIF

;---------------------------------------------------------------------
;IF 4D DATA TyPE THEN
;---------------------------------------------------------------------


IF n_elements(size(data)) EQ 7 THEN BEGIN

zsize=n_elements(data[0,0,0,*])
;INTITAL PLOTTING
;---------------------------------------------------------------------
IF nw eq 0 THEN print, 'Specify with wave position to plot, keyword nw.'
cgimage,data(*,*,nw,0),/window,/axes,output='ps',/keep_aspect_ratio

;PICKING SUNSPOT CENTRE
;---------------------------------------------------------------------
IF n_elements(xcen) eq 0 THEN pick,xcen,ycen,/window
cgplot,xcen,ycen,/window,psym=1,/overplot,color='yellow'

;PLOTTING CIRLCES AND GETTING xcen/ycen COORDINATES
;---------------------------------------------------------------------

FOR j=0,(num_r-1) DO BEGIN
x_rad=0
y_rad=0
sl=slit_lnth(j)
angle=findgen(slit_lnth(j))*app(j)

FOR i=0, sl-1 DO BEGIN
x_rad=[temporary(x_rad),(xcen+r(j)*cos(angle(i)))]
y_rad=[temporary(y_rad),(ycen+r(j)*sin(angle(i)))]
ENDFOR

cgplot,x_rad[1:*],y_rad[1:*],/window,/overplot,color='white'
tot_xrad=[temporary(tot_xrad),x_rad[1:*]]
tot_yrad=[temporary(tot_yrad),y_rad[1:*]]
ENDFOR

tot_xrad=tot_xrad[1:*]
tot_yrad=tot_yrad[1:*]


;SENDING COORDS TO TRACKING TO MAKE T/D
;---------------------------------------------------------------------

mindat=min(data)
IF mindat GT 0 THEN mindat=-1. ELSE mindat=mindat-0.1*sqrt((moment(data))[1])

FOR k=0, 14 DO BEGIN
FOR i=0, (num_r-1) DO BEGIN
data_o=fltarr(slit_lnth[i],zsize)

;xcen=congrid(tot_xrad[r_index(i):(r_index(i+1)-1)],slit_lnth[i],cubic=-0.5)
;ycen=congrid(tot_yrad[r_index(i):(r_index(i+1)-1)],slit_lnth[i],cubic=-0.5)
x=tot_xrad[r_index(i):(r_index(i+1)-1)]
y=tot_yrad[r_index(i):(r_index(i+1)-1)]

FOR j=0, (zsize-1) DO BEGIN

data_o[*,j]=interpolate(reform(data[*,*,k,j]),x,y,cubic=-0.5,missing=mindat)

ENDFOR

save,data_o,description='radial time distance diagram with radius '+strtrim(i,1)+' in height '+strtrim(k,1),filename='ss_test_height'+strtrim(k,1)+'_rad'+strtrim(i,1)+'.idl'

ENDFOR
ENDFOR
ENDIF



END