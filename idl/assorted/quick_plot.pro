;Krishna Mooroogen
;Northumbria University
;krishna.mooroogen@northumbria.ac.uk
;PhD supervisor Richard Morton
;
;
;PURPOSE
;-------------------------------------------------
;Make quick FOV plot with marked points of interest

;NOTES
;-------------------------------------------------
;This specific case is for fibrils in chromospere
;Averaged over time segments to show avrage motion
;Time segments listed in constants
;Change as needed

;INPUTS
;------------------------------------------------
;DATA1 -- Image data 2D x,y

pro quick_plot,data1=data1

;CONSTANTS
--------------------------------------------------
Mm = (0.725/16.981891892)
sz = size(data1)
xsz = Mm*(float(sz(1))-1.0)
ysz = Mm*(float(sz(2))-1.0)
;time_stamps=[0,227,455,681,908,1116]

;AVERAGING DATA over itme segment
;-------------------------------------------------
data=sum(data1[*,*,0:227],2)

;INITIAL PLOTTING
;---------------------------------------------------
cgimage,data,/window,/axes,output='ps',/keep_aspect_ratio,yrange=[0,ysz],xrange=[0,xsz],xtitle='Mm',charsize=1.5
cgtext,-3,20,'Mm',orientation=90,/window,/data,charsize=1.5

;PLACING POINTS OF INTEREST
;simple point and click interface
;while loop ends when user specifies no more points to click on
;--------------------------------------------------
;

ch=''
WHILE ch NE 'y' DO BEGIN
pick,x,y,/window
cgplot,x,y,/window,psym=7,/overplot,color='yellow'
READ,ch,prompt='Are you done?'
ENDWHILE

END