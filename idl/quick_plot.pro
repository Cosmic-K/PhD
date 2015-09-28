pro quick_plot,data1=data1

;time_stamps=[0,227,455,681,908,1116]

Mm = (0.725/16.981891892)
sz = size(data1)
xsz = Mm*(float(sz(1))-1.0)
ysz = Mm*(float(sz(2))-1.0)

data=sum(data1[*,*,0:227],2)


cgimage,data,/window,/axes,output='ps',/keep_aspect_ratio,yrange=[0,ysz],xrange=[0,xsz],xtitle='Mm',charsize=1.5
cgtext,-3,20,'Mm',orientation=90,/window,/data,charsize=1.5
;cgAxis, YAxis=0,title='Mm',/window
;cgAxis, XAxis=0,title='Mm',/window
ch=''
WHILE ch NE 'y' DO BEGIN
pick,x,y,/window
cgplot,x,y,/window,psym=7,/overplot,color='yellow'
READ,ch,prompt='Are you done?'
ENDWHILE

END