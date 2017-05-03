pro fovslits,data,out_ts

Mm = (0.725/16.981891892)
time = 1.343
props={dimension:[800,800],marg:[0.2,0.01,0.01,0.01],FONT_NAME:'Helvetica'}

wave_track,2,12,12,data,out_ts,p_coord=pcoords,arr=arr,x1=xx1,x2=xx2,y1=yy1,y2=yy2
szts = size(out_ts)
tcoords=arr

im=image(data[*,*,0],_EXTRA=props,layout=[2,3,1],axis_style=0,/curr)
a1=axis('x',location='bottom',coord_transform=[0,mm],title='Distance (Mm)',tickFONT_NAME='Helvetica',target=im)
a2=axis('y',location='left',coord_transform=[0,mm],title='Distance (Mm)',tickFONT_NAME='Helvetica',target=im)
a3=axis('x',location='top',coord_transform=[0,mm],showtext=0,ticklayout=1,target=im)
a4=axis('y',location='right',coord_transform=[0,mm],showtext=0,ticklayout=1,target=im)
p=plot([(xx1),(xx2)],[(yy1),(yy2)],/curr,/over,color='white')

FOR j=0,(szts(3)-1) DO BEGIN
p1=plot([(tcoords[0,j]),(tcoords[1,j])],[(tcoords[2,j]),(tcoords[3,j])],/curr,/over,color='white')
ENDFOR

;im1=image(rotate(out_ts[*,*,5],3),_EXTRA=props,layout=[2,1,2],/curr,aspect_ratio=2)
;a5=axis('x',location='bottom',coord_transform=[0,time],title='Time (seconds)',tickFONT_NAME='Helvetica',target=im1)
;a6=axis('y',location='left',coord_transform=[0,mm],title='Distance (Mm)',tickFONT_NAME='Helvetica',target=im1)
;a7=axis('x',location='top',coord_transform=[0,time],showtext=0,ticklayout=1,target=im1)
;a8=axis('y',location='right',coord_transform=[0,mm],showtext=0,ticklayout=1,target=im1)


END