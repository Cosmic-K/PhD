PRO diogplot,int,wg,mag,velo

km = (0.725/16.981891892);not really km its mm 
props={dimension:[900,700],marg:[0.198,0.1,0.005,0.05],FONT_NAME:'Helvetica'}


im=image(int[0:259,100:300,0],findgen(260),findgen(201)+100,_EXTRA=props,layout=[4,3,1],title='H$\alpha$ Profile minimum',/curr)
a=axis('x',location='bottom',coord_transform=[0,km],target=im) 
a1=axis('y',location='left',coord_transform=[0,km],title='Distance (Mm)',tickFONT_NAME='Helvetica',target=im)
a2=axis('x',location='top',coord_transform=[0,km],showtext=0,ticklayout=1,target=im)
a3=axis('y',location='right',coord_transform=[0,km],showtext=0,ticklayout=1,target=im)


im1=image(int[90:350,350:550,0],findgen(261)+90,findgen(201)+350,_EXTRA=props,layout=[4,3,5],/curr)
a=axis('x',location='bottom',coord_transform=[0,km],target=im1)
a1=axis('y',location='left',coord_transform=[0,km],title='Distance (Mm)',tickFONT_NAME='Helvetica',target=im1)
a2=axis('x',location='top',coord_transform=[0,km],showtext=0,ticklayout=1,target=im1)
a3=axis('y',location='right',coord_transform=[0,km],showtext=0,ticklayout=1,target=im1)

coord_transform=[0,km],title='Distance (Mm)',tickFONT_NAME='Helvetica',target=im2)
a2=axis('x',location='top',coord_transform
im2=image(int[120:380,250:450,0],findgen(261)+120,findgen(201)+250,_EXTRA=props,layout=[4,3,9],/curr)
a=axis('x',location='bottom',coord_transform=[0,km],title='Distance (Mm)',tickFONT_NAME='Helvetica',target=im2)
a1=axis('y',location='left',=[0,km],showtext=0,ticklayout=1,target=im2)
a3=axis('y',location='right',coord_transform=[0,km],showtext=0,ticklayout=1,target=im2)


im3=image(wg[0:259,100:300,0],findgen(260),findgen(201)+100,_EXTRA=props,layout=[4,3,2],title='H$\alpha$ -906 $\AA$',/curr)
a=axis('x',location='bottom',coord_transform=[0,km],target=im3)
a1=axis('y',location='left',coord_transform=[0,km],target=im3,showtext=0,ticklayout=1)
a2=axis('x',location='top',coord_transform=[0,km],showtext=0,ticklayout=1,target=im3)
a3=axis('y',location='right',coord_transform=[0,km],showtext=0,ticklayout=1,target=im3)


im4=image(wg[90:350,350:550,0],findgen(261)+90,findgen(201)+350,_EXTRA=props,layout=[4,3,6],/curr)
a=axis('x',location='bottom',coord_transform=[0,km],target=im4)
a1=axis('y',location='left',coord_transform=[0,km],target=im4,showtext=0,ticklayout=1)
a2=axis('x',location='top',coord_transform=[0,km],showtext=0,ticklayout=1,target=im4)
a3=axis('y',location='right',coord_transform=[0,km],showtext=0,ticklayout=1,target=im4)


im5=image(wg[120:380,250:450,0],findgen(261)+120,findgen(201)+250,_EXTRA=props,layout=[4,3,10],/curr)
a=axis('x',location='bottom',coord_transform=[0,km],title='Distance (Mm)',tickFONT_NAME='Helvetica',target=im5)
a1=axis('y',location='left',coord_transform=[0,km],target=im5,showtext=0,ticklayout=1)
a2=axis('x',location='top',coord_transform=[0,km],showtext=0,ticklayout=1,target=im5)
a3=axis('y',location='right',coord_transform=[0,km],showtext=0,ticklayout=1,target=im5)



im6=image(gauss_smooth(mag[0:259,(100-17):(300-17)],2,/edge_truncate) <50.>(-50.),findgen(260),findgen(201)+100,_EXTRA=props,layout=[4,3,3],title='LOS Magnetogram (Gauss) Fe 6302 ',/curr)
a=axis('x',location='bottom',coord_transform=[0,km],target=im6)
a1=axis('y',location='left',coord_transform=[0,km],target=im6,showtext=0,ticklayout=1)
a2=axis('x',location='top',coord_transform=[0,km],showtext=0,ticklayout=1,target=im6)
a3=axis('y',location='right',coord_transform=[0,km],showtext=0,ticklayout=1,target=im6)


im7=image(gauss_smooth(mag[90-7:350-7,350-17:550-17],2,/edge_truncate)<50.>(-50.),findgen(261)+90,findgen(201)+350,_EXTRA=props,layout=[4,3,7],/curr)
a=axis('x',location='bottom',coord_transform=[0,km],target=im7)
a1=axis('y',location='left',coord_transform=[0,km],target=im7,showtext=0,ticklayout=1)
a2=axis('x',location='top',coord_transform=[0,km],showtext=0,ticklayout=1,target=im7)
a3=axis('y',location='right',coord_transform=[0,km],showtext=0,ticklayout=1,target=im7)


im8=image(gauss_smooth(mag[120-7:380-7,250-17:450-17],2,/edge_truncate)<50.>(-50.),findgen(261)+120,findgen(201)+250,_EXTRA=props,layout=[4,3,11],/curr)
a=axis('x',location='bottom',coord_transform=[0,km],title='Distance (Mm)',tickFONT_NAME='Helvetica',target=im8)
a1=axis('y',location='left',coord_transform=[0,km],target=im8,showtext=0,ticklayout=1)
a2=axis('x',location='top',coord_transform=[0,km],showtext=0,ticklayout=1,target=im8)
a3=axis('y',location='right',coord_transform=[0,km],showtext=0,ticklayout=1,target=im8)


im9=image(velo[0:259,(100-17):(300-17)],_EXTRA=props,findgen(260),findgen(201)+100,layout=[4,3,4],title='RMS Doppler velocity',/curr)
a=axis('x',location='bottom',coord_transform=[0,km],target=im9)
a1=axis('y',location='left',coord_transform=[0,km],target=im9,showtext=0,ticklayout=1)
a2=axis('x',location='top',coord_transform=[0,km],showtext=0,ticklayout=1,target=im9)
a3=axis('y',location='right',coord_transform=[0,km],showtext=0,ticklayout=1,target=im9)


im10=image(velo[90-7:350-7,350-17:550-17],findgen(261)+90,findgen(201)+350,_EXTRA=props,layout=[4,3,8],/curr)
a=axis('x',location='bottom',coord_transform=[0,km],target=im10)
a1=axis('y',location='left',coord_transform=[0,km],target=im10,showtext=0,ticklayout=1)
a2=axis('x',location='top',coord_transform=[0,km],showtext=0,ticklayout=1,target=im10)
a3=axis('y',location='right',coord_transform=[0,km],showtext=0,ticklayout=1,target=im10)


im11=image(velo[120-7:380-7,250-17:450-17],findgen(261)+120,findgen(201)+250,_EXTRA=props,layout=[4,3,12],/curr)
a=axis('x',location='bottom',coord_transform=[0,km],title='Distance (Mm)',tickFONT_NAME='Helvetica',target=im11)
a1=axis('y',location='left',coord_transform=[0,km],target=im11,showtext=0,ticklayout=1)
a2=axis('x',location='top',coord_transform=[0,km],showtext=0,ticklayout=1,target=im11)
a3=axis('y',location='right',coord_transform=[0,km],showtext=0,ticklayout=1,target=im11)







END