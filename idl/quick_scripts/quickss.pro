mm = (0.725/16.981891892)

    im=image(total(0.1>dopp_w[*,*,*]<0.5,3),layout=[2,2,3],axis_style=4,/curr,title='Spectral Width (averaged)')
    a1=axis('y',location='left',coord_transform=[0,mm],target=im,title='Distance (Mm)',tickFONT_NAME='Helvetica')
	a2=axis('x',location='top',coord_transform=[0,mm],showtext=0,ticklayout=1,target=im)
    a=axis('x',location='bottom',coord_transform=[0,mm],target=im,title='Distance (Mm)',tickFONT_NAME='Helvetica') 
    a3=axis('y',location='right',coord_transform=[0,mm],target=im,showtext=0)
	
    im4=image(-9000>total(vca_fix[100:880,200:960,0:5],3)<9000,rgb_table=70,layout=[2,2,4],axis_style=4,title='Doppler Velocity',font_name='Helvetica',/curr)
	a=axis('x',location='bottom',coord_transform=[0,mm],target=im4,title='Distance (Mm)',tickFONT_NAME='Helvetica') 
	a1=axis('y',location='left',coord_transform=[0,mm],target=im4,showtext=0)
	a2=axis('x',location='top',coord_transform=[0,mm],showtext=0,ticklayout=1,target=im4)	
	a3=axis('y',location='right',coord_transform=[0,mm],showtext=0,ticklayout=1,target=im4)

    im5=image(caprmin_fix[100:880,200:960,0],axis_style=4, layout=[2,2,1], title='Intensity',font_name='Helvetica',/curr)
	a=axis('x',location='bottom',coord_transform=[0,mm],target=im5,showtext=0) 
	a1=axis('y',location='left',coord_transform=[0,mm],target=im5,title='Distance (Mm)',tickFONT_NAME='Helvetica')
	a2=axis('x',location='top',coord_transform=[0,mm],showtext=0,ticklayout=1,target=im5)
	a3=axis('y',location='right',coord_transform=[0,mm],showtext=0,ticklayout=1,target=im5)

	im2=image(ph2[100:880,200:960,0],axis_style=4,layout=[2,2,2], title='Intensity wing (-0.942 $\AA$)',font_name='Helvetica',/curr)
	
	a=axis('x',location='bottom',coord_transform=[0,tt],showtext=0,title='Time (s)',tickFONT_NAME='Helvetica') 
	a1=axis('y',location='left',coord_transform=[0,km],showtext=0),title='Distance (km)',tickFONT_NAME='Helvetica'
	a2=axis('x',location='top',coord_transform=[0,tt],showtext=0,ticklayout=1)
	a3=axis('y',location='right',coord_transform=[0,km],showtext=0,ticklayout=1)