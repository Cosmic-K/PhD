
props={dimension:[800,800],marg:[0.2,0.01,0.01,0.01],FONT_NAME:'Helvetica'}
Mm = (0.725/16.981891892)

;p=image(core[*,*,2],_EXTRA=props,layout=[3,1,1],title='H$\alpha$ Core (intensity)')
;axis('x',location='bottom',coord_transform=[0,mm],title='Distance (Mm)',tickFONT_NAME='Helvetica')
;axis('y',location='left',coord_transform=[0,mm],title='Distance (Mm)',tickFONT_NAME='Helvetica')
;axis('x',location='top',coord_transform=[0,mm],showtext=0,ticklayout=1)
;axis('y',location='right',coord_transform=[0,mm],showtext=0,ticklayout=1)

;p1=image(wing_ha[*,*,2],_EXTRA=props,layout=[3,1,2],/curr,title='H$\alpha$ -0.906 $\AA$ (intensity)')
;p2=image((gauss_smooth(b_cog,2,/edge_truncate)<200.>(-200.)),_EXTRA=props,layout=[3,1,3],/curr,title='LOS Magnetogram (Gauss) Fe 6302 ')
;c=contour(b_cog>200.,n_levels=10,_EXTRA=props,layout=[3,1,3],/curr,/overplot,color='red')
;c=contour(b_cog<(-200.),n_levels=10,_EXTRA=props,layout=[3,1,3],/curr,/overplot,color='blue')
;a=OBJ_NEW('Colorbar', orientation=1,/border,range=[-200,200])


p=image(alog(rms_noise),_EXTRA=props,layout=[2,2,1],title='Estimated Noise (log intensity)')
a=OBJ_NEW('Colorbar', orientation=1,/border,range=[alog(rms_noise)])
axis('x',location='bottom',coord_transform=[0,mm],title='Distance (Mm)',tickFONT_NAME='Helvetica',target=p)
axis('y',location='left',coord_transform=[0,mm],title='Distance (Mm)',tickFONT_NAME='Helvetica',target=p)
axis('x',location='top',coord_transform=[0,mm],showtext=0,ticklayout=1,target=p)
axis('y',location='right',coord_transform=[0,mm],showtext=0,ticklayout=1,target=p)

p1=errorplot(alog(xk[1:*]),yk[1:*],ek[1:*],_EXTRA=props,layout=[2,2,2],/curr,ytitle='No.events',xtitle='log(RMS noise)')
p2=plot(alog(xk[1:*]),fk,_EXTRA=props,layout=[2,2,2],/overplot,/curr,color='red')
p6=plot(make_array(80,value=ck),findgen(80),color='red',linestyle=2,/curr,/over,layout=[2,2,2])


p5=errorplot(xb(findgen(767)*5)+1200,(mean_av(findgen(767)*5)),av_er(findgen(767)*5),_EXTRA=props,layout=[2,2,3],/curr,xtitle='Average intensity',ytitle='log(Mean RMS noise)')
p4=plot(xb(use)+1200,(fit2),_EXTRA=props,/over,layout=[2,2,3],/curr,color='red')

s=size(hd2)

im=image((-1*hd2)<20,(findgen(s(1))*bsx)+1200,findgen(s(2)),aspect=60,_EXTRA=props,layout=[2,2,4],/curr)
p9=plot(xb(use)+1200,exp(mean_av(use))/bsy,/over,layout=[2,2,4],/curr,color='red',_EXTRA=props)

axis('x',location='bottom',title='Average Intensity',tickFONT_NAME='Helvetica',target=im)
axis('y',location='left',coord_transform=[0,bsy],title='RMS Noise',tickFONT_NAME='Helvetica',target=im)
axis('y',location='right',showtext=0,ticklayout=1,target=im)
axis('x',location='top',showtext=0,ticklayout=1,target=im)


