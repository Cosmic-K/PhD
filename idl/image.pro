pro image,ts,tsdop,tsrb


mask= ts lt -35

sz=size(ts)
ysz=float(sz(2))
xsz=float(sz(1))

Mm=(0.725/16.981891892)

xrmax=((xsz-1.0)*1.343)
yrmax=Mm*(ysz-1.0)

loadct,0
set_plot, 'x'


set_plot,'ps'
device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/data/ims/dop_362.eps'

tvim,tsdop,aspect=15,range=[-200,200],/noaxis
loadct,0,/silent
cgtext,0.52,0.58,'Doppler -.362 16.5 km/s',/normal,alignment=0.5,charsize=0.55
cgtext,0.1,0.54,'Mm',/normal,alignment=0.5,charsize=0.5,orientation=90
axis,0,xaxis=0,charsize=0.8,xrange=[0,xrmax],xst=1,xtitle='Seconds'
axis,0,yaxis=0,charsize=0.45,yrange=[0,yrmax],yst=1

device,/close

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/data/ims/dop_362_mask.eps'

tvim,mask*tsdop,aspect=15,range=[-200,200],/noaxis
loadct,0,/silent
cgtext,0.52,0.58,'Doppler Mask -.362 16.5 km/s',/normal,alignment=0.5,charsize=0.55
cgtext,0.1,0.54,'Mm',/normal,alignment=0.5,charsize=0.5,orientation=90
axis,0,xaxis=0,charsize=0.8,xrange=[0,xrmax],xst=1,xtitle='Seconds'
axis,0,yaxis=0,charsize=0.45,yrange=[0,yrmax],yst=1

device,/close

device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/data/ims/rdbl_362.eps'

tvim,tsrb,aspect=15,range=[-200,200],/noaxis
loadct,0,/silent
cgtext,0.52,0.58,'Red + Blue -.362 16.5 km/s',/normal,alignment=0.5,charsize=0.55
cgtext,0.1,0.54,'Mm',/normal,alignment=0.5,charsize=0.5,orientation=90
axis,0,xaxis=0,charsize=0.8,xrange=[0,xrmax],xst=1,xtitle='Seconds'
axis,0,yaxis=0,charsize=0.45,yrange=[0,yrmax],yst=1

device,/close

set_plot,'x'

end