pro twod_pwr_spec, data,col
sz=size(data)
f=fft(data-mean(data),dimension=3)
Mm = (0.725/16.981891892)

n=sz(3)
x=FINDGEN((n - 1)/2) + 1
dt=30.
;making freq
IF evod(n) EQ 0 THEN freq = [ X, N/2]/(N*dt) ELSE freq = [X]/(N*dt)
;dont want zeroth freq so lost first 1
;freq=freq[1:*]
f2=f[*,*,1:n_elements(freq)]
;for i = 1, 40 do im=image(abs(f[*,*,i])^0.3,/current,layout=[7,6,i],rgb_table=11)
freq_pkpwr=fltarr(sz(1),sz(2))

FOR j=0, sz(2) -1 DO BEGIN
FOR i=0, sz(1) -1 DO BEGIN
mxpwr = max(reform(f2[i,j,*]),in)
freq_pkpwr[i,j]=freq[in]
;plot,freq,reform(f2[i,j,*])
;plots,freq[in],mxpwr,psym=2
;pause
ENDFOR
ENDFOR

im=image(freq_pkpwr)
;ax=axis('x',location='bottom',coord_transform=[0,mm],title='Distance (Mm)',tickFONT_NAME='Helvetica')
;ax1=axis('y',location='left',coord_transform=[0,mm],title='Distance (Mm)',tickFONT_NAME='Helvetica')
;ax2=axis('x',location='top',coord_transform=[0,mm],showtext=0,ticklayout=1)
;ax3=axis('y',location='right',coord_transform=[0,mm],showtext=0,ticklayout=1)
;a=OBJ_NEW('Colorbar', orientation=1,/border,range=(freq_pkpwr^0.3))


END