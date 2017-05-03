pro table,x 
close,1
w_amp=fltarr(2,28)
w_per=fltarr(2,28)
w_va=fltarr(2,28)

FOR i=1, 28 DO BEGIN	
amp=read_table('/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/amp_height/amp'+strtrim(i,1)+'.txt')
per=read_table('/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/p_height/p'+strtrim(i,1)+'.txt')
va=read_table('/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/va_height/va'+strtrim(i,1)+'.txt')


w_amp[0,i-1]=total(abs(amp[0,*])/(amp[1,*]^2))/total(1/(amp[1,*]^2))
w_amp[1,i-1]=sqrt(1./total(1/amp[1,*]^2))
w_per[0,i-1]=total(abs(per[0,*])/(per[1,*]^2))/total(1/(per[1,*]^2))
w_per[1,i-1]=sqrt(1./total(1/per[1,*]^2))
w_va[0,i-1]=total(abs(va[0,*])/(va[1,*]^2))/total(1/(va[1,*]^2))
w_va[1,i-1]=sqrt(1./total(1/va[1,*]^2))
ENDFOR 

openw,1,'/Users/krishnamooroogen/Documents/PHYSICS/PhD/images/w_props/w_a.txt',/append
PRINTF,1,w_amp,format='(2F10.2)'
close,1

openw,1,'/Users/krishnamooroogen/Documents/PHYSICS/PhD/images/w_props/w_p.txt',/append
PRINTF,1,w_per,format='(2F10.2)'
close,1

openw,1,'/Users/krishnamooroogen/Documents/PHYSICS/PhD/images/w_props/w_v.txt',/append
PRINTF,1,w_va,format='(2F10.2)'
close,1


END