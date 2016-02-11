pro phase_seis,x

Mm = (0.725/16.981891892)
dx=2.*Mm*1000.
dt=1.343

;

openr,lun,'lag_list.txt',/get_lun
array = ''
line = ''
WHILE NOT EOF(lun) DO BEGIN & $
READF, lun, line & $
array = [array, line] & $
ENDWHILE
FREE_LUN, lun

;For some reason the array starts from index 1

FOR i=1,(n_elements(array)-1) DO BEGIN
data=read_table(array[i])
sz=size(data)
;undoing incorrect maths in cc

lags=1/(data[0,0:sz(2)-3]/(dx/dt))

lag_er = lags*((data[1,0:sz(2)-3])/(data[0,0:sz(2)-3]))

lag_zero = abs(lags-lags[0])

lag_zero_er = sqrt(lag_er^2+lag_er[0]^2)

ph_val = (findgen(n_elements(lags))*(dx/dt))/lag_zero

ph_val_er = ph_val*(lag_zero_er/lag_zero)


set_plot,'ps'
device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/ph_height/h_vs_ph'+strtrim(i,1)+'.ps'
h=findgen(n_elements(ph_val))*Mm
plot,h,abs(ph_val),xtitle='Height along structure (Mm)',ytitle='Phase speed (km/s)'
errplot,h,abs(ph_val)-ph_val_er,abs(ph_val)+ph_val_er,thick=2

;!Y.OMargin = [2, 8]
;!X.OMargin = [2, 6]
;!P.Charsize=0.60

device,/close


openw,1,'/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/ph_height/phase_speed_values'+strtrim(i,1)+'.txt',/append
PRINTF,1,transpose([[ph_val],[ph_val_er]]),FORMAT='(2F)'
close,1


ENDFOR


END