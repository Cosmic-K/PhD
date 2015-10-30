pro phase_hist2,x

Mm = (0.725/16.981891892)
dx=2.*Mm*1000.
dt=1.343

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

phase_values=1/(data[0,0:sz(2)-3]/(dx/dt))

phase_errors=phase_values*((data[1,0:sz(2)-3])/((data[0,0:sz(2)-3]/(dx/dt))*(dx/dt)))

;in=(where(abs(phase_values) LT 11))


s2=size(phase_values)

;Working out the middle to apply correct dx value between lags
mid=((sz(2))-3)/2


shift_pv=shift(phase_values,1)

d_lag1=shift_pv[1:(mid-1)]-phase_values[1:(mid-1)]

d_lag2=shift_pv[(mid+2):*]-phase_values[(mid+2):*]

d_lag_tot=[d_lag1,phase_values[mid-1],phase_values[mid+1],d_lag2]

ph_val=dx/dt*(1./d_lag_tot)


spve=shift(phase_errors,1)

d_lag_er1=sqrt(phase_errors[1:(mid-1)]^2+spve[1:(mid-1)]^2)
d_lag_er2=sqrt(phase_errors[1:(mid+2)]^2+spve[1:(mid+2)]^2)

dlagerror=[d_lag_er1,phase_errors[mid-1],phase_errors[mid+1],d_lag_er2]

ph_val_er=ph_val*(dlagerror/d_lag_tot)


;some plotting against height

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