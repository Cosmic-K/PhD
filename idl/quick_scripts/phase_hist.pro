pro phase_hist,x

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
weighted_ph=0
tot=0

FOR i=1,(n_elements(array)-1) DO BEGIN
data=read_table(array[i])
sz=size(data)
;undoing incorrect maths in cc

phase_values=data[0,0:sz(2)-3]/(dx/dt)
phase_errors=data[1,0:sz(2)-3]/(dx/dt)

in=(where(abs(phase_values) LT 11))
;phase_errors=phase_error[(where(abs(phase_value) LT 10))]

;print,phase_values
s2=size(phase_values)

;Working out the middle to apply correct dx value between lags
middle=((sz(2))-3)/2
d=0
de=0


FOR j=0,sz(2)-3 DO BEGIN
IF j lt middle THEN BEGIN
d=[temporary(d),phase_values(j)*(middle-j)*dx/dt]
de=[temporary(de),phase_errors(j)*(middle-j)*dx/dt]
ENDIF ELSE BEGIN
d=[temporary(d),phase_values(j)*(j-middle)*dx/dt]
de=[temporary(de),phase_errors(j)*(j-middle)*dx/dt]
ENDELSE
ENDFOR

d=d[1:*]
de=de[1:*]

IF total(in) NE -1 THEN BEGIN
d=d[in]
de=de[in]
ENDIF

dd=d[(where(abs(d) NE 0))]
dde=de[(where(abs(d) NE 0))]


;openw,1,'phase_speed_values.txt',/append
;PRINTF,1,transpose([[dd],[dde]]),FORMAT='(2F)'
;close,1

;weighted mean
weighted_ph=[temporary(weighted_ph),total(abs(dd)/(dde^2))/total(1/(dde^2))]
tot=[temporary(tot),dd]
ENDFOR

weighted_ph=weighted_ph[1:*]
tot=tot[1:*]

cghistoplot,weighted_ph,binsize=100,xtitle='Phase speed (km/s)',ytitle='Density',output='ps',/fill,/window


END