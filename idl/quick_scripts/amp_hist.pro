pro amp_hist,x
openr,lun,'ap_list.txt',/get_lun
array = ''
line = ''
WHILE NOT EOF(lun) DO BEGIN & $
READF, lun, line & $
array = [array, line] & $
ENDWHILE
FREE_LUN, lun

;For some reason the array starts from index 1
km = (0.725/16.981891892)*1000.0
time=1.343

weighted_amp=0
weighted_per=0

FOR i=1,(n_elements(array)-1) DO BEGIN
ad=0;amps
ade=0;amp error
pd=0;per
pde=0; per error
cd,array(i)
s=strsplit(array(i),'/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/measured/kinkwavemeasha/',/extract)
FOR j=0,40 DO BEGIN
f=s+'_'+strtrim(j,1)+'.idl'
file_ch=file_test(f)
;print,f
;print, file_ch
IF file_ch EQ 1 THEN BEGIN
restore,f
var=threads_fit_fg.fit_result_pos
var=var[*,1:*]
ad=[temporary(ad),abs(var(1))]
pd=[temporary(pd),var(2)]
ade=[temporary(ade),var(6)]
pde=[temporary(pde),var(7)]
ENDIF ELSE BEGIN
BREAK
ENDELSE
ENDFOR

;weighted mean
;print,ad
ad=ad[1:*]
pd=pd[1:*]
ade=ade[1:*]
pde=pde[1:*]

weighted_amp=[temporary(weighted_amp),total(abs(ad)/(ade^2))/total(1/(ade^2))]
weighted_per=[temporary(weighted_per),total(abs(pd)/(pde^2))/total(1/(pde^2))]

;need two of these one for amp one for period

ENDFOR
;print,weighted_amp
;print,weighted_per
weighted_amp=weighted_amp[1:*]*km
weighted_per=weighted_per[1:*]*1.343
vel_amp=weighted_amp*((2*!PI)/weighted_per)

;cghistoplot,weighted_amp,binsize=50,xtitle='Amplitudes (km)',ytitle='Density',output='ps',/fill,/window
;cgwindow
;cghistoplot,weighted_per,binsize=50,xtitle='Period (s)',ytitle='Density',output='ps',/fill,/window
;cgwindow
cghistoplot,vel_amp,binsize=1,xtitle='Velocity Amplitude (km/s)',ytitle='Density',output='ps',/fill,/window

END