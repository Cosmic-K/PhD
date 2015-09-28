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
km = (0.725/16.981891892)*1000
time=1.343

weighted_amp=0
weighted_per=0


FOR i=1,(n_elements(array)-1) DO BEGIN
ad=0;amps
ade=0;amp error
pd=0;per
pde=0; per error
cd,array(i)
s=strsplit(array(i)'/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/measured/,/extract)
FOR j=0,40 DO BEGIN
s=s+'_'+strtrim(j,1)+'.idl'
file_ch=file_test(s)
IF file_ch EQ 1 THEN BEGIN
restore,'s'
var=threads_fit_fg.fit_result_pos
var=var[*,1:*]
ad=[tempoary(ad),var(0)]
pd=[tempoary(pd),var(1)]
ade=[tempoary(ade),var(5)]
pde=[tempoary(pde),var(6)]
ENDIF ELSE BEGIN
BREAK
ENDELSE
ENDFOR
;weighted mean
weighted_amp=[temporary(weighted_amp),total(abs(ad)/(ade^2))/total(1/(de^2))]
weighted_per=[temporary(weighted_per),total(abs(pd)/(pde^2))/total(1/(de^2))]
;need two of these one for amp one for period

ENDFOR


weighted_amp=weighted_amp[1:*]*km
weighted_per=weighted_per[1:*]*time

cghistoplot,weighted_amp,binsize=50,xtitle='Amplitudes (km)',ytitle='Density',output='ps',/fill,/window
cghistoplot,weighted_per,binsize=50,xtitle='Period (s)',ytitle='Density',output='ps',/fill,/window
;two hists

END