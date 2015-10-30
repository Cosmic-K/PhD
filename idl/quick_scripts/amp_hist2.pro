pro amp_hist2,x
openr,lun,'ap_list.txt',/get_lun
array = ''
line = ''
WHILE NOT EOF(lun) DO BEGIN & $
READF, lun, line & $
array = [array, line] & $
ENDWHILE
FREE_LUN, lun

;For some reason the array starts from index 1
Mm = (0.725/16.981891892)
km=Mm*1000.0
time=1.343


FOR i=1,(n_elements(array)-1) DO BEGIN
ad=0;amps
ade=0;amp error
cd,array(i)
s=strsplit(array(i),'/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/measured/kinkwavemeasha/',/extract)
FOR j=0,40 DO BEGIN
f=s+'_'+strtrim(j,1)+'.idl'
file_ch=file_test(f)

IF file_ch EQ 1 THEN BEGIN
restore,f
var=threads_fit_fg.fit_result_pos
var=var[*,1:*]

ad=[temporary(ad),abs(var(1))]
ade=[temporary(ade),var(6)]

ENDIF ELSE BEGIN
BREAK
ENDELSE
ENDFOR

ad=ad[1:*]
ade=ade[1:*]

set_plot,'ps'
device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/images/amp_height/h_vs_amp'+strtrim(i,1)+'.ps'
h=findgen(n_elements(ad))*Mm
plot,h,abs(ad)*km,xtitle='Height along structure (Mm)',ytitle='Displacement amplitude (km)'
errplot,h,abs(ad)*km-ade*km,abs(ad)*km+ade*km,thick=2

;!Y.OMargin = [2, 8]
;!X.OMargin = [2, 6]
;!P.Charsize=0.60

device,/close

openw,1,'/Users/krishnamooroogen/Documents/PHYSICS/PhD/images/amp_height/amp'+strtrim(i,1)+'.txt',/append
PRINTF,1,transpose([[ad],[ade]]),FORMAT='(2F)'
close,1

ENDFOR
END