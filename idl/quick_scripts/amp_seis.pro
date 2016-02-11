pro amp_seis,x
openr,lun,'ap_list2.txt',/get_lun
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


FOR i=4,4 do begin; (n_elements(array)-1) DO BEGIN

ad=0;amps
ade=0;amp error
ch=0
dofl=0

cd,array(i)
s=strsplit(array(i),'/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/measured/kinkwavemeasha/',/extract)
indx=[19,12,5,4,0,11,0,0,0,0,0,4,0,0,0,0,4,0,0,0,0,9,0,15,0,0,0,7]
FOR j=indx[3], 11 DO BEGIN
;print,i
;print,j
f=s+'_'+strtrim(j,1)+'.idl'
file_ch=file_test(f)
;print,f
;print,file_ch




IF file_ch EQ 1 THEN BEGIN
restore,f
var=threads_fit_fg.fit_result_pos
var=var[*,1:1]
print,var
print, n_elements(var)

IF n_elements(var) EQ 18 THEN BEGIN
print,'boo'
DOF=(var(12)-var(11))-5
chi=var(10)
redchi=float(chi)/float(dof)
ENDIF

IF n_elements(var) EQ 19 THEN BEGIN
print,'poo'
DOF=var(11)
chi=var(10)
redchi=float(chi)/float(dof)
ENDIF

ad=[temporary(ad),abs(var(1))]
ade=[temporary(ade),(sqrt((var(6)^2)*redchi))]
ch=[temporary(ch),chi]
dofl=[temporary(dofl),DOF]

ENDIF ELSE BEGIN
BREAK
ENDELSE
ENDFOR

ad=ad[1:*]
ade=ade[1:*]
ch=ch[1:*]
dofl=dofl[1:*]

;!
ad=reverse(ad)
ade=reverse(ade)
;!


set_plot,'ps'



device,/encapsul,/color,filename='/Users/krishnamooroogen/Documents/PHYSICS/PhD/images/amp_height/h_vs_amp'+strtrim(i,1)+'.ps'
h=findgen(n_elements(ad))*Mm
plot,h,abs(ad)*km,xtitle='Height along structure (Mm)',ytitle='Displacement amplitude (km)'
errplot,h,abs(ad)*km-ade*km,abs(ad)*km+ade*km,thick=2

device,/close
set_plot,'x'

openw,1,'/Users/krishnamooroogen/Documents/PHYSICS/PhD/images/amp_height/amp'+strtrim(i,1)+'.txt',/append
PRINTF,1,transpose([[ad*km],[ade*km]]),FORMAT='(2F)'
close,1

ENDFOR
END