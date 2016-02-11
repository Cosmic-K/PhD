pro amp_hist,x
openr,lun,'ap_list2.txt',/get_lun
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
indx=[19,12,5,4,0,11,0,0,0,0,0,4,0,0,0,0,4,0,0,0,0,9,0,15,0,0,0,7]

FOR j=indx[i-1],30 DO BEGIN

f=s+'_'+strtrim(j,1)+'.idl'
file_ch=file_test(f)



IF file_ch EQ 1 THEN BEGIN
restore,f
var=threads_fit_fg.fit_result_pos
var=var[*,1:1]

IF n_elements(var) EQ 18 THEN BEGIN
;print,'boo'
dof=(var(12)-var(11))-5
chi=var(10)
redchi=float(chi)/float(dof)
ENDIF

IF n_elements(var) EQ 19 THEN BEGIN
;print,'poo'
DOF=var(11)
chi=var(10)
redchi=float(chi)/float(dof)
ENDIF

ad=[temporary(ad),abs(var(1))]
pd=[temporary(pd),abs(var(2))]

ade=[temporary(ade),(sqrt((var(6)^2)*redchi))]


IF abs(var(7)) EQ 0 THEN pde=[temporary(pde),(sqrt((1)*redchi))] ELSE pde=[temporary(pde),(sqrt((var(7)^2)*redchi))];

ENDIF ELSE BEGIN
BREAK
ENDELSE
ENDFOR

ad=ad[1:*]
pd=pd[1:*]
ade=ade[1:*]
pde=pde[1:*]
;in=where(pd gt 10)
;pd=pd[in]
;pde=pde[in]


weighted_amp=[temporary(weighted_amp),total(abs(ad)/(ade^2))/total(1/(ade^2))]

weighted_per=[temporary(weighted_per),total(abs(pd)/(pde^2))/total(1/(pde^2))]


ENDFOR


weighted_amp=weighted_amp[1:*]*km
weighted_per=weighted_per[1:*]*1.343

vel_amp=weighted_amp*((2*!PI)/weighted_per)

cgwindow
cghistoplot,weighted_amp,binsize=12,xtitle='Amplitudes (km)',ytitle='Density',output='ps',/fill,/window,charsize=1.5,charthick=1,xthick=1.7,ythick=1.7,thick=1,polycolor='sea green',datacolorname='white'
cgwindow
cghistoplot,weighted_per,binsize=20,xtitle='Period (s)',ytitle='Density',output='ps',/window,charsize=1.5,charthick=1,xthick=1.7,ythick=1.7,thick=1,/fill,polycolor='indian red',datacolorname='white'
cgwindow
cghistoplot,vel_amp,binsize=0.9,xtitle='Velocity Amplitude (km/s)',ytitle='Density',output='ps',/window,charsize=1.5,charthick=1,xthick=1.7,ythick=1.7,thick=1,/fill,polycolor='sky blue',datacolorname='white'

cd,'../../../../images/grad'

d=read_table('grad_feat_redchi.txt')
cgwindow
cghistoplot,d[0,*],binsize=150,xtitle='Phase Speed (km/s)',ytitle='Density',output='ps',/fill,/window,charsize=1.5,charthick=1,xthick=1.7,ythick=1.7,thick=1,polycolor='Gold',datacolorname='white'


END