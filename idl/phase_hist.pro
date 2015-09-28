pro phase_hist,x
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
ph_list=0


FOR i=1,(n_elements(array)-1) DO BEGIN
data=read_table(array(i))
phase_values=data[0,*]
phase_error=data[1,*]
sz=size(data)

d=phase_values[(where(abs(phase_values) LT 800 AND phase_values NE 0))]
de=phase_error[(where(abs(phase_values) LT 800 AND phase_values NE 0))]

;total phase
ph_list=[temporary(ph_list),abs(d)]

;weighted mean
weighted_ph=[temporary(weighted_ph),total(abs(d)/(de^2))/total(1/(de^2))]


ENDFOR

ph_list=ph_list[1:*]
weighted_ph=weighted_ph[1:*]

;openw,1,'phase_speed_values'
;PRINTF,1,ph_list,FORMAT='(2F)'
;close,1

print,min(ph_list)
print, max(ph_list[where(ph_list lt 200)])

;print, min(weighted_ph)
;print,max()


a=where(ph_list lt 101,count)
a=where(ph_list lt 201 and ph_list gt 100,count1)
a=where(ph_list lt 301 and ph_list gt 200,count2)
a=where(ph_list lt 401 and ph_list gt 300,count3)
a=where(ph_list lt 501 and ph_list gt 400,count4)
a=where(ph_list lt 601 and ph_list gt 500,count5)
a=where(ph_list lt 701 and ph_list gt 600,count6)
a=where(ph_list lt 801 and ph_list gt 700,count7)

print,count
print,count1
print,count2
print,count3
print,count4
print,count5
print,count6
print,count7

;cghistoplot,ph_list,xtitle='Phase speed (km/s)',ytitle='Density',/window,output='ps',/fill
cghistoplot,weighted_ph,binsize=50,xtitle='Phase speed (km/s)',ytitle='Density',output='ps',/fill,/window


END