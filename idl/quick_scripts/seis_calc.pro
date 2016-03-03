pro seis_calc,n

close,1
close,2
close,3


FOR i=21,21 DO BEGIN

f='amp'+strtrim(i,1)+'.txt'
amp_data=read_table(f)

amps = amp_data[0,*]
amp_er = amp_data[1,*]

amps_norm = amps/amps(0)

m=moment(amps)
m2=moment(amp_er)

shif_amp=shift(amps,1)
amp_diff=amps[1:*]-shif_amp[1:*]

m3=moment(amp_diff)

;print,amp_diff/amp_er
;print,m[1]
;print,amp_diff
;print,'...........................................'
;print,m3[1]
;print,'...........................................'


;IF rms(abs(amp_diff/amp_er)) lt 2 THEN BEGIN
IF m2[1] lt 11.0 THEN BEGIN

R=reform(amps_norm)
B=reform((1/amps_norm)^2)
rho=reform((1/amps_norm)^4)


R_er = reform(R*sqrt((amp_er/amps)^2+(amp_er(0)/amps(0))^2))
B_er = reform(B*sqrt(((2*amp_er/amps)^2+(2*amp_er(0)/amps(0))^2)))
rho_er = reform(rho*sqrt((4*amp_er/amps)^2+(4*amp_er(0)/amps(0))^2))



openw,1,'/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/seis/seis_nu/r_n/seis_r_'+strtrim(i,1)+'.txt',/append
PRINTF,1,transpose([[R],[R_er]]),FORMAT='(2F)'
close,1

openw,2,'/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/seis/seis_nu/b_n/seis_b_'+strtrim(i,1)+'.txt',/append
PRINTF,2,transpose([[B],[B_er]]),FORMAT='(2F)'
close,2

openw,3,'/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/seis/seis_nu/rho_n/seis_rho_'+strtrim(i,1)+'.txt',/append
PRINTF,3,transpose([[rho],[rho_er]]),FORMAT='(2F)'
close,3


ENDIF

ENDFOR


END