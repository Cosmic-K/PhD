PRO ss_amp_hist,file
km = (0.725/16.981891892)*1000.0

openr,lun,file,/get_lun
array = ''
line = ''
WHILE NOT EOF(lun) DO BEGIN & $
READF, lun, line & $
array = [array, line] & $
ENDWHILE
FREE_LUN, lun
col=['blue','forest green','chocolate','pink','orange','gold','firebrick','indigo','cyan','dark red','red']

in_path='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/ss_kinkwmea_ca_sam/measured/'

	FOR i=6, n_elements(array)-1 DO BEGIN
		restore,in_path+strtrim(array[i],1)+'/amps.idl'
		a=scope_varfetch('all_amps')
		x=findgen(n_elements(all_amps))*km
		in=where(all_amps[*,0] ne 0)
		p=errorplot(x[in],all_amps[in,0]+i*150,all_amps[in,1],xtitle='Distance (km)', $
		ytitle='Displacement amplitude (km)',yrange=[900,1700],font_name='Helvetica',color=col[i],errorbar_color=col[i],/curr,/over)
		;IF n_elements(amp) eq 0 THEN amp = total(abs(all_amps[in,0])/(all_amps[in,1]^2))/total(1/(all_amps[in,1]^2))$
		;ELSE amp=[temporary(amp),total(abs(all_amps[in,0])/(all_amps[in,1]^2))/total(1/(all_amps[in,1]^2))]
	
	ENDFOR
;plothist,amp,xhist,yhist,bin=20,/noplot
;bp=barplot(xhist,yhist,xtitle='Displacement amplitude (km)',ytitle='Density',histogram=1,color='black',$
;fill_color='light blue',font_name='Helvetica')

;med=median(amp)
;m=moment(amp,mdev=md,sdev=sd)
;print,m[0]
;print,med
;print,md
;print,sd
;print,total(abs(amp-median(amp)))/n_elements(amp)
;p=plot(make_array(4,value=med),findgen(4),linestyle=2,color='red',/curr,/over)

END
