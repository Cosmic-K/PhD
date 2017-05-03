PRO ss_amp_hist,x

in_path='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/ss_kinkwmea_ca_sam/unsharptest/un10/res/'

	FOR i=0, 17 DO BEGIN
		restore,in_path+strtrim(i,1)+'/amps.idl'
		a=scope_varfetch('all_amps')
		IF n_elements(amp) eq 0 THEN amp=all_amps ELSE amp=[temporary(amp),all_amps]
	ENDFOR

plothist,amp,xhist,yhist,/autobin,/noplot
bp=barplot(xhist,yhist,xtitle='Displacement amplitude (km)',ytitle='Density',histogram=1,color='black',$
fill_color='light blue',font_name='Helvetica')

med=median(amp)
m=moment(amp,mdev=md,sdev=sd)
print,m[0]
print,med
print,md
print,sd
print,total(abs(amp-median(amp)))/n_elements(amp)
p=plot(make_array(140,value=md),findgen(140),linestyle=2,color='red',/curr,/over)

END
