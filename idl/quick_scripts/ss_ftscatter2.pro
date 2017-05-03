PRO ss_ftscatter2,list,velc=velc,pow=pow,chpow=chpow
;frequencies for vel and int dont match should they? 
;why no same? 
;there are stops inputted and the the freq is in hertz not in mhertz and xlogs not on 
;think the freq is messed because of units, so email micah to check unit outpu can prob make my own freq in mean time 

dt=30.
km = (0.725/16.981891892)*1000.
avbinn=fltarr(17)
avbinn_er=fltarr(17)

in_path='/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/ss_kinkwmea_ca_sam/measured/'

openr,lun,list,/get_lun
array = ''
line = ''
WHILE NOT EOF(lun) DO BEGIN & $
READF, lun, line & $
array = [array, line] & $
ENDWHILE
FREE_LUN, lun


	FOR i=1, n_elements(array)-1 DO BEGIN
		
		file=file_search(in_path+array[i]+'/nuwt_results_all_slits_rad*')

		RESTORE_NUWT_COMMON_DATA,file,located_out=located_out, threads_out=threads_out, fft_results_out=fft_results_out, fft_stats_out=fft_stats_out
		COMMON all_nuwt_dat, nuwt_located, nuwt_threads, nuwt_fft_results, nuwt_fft_stats
		slit_num=n_elements(nuwt_threads)
		slit_located = nuwt_threads[0]
		threds = slit_located[0]
		pos = threds.pos
		pos=pos[where(pos ne -1)]
		pos=pos*km	

		N=n_elements(pos)

		time=findgen(n)*dt
		
		IF keyword_set(velc) THEN BEGIN
			vpos=deriv(time,pos)
			signal=fft(vpos-mean(vpos),dimension=1)
			titley ='Spectral power density [$km^2$ $s^{-2}$ $Hz^{-1}$]'
		ENDIF ELSE BEGIN
			signal=fft(pos-mean(pos),dimension=1)
			titley = 'Spectral power density [$km^2$ $Hz^{-1}$]'
		ENDELSE
			

		x=FINDGEN((N - 1)/2) + 1
		
		;making freq full freq now only need half for ft plotting
		IF evod(n) EQ 0 THEN freq = [0.0, X, N/2.,-N/2 + X]/(N*dt) ELSE freq = [0.0, X,-(N/2 + 1) + X]/(N*dt)

		nf=n_elements(freq)
		signal=signal[1:floor(nf/2.)]
		freq=freq[1:floor(nf/2.)]

		IF n_elements(signals) eq 0 THEN signals=alog10((2*dt*n)*abs(signal)^2) ELSE signals=[temporary(signals),alog10((2*dt*n)*abs(signal)^2)]
		IF n_elements(freqs) eq 0 THEN freqs=freq ELSE freqs=[temporary(freqs),freq]

		p=plot(freq*1000.,(2*dt*n)*abs(signal)^2,/current,/over,/xlog,/ylog,linestyle=6,symbol='*',$
		FONT_NAME='Helvetica',ytitle=titley,xtitle='Frequency [mHz]',xrange=[0,25]) 

	ENDFOR
	
	av1=signals[where(freqs lt (0.001))]
	av2=signals[where((freqs gt (0.001)) and (freqs LT (0.002)))] 
	av3=signals[where((freqs gt (0.002)) and (freqs LT (0.003)))]
	av4=signals[where((freqs gt (0.003)) and (freqs LT (0.004)))]
	av5=signals[where((freqs gt (0.004)) and (freqs LT (0.005)))]
	av6=signals[where((freqs gt (0.005)) and (freqs LT (0.006)))]
	av7=signals[where((freqs gt (0.006)) and (freqs LT (0.007)))]
	av8=signals[where((freqs gt (0.007)) and (freqs LT (0.008)))]
	av9=signals[where((freqs gt (0.008)) and (freqs LT (0.009)))]
	av10=signals[where((freqs gt (0.009)) and (freqs LT (0.010)))]
	av11=signals[where((freqs gt (0.010)) and (freqs LT (0.011)))]
	av12=signals[where((freqs gt (0.011)) and (freqs LT (0.012)))]
	av13=signals[where((freqs gt (0.012)) and (freqs LT (0.013)))]
	av14=signals[where((freqs gt (0.013)) and (freqs LT (0.014)))]
	av15=signals[where((freqs gt (0.014)) and (freqs LT (0.015)))]
	av16=signals[where((freqs gt (0.015)) and (freqs LT (0.016)))]
	av17=signals[where((freqs gt (0.016)) and (freqs LT (0.017)))]

	FOR i=1,17 DO BEGIN 

		av=scope_varfetch('av'+strtrim(i,1))
		av=av[where(av ne 0)]
			                 
		nb=n_elements(av)

		indx=floor(nb*randomu(seed,nb,500))

		m=moment(mean(av[indx],dimension=1))

		avbinn[i-1]=10^(mean(av))
		avbinn_er[i-1]=2.303*(10^(mean(av)))*sqrt(m[1])

	ENDFOR
	
	f=[0.0005,((findgen(16)+1)*0.001)+0.0005]

	p7=plot(freqs[sort(freqs)]*1000.,gauss_smooth(10^(signals[sort(freqs)]),12,/edge_truncate),$
	color='gold',/over,/curr,/ylog,/xlog,thick=2)

	e=errorplot(f*1000.,avbinn,avbinn_er,linestyle=6,symbol='*',FONT_NAME='Helvetica',$
	title='Chromospheric power spectrum',/curr,/over,/xlog,color='red',errorbar_color='red',$
	sym_thick=1.5,/ylog,name='Active Ca II 8542 $\AA$')
	
	IF (NOT keyword_set(velc)) and (n_elements(chpow) NE 0) THEN BEGIN
	restore,chpow
	fq=scope_varfetch('f')
	hachromo=scope_varfetch('av')
	haer=scope_varfetch('aver')
	hach=errorplot(fq*1000.,hachromo,haer,linestyle=6,symbol='d',color='purple',sym_thick=1.5,ERRORBAR_color='purple',/curr,/over,/ylog,/xlog)
	ENDIF

	IF keyword_set(velc) THEN BEGIN
	restore,pow
	restore,chpow
	fq=scope_varfetch('f')
	hachromo=scope_varfetch('av')
	haer=scope_varfetch('aver')
	v=scope_varfetch('V')
	phot=scope_varfetch('MEAN_POWER_PHOTOSPHERE')
	chromo=scope_varfetch('MEAN_POWER_CHROMOSPHERE')
	erphoto=scope_varfetch('ERR_PHOTO')
	erchromo=scope_varfetch('ERR_CHROMO')
	

	hach=errorplot(fq*1000.,hachromo,haer,linestyle=6,symbol='d',color='red',sym_thick=1.5,ERRORBAR_color='red',/curr,/over,/ylog,/xlog,name='Quiet H-$\alpha$')
	ph=errorplot(v,phot,erphoto,linestyle=6,symbol='tu',color='green',sym_thick=1.5,ERRORBAR_color='green',/curr,/over,/ylog,/xlog,name='Quiet Stangalini 2015 Fe I 6300 $\AA$')
	ch=errorplot(v,chromo,erchromo,linestyle=6,symbol='td',color='blue',sym_thick=1.5,ERRORBAR_color='blue',/curr,/over,/ylog,/xlog,name=' Quiet Stangalini 2015 Ca II 3969 $\AA$')
	
	leg=legend(target=[e,hach,ph,ch],/data,/auto_text_color,HORIZONTAL_SPACING=0.045,font_size=8,FONT_NAME='Helvetica')

	ENDIF



END