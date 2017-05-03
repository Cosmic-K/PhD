PRO ss_ftscatter,file,velc=velc
;frequencies for vel and int dont match should they? 
;why no same? 
;there are stops inputted and the the freq is in hertz not in mhertz and xlogs not on 
;think the freq is messed because of units, so email micah to check unit outpu can prob make my

dt=30.
avbinn=fltarr(25)
avbinn_er=fltarr(25)

RESTORE_NUWT_COMMON_DATA,file,located_out=located_out, threads_out=threads_out, fft_results_out=fft_results_out, fft_stats_out=fft_stats_out
COMMON all_nuwt_dat, nuwt_located, nuwt_threads, nuwt_fft_results, nuwt_fft_stats
slit_num=n_elements(nuwt_threads)


IF keyword_set(velc) THEN BEGIN
	FOR i=0, slit_num-1,2 DO BEGIN

		slit_located = nuwt_threads[i]
		threds = slit_located[0]
		pos = threds.pos
		pos=pos[where(pos ne -1)]

		N=n_elements(pos)

		time=findgen(n)*dt
		vpos=deriv(time,pos)
		x=FINDGEN((N - 1)/2) + 1
		
		;making freq full freq now only need half for ft plotting
		IF evod(n) EQ 0 THEN freq = [0.0, X, N/2.,-N/2 + X]/(N*dt) ELSE freq = [0.0, X,-(N/2 + 1) + X]/(N*dt)

		signal=fft(vpos-mean(vpos),dimension=1)
		nf=n_elements(freq)
		signal=signal[1:floor(nf/2.)]
		freq=freq[1:floor(nf/2.)]


		IF n_elements(signals) eq 0 THEN signals=alog10((2*dt*n)*abs(signal)^2) ELSE signals=[temporary(signals),alog10((2*dt*n)*abs(signal)^2)]
		IF n_elements(freqs) eq 0 THEN freqs=freq ELSE freqs=[temporary(freqs),freq]

		p=plot(freq,(2*dt*n)*abs(signal)^2,/current,/over,/ylog,linestyle=6,symbol='*',$
		FONT_NAME='Helvetica',ytitle='Spectral power density [$km^2$ $s^{-2}$ $Hz^{-1}$]',xtitle='Frequency [mHz]');,xrange=[0.5,600],yrange=[10^(-7.),10^2])
	ENDFOR

	av1=signals[where(freqs lt (0.02))]
	av2=signals[where((freqs gt (0.02)) and (freqs LT (0.04)))] 
	av3=signals[where((freqs gt (0.04)) and (freqs LT (0.06)))]
	av4=signals[where((freqs gt (0.06)) and (freqs LT (0.08)))]
	av5=signals[where((freqs gt (0.08)) and (freqs LT (0.10)))]
	av6=signals[where((freqs gt (0.10)) and (freqs LT (0.12)))]
	av7=signals[where((freqs gt (0.12)) and (freqs LT (0.14)))]
	av8=signals[where((freqs gt (0.14)) and (freqs LT (0.16)))]
	av9=signals[where((freqs gt (0.16)) and (freqs LT (0.18)))]
	av10=signals[where((freqs gt (0.18)) and (freqs LT (0.20)))]
	av11=signals[where((freqs gt (0.20)) and (freqs LT (0.22)))]
	av12=signals[where((freqs gt (0.22)) and (freqs LT (0.24)))]
	av13=signals[where((freqs gt (0.24)) and (freqs LT (0.26)))]
	av14=signals[where((freqs gt (0.26)) and (freqs LT (0.28)))]
	av15=signals[where((freqs gt (0.28)) and (freqs LT (0.30)))]
	av16=signals[where((freqs gt (0.30)) and (freqs LT (0.32)))]
	av17=signals[where((freqs gt (0.32)) and (freqs LT (0.34)))]
	av18=signals[where((freqs gt (0.34)) and (freqs LT (0.36)))]
	av19=signals[where((freqs gt (0.36)) and (freqs LT (0.38)))]
	av20=signals[where((freqs gt (0.38)) and (freqs LT (0.40)))]
	av21=signals[where((freqs gt (0.40)) and (freqs LT (0.42)))]
	av22=signals[where((freqs gt (0.42)) and (freqs LT (0.44)))]
	av23=signals[where((freqs gt (0.44)) and (freqs LT (0.46)))]
	av24=signals[where((freqs gt (0.46)) and (freqs LT (0.48)))]
	av25=signals[where((freqs gt (0.48)) and (freqs LT (0.50)))]

	FOR i=1,25 DO BEGIN 

		av=scope_varfetch('av'+strtrim(i,1))
		av=av[where(av ne 0)]
			                 
		nb=n_elements(av)

		indx=floor(nb*randomu(seed,nb,500))

		m=moment(mean(av[indx],dimension=1))

		avbinn[i-1]=10^(mean(av))
		avbinn_er[i-1]=2.303*(10^(mean(av)))*sqrt(m[1])

	ENDFOR

	f=[((findgen(25)+1)*0.02)]

	;p7=plot(freqs[sort(freqs)]*1000.,gauss_smooth(10^(signals[sort(freqs)]),12,/edge_truncate),$
	;color='gold',/over,/curr,/ylog,thick=2)

	;e=errorplot(f*1000.,avbinn,avbinn_er,linestyle=6,symbol='*',FONT_NAME='Helvetica',$
	;title='Chromospheric power spectrum',/curr,/over,color='red',errorbar_color='red',$
	;sym_thick=1.5,/ylog)



ENDIF ELSE BEGIN

	FOR i=0, slit_num-1 DO BEGIN
		
		slit_fft_results=nuwt_fft_results[i]
		fft_res = slit_fft_results[0]
		pow=fft_res.power
		freq=fft_res.freq
		n=n_elements(pow)


		IF n_elements(signals) eq 0 THEN signals=alog10((2*dt*n)*abs(pow)^2) ELSE signals=[temporary(signals),alog10((2*dt*n)*abs(pow)^2)]
		IF n_elements(freqs) eq 0 THEN freqs=freq ELSE freqs=[temporary(freqs),freq]

		p=plot(freq,(2*dt*n)*abs(pow)^2,/current,/over,/ylog,linestyle=6,symbol='*',$
		FONT_NAME='Helvetica',ytitle='Spectral power density [$km^2$ $Hz^{-1}$]',xtitle='Frequency [mHz]');,xrange=[0.5,600],yrange=[10^(-7.),10^2])

		;UNITS IN AXIS NEED CHANGING IF NOT USING VELOCITY per second or not
	ENDFOR
		stop
		av1=signals[where(freqs lt (0.02))]
		av2=signals[where((freqs gt (0.02)) and (freqs LT (0.04)))] 
		av3=signals[where((freqs gt (0.04)) and (freqs LT (0.06)))]
		av4=signals[where((freqs gt (0.06)) and (freqs LT (0.08)))]
		av5=signals[where((freqs gt (0.08)) and (freqs LT (0.10)))]
		av6=signals[where((freqs gt (0.10)) and (freqs LT (0.12)))]
		av7=signals[where((freqs gt (0.12)) and (freqs LT (0.14)))]
		av8=signals[where((freqs gt (0.14)) and (freqs LT (0.16)))]
		av9=signals[where((freqs gt (0.16)) and (freqs LT (0.18)))]
		av10=signals[where((freqs gt (0.18)) and (freqs LT (0.20)))]
		av11=signals[where((freqs gt (0.20)) and (freqs LT (0.22)))]
		av12=signals[where((freqs gt (0.22)) and (freqs LT (0.24)))]
		av13=signals[where((freqs gt (0.24)) and (freqs LT (0.26)))]
		av14=signals[where((freqs gt (0.26)) and (freqs LT (0.28)))]
		av15=signals[where((freqs gt (0.28)) and (freqs LT (0.30)))]
		av16=signals[where((freqs gt (0.30)) and (freqs LT (0.32)))]
		av17=signals[where((freqs gt (0.32)) and (freqs LT (0.34)))]
		av18=signals[where((freqs gt (0.34)) and (freqs LT (0.36)))]
		av19=signals[where((freqs gt (0.36)) and (freqs LT (0.38)))]
		av20=signals[where((freqs gt (0.38)) and (freqs LT (0.40)))]
		av21=signals[where((freqs gt (0.40)) and (freqs LT (0.42)))]
		av22=signals[where((freqs gt (0.42)) and (freqs LT (0.44)))]
		av23=signals[where((freqs gt (0.44)) and (freqs LT (0.46)))]
		av24=signals[where((freqs gt (0.46)) and (freqs LT (0.48)))]
		av25=signals[where((freqs gt (0.48)) and (freqs LT (0.50)))]

		FOR i=1,25 DO BEGIN 
			av=scope_varfetch('av'+strtrim(i,1))
			av=av[where(av ne 0)]
			                 
			nb=n_elements(av)

			indx=floor(nb*randomu(seed,nb,500))

			m=moment(mean(av[indx],dimension=1))

			avbinn[i-1]=10^(mean(av))
			avbinn_er[i-1]=2.303*(10^(mean(av)))*sqrt(m[1])

		ENDFOR


		f=[((findgen(25)+1)*0.02)]

		p7=plot(freqs[sort(freqs)]*1000.,gauss_smooth(10^(signals[sort(freqs)]),12,/edge_truncate),$
		color='gold',/over,/curr,/xlog,/ylog,thick=2)

		e=errorplot(f*1000.,avbinn,avbinn_er,linestyle=6,symbol='*',FONT_NAME='Helvetica',$
		title='Chromospheric power spectrum',/curr,/over,color='red',errorbar_color='red',$
		sym_thick=1.5,/xlog,/ylog)

ENDELSE

END