;Krishna Mooroogen
;Fourier analysis script with bootstrap
;dimenisons can be 1 (by default) or 2 
;data 2 must be of same shape 
;keeps killing itslef

pro ftboot,data,errors,data2=data2,errors2=errors2,alignment_error=alignment_error,dt=dt

sz=size(data)

IF n_elements(sz) EQ 6 THEN BEGIN
	N=sz(3)
	dim=3
ENDIF

IF n_elements(sz) EQ 5 THEN BEGIN
	N=sz(2)
	dim=2
ENDIF

IF n_elements(sz) EQ 4 THEN BEGIN
	N=sz(1)
	dim=0
ENDIF

IF n_elements(nt) eq 0 THEN dt=30;1.343 ; for HA data on CRISP 30 for CA 
ti=findgen(n)*dt
x=FINDGEN((N - 1)/2.) + 1
;making freq full freq now only need half for ft plotting
IF evod(n) EQ 0 THEN freq = [0.0, X, N/2,-N/2+X]/(N*dt) ELSE freq = [0.0, X,-(N/2 +1)+x]/(N*dt)

IF N_elements(alignment_error) EQ 0 THEN alignment_error=0.471906;0.164214 ; value for HA data set 
																	  ; 0.471906 for CA set 
																	  ; chnage this to better deafult in future 
	 
IF n_elements(dim) eq 0 THEN BEGIN ; 1d 

	aln_er=make_array(n,value=alignment_error)
	errs=sqrt(errors^2 + aln_er^2)
;bootstrap 
	signal_matrix=rebin(data-mean(data),n,500)
	noise_mat=randomn(seed,n,500)*rebin(sqrt((errs^2.)/n),n,500)
	sig_matrix_plsnoise=signal_matrix+noise_mat
	ft=fft(sig_matrix_plsnoise,dimension=1)
	signal=mean(ftt,dimension=2)


	IF n_elements(data2) NE 0 THEN BEGIN 
		errs2=sqrt(errors2^2 + aln_er^2)

		signal_matrix2=rebin(data2-mean(data2),n,500)
		noise_mat2=randomn(seed,n,500)*rebin(sqrt((errs2^2.)/n),n,500)
		sig_matrix_plsnoise2=signal_matrix2+noise_mat2
		ft2=fft(sig_matrix_plsnoise2,dimension=1)
		signal2=mean(ftt2,dimension=2)


		csrb=signal*conj(signal2)
		phase = atan(csrb,/phase)

		csrbsm = smooth(signal,5,/edge_truncate)*conj(smooth(signal2,5,/edge_truncate))
		csrr = smooth(signal,5,/edge_truncate)*conj(smooth(signal,5,/edge_truncate))
		csbb = smooth(signal2,5,/edge_truncate)*conj(smooth(signal2,5,/edge_truncate))
		coh = (csrbsm^2/(csrr*csbb))

		p1=plot(freq[1:n/2],phase)
		p2=plot(freq[1:n/2],coh(where(real_part(coh) gt 0.7)))
		p3=plot(freq[1:n/2],abs(signal2),linestyle=5,xstyle=3,/ylog)
		p4=plot(freq[1:n/2],abs(signal2)/abs(signal),linestyle=5,xstyle=3,/ylog)

	ENDIF
	p=plot(freq[1:n/2],abs(signal),linestyle=5,xstyle=3,/ylog)

ENDIF

IF dim EQ 2 THEN BEGIN
	aln_er=make_array(sz(1),sz(2),value=alignment_error)
	errs=sqrt(errors^2 + aln_er^2)

	signal_matrix=rebin(data-mean(data),sz(1),sz(2),500)
	noise_mat=randomn(seed,sz(1),sz(2),500)*rebin(sqrt((errs^2.)/n),n,500)
	sig_matrix_plsnoise=signal_matrix+noise_mat
	ft=fft(sig_matrix_plsnoise,dimension=2)
	signal=mean(ftt,dimension=3)

	IF n_elements(data2) NE 0 THEN BEGIN 
		errs2=sqrt(errors2^2 + aln_er^2)

		signal_matrix2=rebin(data2-mean(data2),sz(1),sz(2),500)
		noise_mat2=randomn(seed,sz(1),sz(2),500)*rebin(sqrt((errs2^2.)/n),sz(1),sz(2),500)
		sig_matrix_plsnoise2=signal_matrix2+noise_mat2
		ft2=fft(sig_matrix_plsnoise2,dimension=2)
		signal2=mean(ftt2,dimension=3)


		csrb=signal*conj(signal2)
		phase = atan(csrb,/phase)

		csrbsm = smooth(signal,5,/edge_truncate)*conj(smooth(signal2,5,/edge_truncate))
		csrr = smooth(signal,5,/edge_truncate)*conj(smooth(signal,5,/edge_truncate))
		csbb = smooth(signal2,5,/edge_truncate)*conj(smooth(signal2,5,/edge_truncate))
		coh = (csrbsm^2/(csrr*csbb))

		for i = 1, N_elements(freq)/2 do im1=image(phase^0.3,/current,rgb_table=11)
		for i = 1, N_elements(freq)/2 do im2=image(coh(where(real_part(coh) gt 0.7)),/current,rgb_table=11)
		for i = 1, N_elements(freq)/2 do im3=image(abs(signal2)^0.3,/current,rgb_table=11)
		for i = 1, N_elements(freq)/2 do im4=image(abs(signal2)/abs(signal)^0.3,/current,rgb_table=11)
		;Need to make these same colour scale bar! format add bar 

	ENDIF
ENDIF

IF dim EQ 3 THEN BEGIN
   
	aln_er=make_array(sz(1),sz(2),sz(3),value=alignment_error)
	errs=sqrt(errors^2 + aln_er^2)

	;bootstrap 
	signal_matrix=rebin(data-mean(data),sz(1),sz(2),sz(3),100)

	noise_mat=randomn(seed,sz(1),sz(2),sz(3),100)*rebin(sqrt((errs^2.)/n),sz(1),sz(2),n,100)
	help,noise_mat
	help,signal_matrix
	sig_matrix_plsnoise=signal_matrix+noise_mat
	ft=fft(sig_matrix_plsnoise,dimension=3)
	signal=mean(ftt,dimension=4)


	IF n_elements(data2) NE 0 THEN BEGIN 
		errs2=sqrt(errors2^2 + aln_er^2)

		signal_matrix2=rebin(data2-mean(data2),sz(1),sz(2),sz(3),100)
		noise_mat2=randomn(seed,sz(1),sz(2),sz(3),100)*rebin(sqrt((errs2^2.)/n),sz(1),sz(2),sz(3),100)
		sig_matrix_plsnoise2=signal_matrix2+noise_mat2
		ft2=fft(sig_matrix_plsnoise2,dimension=3)
		signal2=mean(ftt2,dimension=4)


		csrb=signal*conj(signal2)
		phase = atan(csrb,/phase)

		csrbsm = smooth(signal,5,/edge_truncate)*conj(smooth(signal2,5,/edge_truncate))
		csrr = smooth(signal,5,/edge_truncate)*conj(smooth(signal,5,/edge_truncate))
		csbb = smooth(signal2,5,/edge_truncate)*conj(smooth(signal2,5,/edge_truncate))
		coh = (csrbsm^2/(csrr*csbb))

		for i = 1, N_elements(freq)/2 do im1=image(phase[*,*,i]^0.3,/current,layout=[7,(N_elements(freq)/2)/7,i],rgb_table=11)
		for i = 1, N_elements(freq)/2 do im2=image(real_part(coh[*,*,i]) gt 0.7,/current,layout=[7,(N_elements(freq)/2)/7,i],rgb_table=11)
		for i = 1, N_elements(freq)/2 do im3=image(abs(signal2[*,*,i])^0.3,/current,layout=[7,(N_elements(freq)/2)/7,i],rgb_table=11)
		for i = 1, N_elements(freq)/2 do im4=image(abs(signal2[*,*,i])/abs(signal[*,*,i])^0.3,/current,layout=[7,(N_elements(freq)/2)/7,i],rgb_table=11)
		;Need to make these same colour scale bar! format add bar 

	ENDIF
	index = [0,30,60,90,120,150,220]
	FOR i = 1, N_elements(freq)/2  do Im=image(abs(signal)^0.3,/current,layout=[7,(N_elements(freq)/2)/7,i],rgb_table=11,c_value=index)

	freq_pkpwr=fltarr(sz(1),sz(2))

	FOR j=0, sz(2) -1 DO BEGIN
	FOR i=0, sz(1) -1 DO BEGIN
	mxpwr = max(reform(signal[i,j,*]),in)
	freq_pkpwr[i,j]=freq[in]
	ENDFOR
	ENDFOR

	im=image(freq_pkpwr^0.3,axis_style=2,rgb_table=4,layout=[2,2,1])
	a=OBJ_NEW('Colorbar', orientation=1,/border,range=freq)


ENDIF  

IF (n_elements(dim) NE 0) AND (dim NE 2) THEN print, 'Only 1d or 2d  or 3d handled, please enter dim=2 if required or leave keyword empty for 1d'
  
END












