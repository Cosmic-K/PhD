pro quiforap,file,all_amps,all_periods,single=single

RESTORE_NUWT_COMMON_DATA,file,located_out=located_out, threads_out=threads_out, fft_results_out=fft_results_out, fft_stats_out=fft_stats_out
COMMON all_nuwt_dat, nuwt_located, nuwt_threads, nuwt_fft_results, nuwt_fft_stats
slit_num=n_elements(nuwt_threads)

;###############################################
;UNIT CONVERSIONS AND LABELS
;###############################################
km_per_arcsec = 725.27
amp_units = 'km'
period_units = 's'
freq_units = 's^{-1}'
res = 0.0592
amp_units = 'pixels'
cad = 30.
period_units = 'timestep'
amp_dx = res*km_per_arcsec
freq_dt = 1.0/cad
;Mm = (0.725/16.981891892)


IF Keyword_set(single) THEN begin
all_amps=fltarr(slit_num,2)
all_periods=fltarr(slit_num)

FOR i=0, slit_num-1 do begin 
slit_threads = nuwt_threads[i]
slit_fft_stats = nuwt_fft_stats[i]
len=max(slit_threads.length ,bt)

a_amps = [slit_fft_stats[bt].peak_amplitude[0]*amp_dx]
a_amps_er = [slit_fft_stats[bt].err_peak_amplitude[0]*amp_dx]

a_periods = [1.0/(slit_fft_stats[bt].peak_freq[0]*freq_dt)]

all_amps[i,0]=a_amps
all_amps[i,1]=a_amps_er
all_periods[i]=a_periods

ENDFOR

ENDIF ELSE BEGIN

min_user_flag = -1
max_user_flag = 1000

FOR i=0, slit_num-1 do begin 

slit_threads = nuwt_threads[i]
slit_fft_stats = nuwt_fft_stats[i]

n_threads = n_elements(slit_threads)

;###############################################
;FIND VALID WAVES AND CONVERT TO PROPER UNITS
;###############################################
threads_with_waves = where(slit_fft_stats.num_signif_peaks GT 0, /NULL)
num_th_waves = slit_fft_stats[threads_with_waves].num_signif_peaks

;Cap the number of allowed waves to four (or whatever is the limit of the current results)
max_num_waves_per_thread = n_elements(slit_fft_stats[0].peak_amplitude)
loc_too_many_waves = where(num_th_waves GT max_num_waves_per_thread, /NULL)
IF n_elements(loc_too_many_waves) GT 0 THEN BEGIN
    num_th_waves[loc_too_many_waves] = max_num_waves_per_thread
ENDIF

found_first_good_wave = 0
FOR tt=0L, n_elements(threads_with_waves)-1 DO BEGIN
    th_user_flag = slit_fft_stats[threads_with_waves[tt]].user_qual_flag
    th_auto_flag = slit_fft_stats[threads_with_waves[tt]].auto_qual_flag
    IF th_user_flag GE min_user_flag AND th_user_flag LE max_user_flag THEN BEGIN
        FOR ww=0, num_th_waves[tt]-1 DO BEGIN
            IF NOT found_first_good_wave THEN BEGIN
                a_amps = [slit_fft_stats[threads_with_waves[tt]].peak_amplitude[0]*amp_dx]
                a_periods = [1.0/(slit_fft_stats[threads_with_waves[tt]].peak_freq[0]*freq_dt)]
                a_wave_order = [1]
                found_first_good_wave = 1
            ENDIF ELSE BEGIN
                a_amps = [a_amps, slit_fft_stats[threads_with_waves[tt]].peak_amplitude[ww]*amp_dx]
                a_periods = [a_periods, 1.0/(slit_fft_stats[threads_with_waves[tt]].peak_freq[ww]*freq_dt)]
                a_wave_order = [a_wave_order, ww+1]
            ENDELSE
        ENDFOR
    ENDIF
ENDFOR

all_amps=[temporary(all_amps),a_amps]

all_periods=[temporary(all_periods),a_periods]
ENDFOR

ENDELSE 


END

