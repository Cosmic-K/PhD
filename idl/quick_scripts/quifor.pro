pro quiforap,file,all_amps,all_periods

RESTORE_NUWT_COMMON_DATA,file,located_out=located_out, threads_out=threads_out, fft_results_out=fft_results_out, fft_stats_out=fft_stats_out
COMMON all_nuwt_dat, nuwt_located, nuwt_threads, nuwt_fft_results, nuwt_fft_stats

IF NOT KEYWORD_SET(min_user_flag) THEN min_user_flag = -1
IF NOT KEYWORD_SET(max_user_flag) THEN max_user_flag = 1000

slit_threads = nuwt_threads[0]
slit_fft_stats = nuwt_fft_stats[0]

n_threads = n_elements(slit_threads)

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
                all_amps = [slit_fft_stats[threads_with_waves[tt]].peak_amplitude[0]*amp_dx]
                all_periods = [1.0/(slit_fft_stats[threads_with_waves[tt]].peak_freq[0]*freq_dt)]
                all_wave_order = [1]
                found_first_good_wave = 1
            ENDIF ELSE BEGIN
                all_amps = [all_amps, slit_fft_stats[threads_with_waves[tt]].peak_amplitude[ww]*amp_dx]
                all_periods = [all_periods, 1.0/(slit_fft_stats[threads_with_waves[tt]].peak_freq[ww]*freq_dt)]
                all_wave_order = [all_wave_order, ww+1]
            ENDELSE
        ENDFOR
    ENDIF
ENDFOR
end

