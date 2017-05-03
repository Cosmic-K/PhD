pro quifoft,file,slit_threads,slit_fft_results,slitnum=slitnum

RESTORE_NUWT_COMMON_DATA,file,located_out=located_out, threads_out=threads_out, fft_results_out=fft_results_out, fft_stats_out=fft_stats_out

COMMON all_nuwt_dat, nuwt_located, nuwt_threads, nuwt_fft_results, nuwt_fft_stats

IF n_elements(slitnum) EQ 0 THEN slitnum = 0

IF slitnum GT n_elements(nuwt_fft_stats) THEN BEGIN
    last_slit = n_elements(nuwt_fft_stats)-1
    print, 'WARNING! slit number '+strtrim(slitnum, 2)+' does not exist!'
    print, '   slitnum set to '+strtrim(last_slit, 2)+' (last set of results)'
    slitnum = last_slit
ENDIF

slit_threads = nuwt_threads[slitnum]
slit_fft_results = nuwt_fft_results[slitnum]
;slit_fft_stats = nuwt_fft_stats[slitnum]

print,n_elements(slit_threads)

;active_th = slit_threads[h]
;tpos = active_th.pos ;dummy array with postions of the selected thread
;terr = active_th.err_pos ;errors on postions of thread
;th_flags = active_th.bin_flags ;flags for thread data quality
;res = slit_fft_results[h] ;results from 'wave_from_fft.pro'
;stats = slit_fft_stats[h] ;results from 'wave_from_fft.pro'
;tpos = tpos[active_th.start_bin:active_th.end_bin]
;terr = terr[active_th.start_bin:active_th.end_bin]
;trend = res.trend
;apod_pos = (tpos-trend)*res.apod_window + trend

;s_len = active_th.length
;ts_xvals = (findgen(s_len) + active_th.start_bin)*dt



;For reference, each element in fft_results contains a structure with the fft results for a single thread
;The format of each structure is as follows:
;
;   result_th_fft = {power:fltarr(nf), $
;                    amplitude:fltarr(nf), $
;                    freq:fltarr(nf), $
;                    phase:fltarr(nf), $
;                    err_power:fltarr(nf), $
;                    err_amplitude:fltarr(nf), $
;                    err_phase:fltarr(nf), $
;                    trend:fltarr(len_thread), $
;                    apod_window:fltarr(len_thread), $
;                    bin_flags:intarr(nf), $
;                    signif_vals:fltarr(nf)}
;
;    stats_th_fft = {peak_power:[0.0, 0.0, 0.0, 0.0], $
;                    peak_amplitude:[0.0, 0.0, 0.0, 0.0], $
;                    peak_freq:[0.0, 0.0, 0.0, 0.0], $
;                    peak_phase:[0.0, 0.0, 0.0, 0.0], $
;                    err_peak_power:[0.0, 0.0, 0.0, 0.0], $
;                    err_peak_amplitude:[0.0, 0.0, 0.0, 0.0], $
;                    err_peak_phase:[0.0, 0.0, 0.0, 0.0], $
;                    peak_bin:[-1, -1, -1, -1], $
;                    KS_stat:[-1.0, -1.0, -1.0, -1.0, -1.0], $
;                    KS_prob:[-1.0, -1.0, -1.0, -1.0, -1.0], $
;                    AD_stat:[-1.0, -1.0, -1.0, -1.0, -1.0], $
;                    AD_crit:[-1.0, -1.0, -1.0, -1.0, -1.0], $
;                    LB_stat:[-1.0, -1.0, -1.0, -1.0, -1.0], $
;                    LB_chisqrd:[-1.0, -1.0, -1.0, -1.0, -1.0], $
;                    num_signif_peaks:0, $
;                    fft_length:-1, $
;                    window_func:'not_applicable', $
;                    window_param:-1.0, $
;                    cpg:-1.0, $
;                    signif_level:0.0, $
;                    signif_test:'not_applicable', $
;                    adjacent_peaks:allow_adj_pks, $
;                    user_qual_flag:-1, $
;                    auto_qual_flag:1}

;   Quick key for values in 'bin_flags' :
;       -2 : invalid thread (too litle real data)
;       -1 : empty or invalid frequency bin (not used in fft of particular thread)
;        0 : fft result below selected significance level
;        1 : significant value in fft result
;        2 : local maximum in nearby significant fft results

;###############################################################################
;PLOTTING THE DATA
;###############################################################################


END

