;+
;NAME: PLOT_NUWT_FFT_RESULTS
;
;PURPOSE:
;   Plots the FFT wave results from 'wave_from_fft.pro' for each thread as a
;   seperate multi-panel plot in a single PDF. Also prints various parameters
;   for diagnostic purposes.
;
;INPUTS:
;   None directly. Loads in the common blocks containing the results from
;   'locate_things.pro' (not currently used), 'follow_thread.pro', and
;   'wave_from_fft.pro'
;
;OPTIONAL INPUTS:
;   res - spatial resolution of data (in arcsec). Default is 1
;   cad - temporal cadence of the data (in s). Default is 1
;   plot_dist_units - string indicating what units to use for distance in the plot
;                     labels. Defaults to units of 'pixels' unless 'res' is set,
;                     in which case the default is 'arcsec'
;   plot_time_units - string indicating what units to use for time in the plot
;                     labels. Defaults to units of 'steps' unless 'cad' is set,
;                     in which case the default is 's'
;   /final_units - if set, will assume wave parameters are already in the correct
;                  units of [km] for amplitude and [s^-1] for frequency
;   slitnum - virtual slit number for header text. By defualt assumes 1
;   header - custom header text. Useful for identifying the source data.
;            Defaults to 'NUWT FFT Results'
;   run_tag - custom string that will be appended to the end of the header.
;             Normally used to keep track of differnt runs of the same dataset.
;             There is no default run_tag string.
;   save_folder - folder in which to save the plots. Defaults to the user's home folder.
;                 Will also append a '/' to the end if not included.
;   filename - name for the output PDF file. Default is 'NUWT_FFT_results'
;
;OUTPUTS:
;   PDF_file - multi-page PDF containing FFT results and diagnostics for each
;              thread / set of waves found. If there are multiple significant
;              wave results, the program will plot the combined waveform of
;              all of the waves with power above the significance threshold.
;
;HISTORY: Name---------Date---------Description
;         M Weberg  30 JUNE, 2016  Initial coding
;         M Weberg  26  AUG, 2016  Expanded options and updated plot format
;                                  - Added basic unit conversions
;                                  - Will now plot / list the four largest waves
;                                  - Can not be passed an arbitary header text
;                                  - Other visual tweaks and small improvements
;         M Weberg  14 SEPT, 2016  reconfigured to use NUWT master COMMON block
;
;TO-DO / LIMITATIONS:
;   - Automatic metadata handling to denote dataset and source of observations.
;     (currently managed via user defined text strings)
;-

FUNCTION TEST_FFT_WAVE_FUNCTION, amplitude_in, freq_in, phase_in, total_len, dt=dt
    IF NOT keyword_set(dt) THEN dt = 1.0
    ;print, 'input wave parameters: amp =', amplitude_in, ' freq =', freq_in, ' phase =', phase_in, ' total len', total_len
    wave_func = amplitude_in*cos(2.0*!PI*freq_in*findgen(total_len)*dt+phase_in)
    ;print, 'num vals returned =', n_elements(wave_func)
    ;if n_elements(wave_func) EQ 1 THEN print, 'wave val =', wave_func
RETURN, wave_func
END

PRO PLOT_NUWT_FFT_RESULTS, res=res, cad=cad, plot_dist_units=plot_dist_units, $
                           plot_time_units=plot_time_units, final_units=final_units, $
                           show_power_errors=show_power_errors, $
                           slitnum=slitnum, header=header, run_tag=run_tag, $
                           save_folder=save_folder, filename=filename

;###############################################################################
;Setting default values
;###############################################################################
IF n_elements(slitnum) EQ 0 THEN slitnum = 0
IF NOT KEYWORD_SET(header) THEN header = 'NUWT FFT Results'
IF KEYWORD_SET(run_tag) THEN header = header + ' - '+run_tag
IF NOT KEYWORD_SET(filename) THEN filename = 'NUWT_FFT_results'
IF NOT KEYWORD_SET(save_folder) THEN save_folder = '' ;i.e. defaults to home folder
IF strlen(save_folder) GT 0 AND NOT save_folder.endswith('/') THEN save_folder = save_folder+'/'

wave_order_colors = ['red', 'orange', 'purple', 'violet']

update_progress_interval = 5.0 ;[percent]

;###############################################################################
;LOADING DATA AND SELECTING CORRECT SET OF RESULTS
;###############################################################################
COMMON all_nuwt_dat, nuwt_located, nuwt_threads, nuwt_fft_results, nuwt_fft_stats

IF slitnum GT n_elements(nuwt_fft_stats) THEN BEGIN
    last_slit = n_elements(nuwt_fft_stats)-1
    print, 'WARNING! slit number '+strtrim(slitnum, 2)+' does not exist!'
    print, '   slitnum set to '+strtrim(last_slit, 2)+' (last set of results)'
    slitnum = last_slit
ENDIF

slit_threads = nuwt_threads[slitnum]
slit_fft_results = nuwt_fft_results[slitnum]
slit_fft_stats = nuwt_fft_stats[slitnum]

n_threads = n_elements(slit_threads)

;###############################################################################
;UNIT CONVERSIONS AND LABELS
;###############################################################################
km_per_arcsec = 725.27
dx = 1.0
dt = 1.0
amp_dx = 1.0
freq_dt = 1.0
amp_units = 'km'
freq_units = 's^{-1}'

IF NOT keyword_set(plot_dist_units) THEN plot_dist_units = 'arcsec'
IF NOT keyword_set(plot_time_units) THEN plot_time_units = 's'
IF NOT keyword_set(res) THEN BEGIN
    res = 1.0
    plot_dist_units = 'pixels'
    amp_units = 'pixels'
ENDIF
IF NOT keyword_set(cad) THEN BEGIN
    cad = 1.0
    plot_time_units = 'steps'
    freq_units = 'step^{-1}'
ENDIF

IF NOT keyword_set(final_units) THEN BEGIN
    IF plot_dist_units EQ 'arcsec' THEN dx = res
    IF plot_dist_units EQ 'Mm' THEN dx = res*km_per_arcsec/1000.0
    IF plot_time_units EQ 's' THEN dt = cad
    IF plot_time_units EQ 'min' THEN dt = cad/60.0
    IF plot_dist_units NE 'pixels' THEN amp_dx = res*km_per_arcsec
    IF plot_time_units NE 'steps' THEN freq_dt = 1.0/cad
    convert_to_pxls = 1.0
    convert_to_ts = 1.0
ENDIF ELSE BEGIN
    convert_to_pxls = 1.0/res*km_per_arcsec
    convert_to_ts = 1.0*cad
ENDELSE

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
print, 'Plotting FFT results for all '+strtrim(n_threads, 2)+' threads ...'
print, '--- Please wait, this may take a few minutes ---'
print_progress_threshold = update_progress_interval

fig_fft = window(name='fig_fft', dimensions=[1200, 725], /buffer)

start_time = systime(/seconds)
FOR h=0L, (n_threads-1) DO BEGIN
    ;print, 'Thread number', h
    active_th = slit_threads[h]
    tpos = active_th.pos ;dummy array with postions of the selected thread
    terr = active_th.err_pos ;errors on postions of thread
    th_flags = active_th.bin_flags ;flags for thread data quality

    res = slit_fft_results[h] ;results from 'wave_from_fft.pro'
    stats = slit_fft_stats[h] ;results from 'wave_from_fft.pro'
    tpos = tpos[active_th.start_bin:active_th.end_bin]
    terr = terr[active_th.start_bin:active_th.end_bin]
    trend = res.trend
    apod_pos = (tpos-trend)*res.apod_window + trend

    s_len = active_th.length
    ts_xvals = (findgen(s_len) + active_th.start_bin)*dt

    ;Finding wave properties (including any secondary waves)
    total_fit_params = 0
    total_num_waves = 0
    wave_vals = fltarr(s_len)
    tag_color = strarr(n_elements(stats.peak_bin))
    tag_color[*] = 'dark grey'
    FOR w=0, 3 DO BEGIN ;only consider the first four waves for now
        IF stats.peak_bin[w] GT -1 THEN BEGIN
            add_wave = TEST_FFT_WAVE_FUNCTION(stats.peak_amplitude[w]*convert_to_pxls, $
                                              stats.peak_freq[w]*convert_to_ts, stats.peak_phase[w], s_len)
            wave_vals = wave_vals + add_wave
            total_num_waves += 1
            total_fit_params += 3
            tag_color[w] = 'black'
        ENDIF
    ENDFOR

    residuals = tpos - (wave_vals + trend)

    ;Extracting various bits of information for printing
    num_good_pnts = n_elements(th_flags[where(th_flags EQ 2, /NULL)])
    num_filled_pnts = s_len - num_good_pnts
    percent_filled = (100.0*num_filled_pnts)/s_len
    num_freq_bins = n_elements(res.freq)
    num_fft_pad_zeros = stats.fft_length - s_len
    IF num_fft_pad_zeros GT 0 THEN tag_smpad = 'YES' ELSE tag_smpad = 'no'

    mean_trend = mean(trend)*dx
    slope_trend = ((trend[-1] - trend[0])/(ts_xvals[-1] - ts_xvals[0]))*(dx/dt)

    ;##### UPPER LEFT SUBPLOT #####
    ;Plot the time series of thread positions along with apodized values and estimated wave
    plt_pos = errorplot(ts_xvals, tpos*dx, terr*dx, name='Observed', $
                        color='blue', symbol='o', /sym_filled, linestyle='none', errorbar_color='blue',$
                        position=[0.06, 0.52, 0.35, 0.9], /current)

    plt_apod_pos = plot(ts_xvals, apod_pos*dx, name='Windowed', $
                        color='sky blue', symbol='x', linestyle='none', /overplot)

    plt_trend = plot(ts_xvals, trend*dx, name='Trend', color='black', symbol='none', linestyle=':', /overplot)

    plt_wave = plot(ts_xvals, (wave_vals + trend)*dx, name='Fit Wave', $
                    color='black', symbol='none', linestyle='-', /overplot)

    leg_pos = legend(target=[plt_pos, plt_apod_pos, plt_trend, plt_wave], /normal, $
                     position=[0.485, 0.9], linestyle='none', transparency=100, font_size=12)
    plt_pos.title = 'Time Series'
    plt_pos.ytitle = 'Distance ['+plot_dist_units+']'

    ;##### LOWER LEFT SUBPLOT #####
    ;Plot the residuals of the time series (i.e. what is left after subtracting the 'fit' function)
    plt_residuals = plot(ts_xvals, residuals*dx, color='blue', symbol='none', linestyle='-', $
                         position=[0.06, 0.1, 0.35, 0.48], /current)
    plt_residuals.ytitle = 'Residuals ['+plot_dist_units+']'
    plt_residuals.xtitle = 'Time ['+plot_time_units+']'

    ;##### UPPER RIGHT SUBPLOT #####
    ;Plot fft power spectrum with the significance limit
    plt_pow = plot(res.freq*freq_dt, res.power*amp_dx^2, name='FFT results', $
                   color='black', symbol='none', linestyle='-', $
                   position=[0.56, 0.52, 0.85, 0.9], /current, /stairstep)

    IF KEYWORD_SET(show_power_errors) THEN BEGIN
        plt_errs_pow = errorplot(res.freq*freq_dt, res.power*amp_dx^2, res.err_power*amp_dx^2, $
                                 color='black', symbol='none', linestyle='none', $
                                 errorbar_color='grey', /overplot)
    ENDIF

    plt_signif_pow = plot(res.freq*freq_dt, res.signif_vals*amp_dx^2, name='Signif. limit', $
                          color='black', symbol='none', linestyle='--', /overplot)

    plt_primary_peak = plot([stats.peak_freq[0]*freq_dt, stats.peak_freq[0]*freq_dt], $
                            [stats.peak_power[0]*amp_dx^2, stats.peak_power[0]*amp_dx^2], $
                            name='Primary Peak', color=wave_order_colors[0], symbol='D', linestyle='none', /overplot)

    plt_secondary_peak = plot([stats.peak_freq[1]*freq_dt, stats.peak_freq[1]*freq_dt], $
                              [stats.peak_power[1]*amp_dx^2, stats.peak_power[1]*amp_dx^2], $
                              name='Secondary', color=wave_order_colors[1], symbol='*', linestyle='none', /overplot)

    plt_tertiary_peak = plot([stats.peak_freq[2]*freq_dt, stats.peak_freq[2]*freq_dt], $
                             [stats.peak_power[2]*amp_dx^2, stats.peak_power[2]*amp_dx^2], $
                             name='Tertiary', color=wave_order_colors[2], symbol='+', linestyle='none', /overplot)

    plt_quaternary_peak = plot([stats.peak_freq[3]*freq_dt, stats.peak_freq[3]*freq_dt], $
                               [stats.peak_power[3]*amp_dx^2, stats.peak_power[3]*amp_dx^2], $
                               name='Quaternary', color=wave_order_colors[3], symbol='Td', linestyle='none', /overplot)

    leg_fft = legend(target=[plt_pow, plt_signif_pow, plt_primary_peak, plt_secondary_peak, $
                             plt_tertiary_peak, plt_quaternary_peak], $
                     /normal, position=[1.0, 0.9], linestyle='none', transparency=100, font_size=12)
    plt_pow.title = 'FFT'
    plt_pow.ytitle = 'PSD [$'+amp_units+'^2$]'
    plt_pow.xtitle = 'Frequency [$'+freq_units+'$]'

    ;##### PRINTED TEXT #####
    ;Printing useful information in the plot
    txt_slit_ID = text(0.01, 0.97, 'Slit # '+strtrim(slitnum, 2), font_size=14, font_style=1)
    txt_thread_ID = text(0.08, 0.97, 'Thread # '+strtrim(h+1, 2), font_size=14, font_style=1)
    txt_header = text(0.35, 0.97, header, font_size=14, font_style=1)

    txt_trend = text(0.365, 0.67, 'Linear Trend:$\n$'+$
                     '   mean = '+strtrim(mean_trend, 2)+'$\n$'+$
                     '   slope = '+strtrim(string(slope_trend, format='(e10.3)'), 2), font_size=12)
    txt_thread_len = text(0.365, 0.57, strtrim(active_th.length, 2)+' data points:$\n$   '+$
                          strtrim(num_good_pnts, 2)+' good$\n$   '+$
                          strtrim(num_filled_pnts, 2)+' filled', font_size=12)
    txt_percent_filled = text(0.365, 0.52, strtrim(percent_filled, 2)+'% filled', font_size=12)

    txt_freq_len = text(0.86, 0.65, strtrim(num_freq_bins, 2)+' freq. bins', font_size=12)
    txt_fft_len = text(0.86, 0.57, strtrim(stats.fft_length, 2)+' points input to FFT:$\n$   '+$
                          strtrim(active_th.length, 2)+' data (windowed)$\n$   '+$
                          strtrim(num_fft_pad_zeros, 2)+' zeros (padding)', font_size=12)

    txt_GOF_header = text(0.365, 0.43, 'GOF Tests', font_size=14, font_style=1)
    txt_KS_test = text(0.365, 0.38, 'Kolmogorov-Smirnov', font_size=12)
    txt_KS_stat = text(0.365, 0.35, '   stat = '+strtrim(stats.KS_stat[total_num_waves], 2), font_size=12)
    txt_KS_prob = text(0.365, 0.32, '   prob = '+strtrim(stats.KS_prob[total_num_waves], 2), font_size=12)
    txt_AD_test = text(0.365, 0.27, 'Anderson-Darling', font_size=12)
    txt_AD_stat = text(0.365, 0.24, '   $A^2$ = '+strtrim(stats.AD_stat[total_num_waves], 2), font_size=12)
    txt_AD_crit = text(0.365, 0.21, '   crit val = '+strtrim(stats.AD_crit[total_num_waves], 2), font_size=12)
    txt_LB_test = text(0.365, 0.16, 'Ljung-Box', font_size=12)
    txt_LB_stat = text(0.365, 0.13, '   $Q$ = '+strtrim(stats.LB_stat[total_num_waves], 2), font_size=12)
    txt_LB_crit = text(0.365, 0.10, '   $\chi^2$ = '+strtrim(stats.LB_chisqrd[total_num_waves], 2), font_size=12)

    txt_result_header = text(0.56, 0.43, 'Results', font_size=14, font_style=1)
    txt_primary_tag = text(0.515, 0.355, 'Primary$\n$Wave', font_size=12, font_style=1, color=wave_order_colors[0])
    txt_primary_results = text(0.59, 0.33, $
                               'peak PSD = '+strtrim(stats.peak_power[0]*amp_dx^2, 2)+' $'+amp_units+'^2\n$'+$
                               'amplitude = '+strtrim(stats.peak_amplitude[0]*amp_dx, 2)+' $'+amp_units+'\n$'+$
                               'frequency = '+strtrim(stats.peak_freq[0]*freq_dt, 2)+' $'+freq_units+'\n$'+$
                               'phase = '+strtrim(stats.peak_phase[0], 2)+' rad', font_size=12, color=tag_color[0])

    txt_secondary_tag = text(0.515, 0.25, 'Secondary$\n$Wave', font_size=12, font_style=1, color=wave_order_colors[1])
    txt_secondary_results = text(0.59, 0.225, $
                                 'peak PSD = '+strtrim(stats.peak_power[1]*amp_dx^2, 2)+' $'+amp_units+'^2\n$'+$
                                 'amplitude = '+strtrim(stats.peak_amplitude[1]*amp_dx, 2)+' $'+amp_units+'\n$'+$
                                 'frequency = '+strtrim(stats.peak_freq[1]*freq_dt, 2)+' $'+freq_units+'\n$'+$
                                 'phase = '+strtrim(stats.peak_phase[1], 2)+' rad', font_size=12, color=tag_color[1])

    txt_tertiary_tag = text(0.515, 0.145, 'Tertiary$\n$Wave', font_size=12, font_style=1, color=wave_order_colors[2])
    txt_tertiary_results = text(0.59, 0.12, $
                                'peak PSD = '+strtrim(stats.peak_power[2]*amp_dx^2, 2)+' $'+amp_units+'^2\n$'+$
                                'amplitude = '+strtrim(stats.peak_amplitude[2]*amp_dx, 2)+' $'+amp_units+'\n$'+$
                                'frequency = '+strtrim(stats.peak_freq[2]*freq_dt, 2)+' $'+freq_units+'\n$'+$
                                'phase = '+strtrim(stats.peak_phase[2], 2)+' rad', font_size=12, color=tag_color[2])

    txt_quaternary_tag = text(0.515, 0.04, 'Quaternary$\n$Wave', font_size=12, font_style=1, color=wave_order_colors[3])
    txt_quaternary_results = text(0.59, 0.015, $
                                  'peak PSD = '+strtrim(stats.peak_power[3]*amp_dx^2, 2)+' $'+amp_units+'^2\n$'+$
                                  'amplitude = '+strtrim(stats.peak_amplitude[3]*amp_dx, 2)+' $'+amp_units+'\n$'+$
                                  'frequency = '+strtrim(stats.peak_freq[3]*freq_dt, 2)+' $'+freq_units+'\n$'+$
                                  'phase = '+strtrim(stats.peak_phase[3], 2)+' rad', font_size=12, color=tag_color[3])

    txt_fft_header = text(0.80, 0.43, 'FFT Details', font_size=14, font_style=1)
    txt_window_func = text(0.80, 0.36, 'Window Function:$\n$   '+stats.window_func, font_size=12)
    txt_window_param = text(0.80, 0.33, 'Window Parameter: '+strtrim(stats.window_param, 2), font_size=12)
    txt_cpg = text(0.80, 0.30, 'Coherent Power Gain = '+strtrim(stats.cpg, 2), font_size=12)
    txt_smpad = text(0.80, 0.27, 'Smoothed & Padded? '+tag_smpad, font_size=12)
    txt_signif_level = text(0.80, 0.17, 'Significance Level: '+strtrim(stats.signif_level, 2), font_size=12)
    txt_signif_test = text(0.80, 0.115, 'Significance Test:$\n$   '+stats.signif_test, font_size=12)
    txt_signif_test = text(0.80, 0.085, 'Adjacent peaks allowed? '+stats.adjacent_peaks, font_size=12)

    txt_timestamp = text(0.85, 0.01, 'Date & Time Created$\n$'+systime(), font_size=10, color='dark grey')

    IF h LT (n_threads-1) THEN BEGIN
        fig_fft.save, save_folder+filename+'.pdf', page_size=[12.0, 7.25], /append
        fig_fft.erase
    ENDIF ELSE BEGIN
        fig_fft.save, save_folder+filename+'.pdf', page_size=[12.0, 7.25], /append, /close
    ENDELSE

    ;##### PRINTING STATUS AND ESTIMATED TIME REMAINING #####
    percent_done = (float(h+1)/float(n_threads))*100.0
    IF percent_done GE print_progress_threshold THEN BEGIN
        current_time = systime(/seconds)
        avg_sec_per_loop = (current_time - start_time)/double(h+1)
        est_min_remaining = (n_threads-h-1)*avg_sec_per_loop/60.0
        print, strtrim(string(percent_done, format='(F6.2)'), 2)+'% completed ('+strtrim(h+1,2)+'/'+strtrim(n_threads, 2)+'), '+$
               'estimated '+strtrim(string(est_min_remaining, format='(F8.2)'), 2)+' min remaining'
        print_progress_threshold = print_progress_threshold + update_progress_interval
    ENDIF
ENDFOR
print, 'Finished plotting FFT results to a multi-page PDF!'
print, 'Save Folder: ', save_folder
print, 'Filename: ', filename+'.pdf'
END
