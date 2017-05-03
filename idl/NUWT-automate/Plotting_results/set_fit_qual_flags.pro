;+
;NAME: SET_FIT_QUAL_FLAGS
;
;PURPOSE:
;   Interactively plots the FFT wave results from 'wave_from_fft.pro' for each thread
;   and asks the user to rate the quality of the fit on a three step scale of:
;   0 (bad fit), 1 (questionable fit), & 2 (good fit)
;   Various GOF statistics are given as an aid for selection
;
;INPUTS:
;   None directly. Loads in the NUWT common blocks containing the wave results
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
;            Defaults to 'NUWT FFT Wave Result'
;
;OUTPUTS:
;   fit_qual - interger array with the user's selected quality flags
;
;HISTORY: Name---------Date---------Description
;         M Weberg  19 SEPT, 2016  Initial coding
;
;TO-DO / LIMITATIONS:
;   - Automatic metadata handling to denote dataset and source of observations.
;     (currently managed via user defined text strings)
;   - Better interfacing with COMMON block data to store the results
;-

FUNCTION TEST_FFT_WAVE_FUNCTION, amplitude_in, freq_in, phase_in, total_len, dt=dt
    IF NOT keyword_set(dt) THEN dt = 1.0
    ;print, 'input wave parameters: amp =', amplitude_in, ' freq =', freq_in, ' phase =', phase_in, ' total len', total_len
    wave_func = amplitude_in*cos(2.0*!PI*freq_in*findgen(total_len)*dt+phase_in)
    ;print, 'num vals returned =', n_elements(wave_func)
    ;if n_elements(wave_func) EQ 1 THEN print, 'wave val =', wave_func
RETURN, wave_func
END

PRO SET_FIT_QUAL_FLAGS, res=res, cad=cad, qual_flags=qual_flags, $
                        plot_dist_units=plot_dist_units, plot_time_units=plot_time_units, $
                        final_units=final_units, slitnum=slitnum, header=header

;Seting default values
IF n_elements(slitnum) EQ 0 THEN slitnum = 0
IF NOT KEYWORD_SET(header) THEN header = 'NUWT FFT Wave Result'

;###############################################
;LOADING DATA AND SELECTING CORRECT SET OF RESULTS
;###############################################
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

;###############################################
;UNIT CONVERSIONS AND LABELS
;###############################################
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
;                    adjacent_peaks:allow_adj_pks}

;   Quick key for values in 'bin_flags' :
;       -2 : invalid thread (too litle real data)
;       -1 : empty or invalid frequency bin (not used in fft of particular thread)
;        0 : fft result below selected significance level
;        1 : significant value in fft result
;        2 : local maximum in nearby significant fft results

;###############################################
;PLOTTING THE DATA AND GETTING FLAGS
;###############################################
IF n_threads GT 20 THEN BEGIN
    ask_cont = 'yes'
    print, ' ' ;empty line for cleaner printing
    print, 'WARNING! There are '+strtrim(n_threads, 2)+' total threads to be flagged!'
    READ, ask_cont, PROMPT='Are you sure you want to continue? (y/n) '
    ask_cont = strlowcase(ask_cont)
    IF NOT ask_cont.startswith('y') THEN BEGIN
        print, 'Exiting program. Quality flags NOT set.'
        STOP
    ENDIF
ENDIF

print, ' ' ;empty line for cleaner printing
print, '##### Setting fit quality flags for all '+strtrim(n_threads, 2)+' threads #####'
print, '--- Type "q" at any time to quit early ---'

qual_flags = intarr(n_threads)

fig_wave = window(name='fig_wave', window_title='Setting Wave Quality Flags', dimensions=[800, 800], /no_toolbar)

end_early = 0
FOR h=0L, (n_threads-1) DO BEGIN
    fig_wave.erase ;clearing the plot window

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

    mean_trend = mean(trend)*dx
    slope_trend = ((trend[-1] - trend[0])/(ts_xvals[-1] - ts_xvals[0]))*(dx/dt)

    ;##### UPPER LEFT SUBPLOT #####
    ;Plot the time series of thread positions along with apodized values and estimated wave
    plt_pos = errorplot(ts_xvals, tpos*dx, terr, name='Observed', $
                        color='blue', symbol='o', /sym_filled, linestyle='none', errorbar_color='blue',$
                        position=[0.10, 0.52, 0.74, 0.9], current=fig_wave)

    plt_apod_pos = plot(ts_xvals, apod_pos*dx, name='Windowed', $
                        color='sky blue', symbol='x', linestyle='none', /overplot)

    plt_trend = plot(ts_xvals, trend*dx, name='Trend', color='black', symbol='none', linestyle=':', /overplot)

    plt_wave = plot(ts_xvals, (wave_vals + trend)*dx, name='Fit Wave(s)', $
                    color='black', symbol='none', linestyle='-', /overplot)

    leg_pos = legend(target=[plt_pos, plt_apod_pos, plt_trend, plt_wave], /normal, $
                     position=[0.95, 0.9], linestyle='none', transparency=100, font_size=12)
    plt_pos.title = 'Time Series'
    plt_pos.ytitle = 'Distance ['+plot_dist_units+']'

    ;##### LOWER LEFT SUBPLOT #####
    ;Plot the residuals of the time series (i.e. what is left after subtracting the 'fit' function)
    plt_residuals = plot(ts_xvals, residuals*dx, color='blue', symbol='none', linestyle='-', $
                         position=[0.10, 0.1, 0.74, 0.48], current=fig_wave)
    plt_residuals.ytitle = 'Residuals ['+plot_dist_units+']'
    plt_residuals.xtitle = 'Time ['+plot_time_units+']'

    ;##### PRINTED TEXT #####
    ;Printing useful information in the plot
    txt_slit_ID = text(0.01, 0.97, 'Slit # '+strtrim(slitnum, 2), font_size=14, font_style=1)
    txt_thread_ID = text(0.12, 0.97, 'Thread # '+strtrim(h+1, 2), font_size=14, font_style=1)
    txt_header = text(0.35, 0.97, header, font_size=14, font_style=1)

    tx_num_waves = text(0.755, 0.75, strtrim(total_num_waves, 2) + ' total waves', font_size=12)
    txt_trend = text(0.755, 0.67, 'Linear Trend:$\n$'+$
                     '   mean = '+strtrim(mean_trend, 2)+'$\n$'+$
                     '   slope = '+strtrim(string(slope_trend, format='(e10.3)'), 2), font_size=12)
    txt_thread_len = text(0.755, 0.57, strtrim(active_th.length, 2)+' data points:$\n$   '+$
                          strtrim(num_good_pnts, 2)+' good$\n$   '+$
                          strtrim(num_filled_pnts, 2)+' filled', font_size=12)
    txt_percent_filled = text(0.755, 0.52, strtrim(percent_filled, 2)+'% filled', font_size=12)

    txt_GOF_header = text(0.755, 0.43, 'GOF Tests', font_size=14, font_style=1)
    txt_KS_test = text(0.755, 0.38, 'Kolmogorov-Smirnov', font_size=12)
    txt_KS_stat = text(0.755, 0.35, '   stat = '+strtrim(stats.KS_stat[total_num_waves], 2), font_size=12)
    txt_KS_prob = text(0.755, 0.32, '   prob = '+strtrim(stats.KS_prob[total_num_waves], 2), font_size=12)
    txt_AD_test = text(0.755, 0.27, 'Anderson-Darling', font_size=12)
    txt_AD_stat = text(0.755, 0.24, '   $A^2$ = '+strtrim(stats.AD_stat[total_num_waves], 2), font_size=12)
    txt_AD_crit = text(0.755, 0.21, '   crit val = '+strtrim(stats.AD_crit[total_num_waves], 2), font_size=12)
    txt_LB_test = text(0.755, 0.16, 'Ljung-Box', font_size=12)
    txt_LB_stat = text(0.755, 0.13, '   $Q$ = '+strtrim(stats.LB_stat[total_num_waves], 2), font_size=12)
    txt_LB_crit = text(0.755, 0.10, '   $\chi^2$ = '+strtrim(stats.LB_chisqrd[total_num_waves], 2), font_size=12)

    ;##### GETTING USER INPUT #####
    print, ' ' ;empty line for cleaner printing
    print, 'Plotting thread '+strtrim(h+1, 2)+' out of '+strtrim(n_threads, 2)
    user_flag = '0' ;initializing as a string allows for more robust options
    keep_asking = 1
    WHILE keep_asking DO BEGIN
        READ, user_flag, PROMPT='Please enter fit quality flag (0=bad, 1=questionable, 2=good): '
        test_for_int = stregex(user_flag, '^[-+]?[0-9][0-9]*$')
        IF test_for_int NE -1 THEN BEGIN
            keep_asking = 0 ; quit loop if actually given an integer
        ENDIF ELSE BEGIN
            user_flag = strlowcase(user_flag)
            IF user_flag.startswith('q') THEN BEGIN
                print, 'Exiting program early. Remaining quality flags set to 0 (bad fit)'
                user_flag = '0'
                keep_asking = 0
                end_early = 1
            ENDIF
        ENDELSE
    ENDWHILE
    qual_flags[h] = fix(user_flag)
    slit_fft_stats[h].user_qual_flag = user_flag
    IF end_early THEN h = n_threads ; should break out of the loop cleanly
ENDFOR

; Updating the NUWT master common block
nuwt_fft_stats[slitnum]  = slit_fft_stats

fig_wave.close

END
