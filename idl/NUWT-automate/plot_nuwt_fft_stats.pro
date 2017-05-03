;+
;NAME: PLOT_NUWT_FFT_STATS
;
;PURPOSE:
;   Plots histograms of the FFT wave statistics from 'wave_from_fft.pro' for all
;   found waves. Returns one set of plots for each order (primary, secondary, etc.)
;   of waves as well as the combined distibution of all waves
;
;INPUTS:
;   None directly. Loads in the common blocks containing the results from
;   'locate_things.pro' (not currently used), 'follow_thread.pro', and
;   'wave_from_fft.pro'
;
;OPTIONAL INPUTS:
;   res - spatial resolution of data (in arcsec). Default is 1
;   cad - temporal cadence of the data (in s). Default is 1
;   /final_units - if set, will assume wave parameters are already in the correct
;                  units of [km] for amplitude and [s^-1] for frequency
;   min_user_flag - minimum user quality flag to plot. These flags are set with
;                   'set_fit_qual_flags.pro'. Default is -1 (all waves)
;   max_user_flag - maximum user quality flag to plot (see above). Default is 1000
;   VVV_range - range to plot for amp, period, or vel amp. Defaults are [0,2000],
;               [0,2000], & [0,50] respectively
;   VVV_nbins - number of histogram bins for amp, period, or vel amp. Defaults are
;               max_amp/25, max_period/25, & max_vel_amp/1.0 respectivly
;   /normalized - if set, will normalize the histograms by the number of waves
;   ref_waves - stucture containing either input simulated wave parameters (from
;               'generate_kink_waves.pro') or the 'fft_stats' output from a different
;               run of NUWT. Will be histogrammed and compared to the current results.
;               Used for testing and validation.
;   /simulated - if set, will assume the ref_waves structure contains input
;                simulated parameters (default)
;   /nuwt - if set, will assume the ref_waves structure contains output from
;           a seperate NUWT run. Note: if both /simulated and /nuwt are set, the
;           program will give precedence to the /simulated option.
;   slitnum - virtual slit number to plot. By default, will show slit number "0".
;             Note: can also set to 'all' to plot a combined histogram of all slits.
;   header - custom header text. Useful for identifying the source data.
;            Defaults to 'NUWT FFT stats'
;   run_tag - custom string that will be appended to the end of the header.
;             Normally used to keep track of differnt runs of the same dataset.
;             There is no default run_tag string.
;   save_folder - folder in which to save the plots. Defaults to the user's home folder.
;                 Will also append a '/' to the end if not included.
;   filename - name for the output PDF file. Default is 'NUWT_FFT_stats'
;
;OUTPUTS:
;   PDF_file - multi-page PDF containing FFT stats and diagnostics for each
;              ordered group of waves (primary, secondary, etc.) The first page
;              will show the distributions of all waves combined.
;   bulk_stats_out - stucture with the calculated summary statistics for the
;                    bulk (ALL good waves) NUWT results
;   ref_stats_out - stucture with the calculated summary statistics for the
;                   reference data
;
;HISTORY: Name---------Date---------Description
;         M Weberg  8 Sept, 2016  Initial coding
;         M Weberg 14 SEPT, 2016  reconfigured to use NUWT master COMMON block
;         M Weberg 18 OCT, 2016   added output structure for summary stats
;         M Weberg 09 JAN, 2017   added the 'all' option to 'slitnum'
;
;TO-DO / LIMITATIONS:
;   - more options for the 'compare_waves' distibution that would allow comparisions
;     to other NUWT results and not just the simulated waves input for testing
;   - Automatic metadata handling to denote dataset and source of observations.
;     (currently managed via user defined text strings)
;-

PRO PLOT_NUWT_FFT_STATS, res=res, cad=cad, final_units=final_units, $
                         min_user_flag=min_user_flag, max_user_flag=max_user_flag,$
                         amp_range=amp_range, amp_nbins=amp_nbins, $
                         period_range=period_range, period_nbins=period_nbins, $
                         vel_amp_range=vel_amp_range, vel_amp_nbins=vel_amp_nbins, $
                         normalized=normalized, ref_waves=ref_waves, $
                         bulk_stats_out=bulk_stats_out, ref_stats_out=ref_stats_out, $
                         simulated=simulated, nuwt=nuwt, $
                         slitnum=slitnum, header=header, run_tag=run_tag, $
                         save_folder=save_folder, filename=filename

;###############################################################################
;Setting default values
;###############################################################################
IF NOT KEYWORD_SET(min_user_flag) THEN min_user_flag = -1
IF NOT KEYWORD_SET(max_user_flag) THEN max_user_flag = 1000

IF KEYWORD_SET(res) AND KEYWORD_SET(cad) THEN BEGIN
    ;defaults for units of [km] and [s]
    IF NOT KEYWORD_SET(amp_range) THEN amp_range = [0.0, 2000.0]
    IF NOT KEYWORD_SET(period_range) THEN period_range = [0.0, 2000.0]
    IF NOT KEYWORD_SET(vel_amp_range) THEN vel_amp_range = [0.0, 50.0]
    IF NOT KEYWORD_SET(amp_nbins) THEN amp_nbins = amp_range[1]/25
    IF NOT KEYWORD_SET(period_nbins) THEN period_nbins = period_range[1]/25
    IF NOT KEYWORD_SET(vel_amp_nbins) THEN vel_amp_nbins = vel_amp_range[1]/1
ENDIF ELSE BEGIN
    ;defaults for units of [pixels] and [timesteps]
    IF NOT KEYWORD_SET(amp_range) THEN amp_range = [0.0, 10.0]
    IF NOT KEYWORD_SET(period_range) THEN period_range = [0.0, 100.0]
    IF NOT KEYWORD_SET(vel_amp_range) THEN vel_amp_range = [0.0, 10.0]
    IF NOT KEYWORD_SET(amp_nbins) THEN amp_nbins = 50
    IF NOT KEYWORD_SET(period_nbins) THEN period_nbins = 50
    IF NOT KEYWORD_SET(vel_amp_nbins) THEN vel_amp_nbins = 50
ENDELSE

;Selecting which type (if any) of reference waves to show
IF KEYWORD_SET(simulated) AND KEYWORD_SET(nuwt) THEN nuwt = 0 ;simulated takes precedence

IF n_elements(slitnum) EQ 0 THEN slitnum = 0
IF NOT KEYWORD_SET(header) THEN header = 'NUWT FFT Wave Summary'
IF KEYWORD_SET(run_tag) THEN header = header + ' - '+run_tag
IF NOT KEYWORD_SET(filename) THEN filename = 'NUWT_FFT_stats'
IF NOT KEYWORD_SET(save_folder) THEN save_folder = '' ;i.e. defaults to home folder
IF strlen(save_folder) GT 0 AND NOT save_folder.endswith('/') THEN save_folder = save_folder+'/'

;###############################################################################
;LOADING DATA AND SELECTING CORRECT SET OF RESULTS
;###############################################################################
COMMON all_nuwt_dat, nuwt_located, nuwt_threads, nuwt_fft_results, nuwt_fft_stats

IF typename(slitnum) EQ 'STRING' THEN BEGIN
    ;Concat results from all data slits and plot them together
    total_num_slits = n_elements(nuwt_fft_stats)

    slit_threads = nuwt_threads[0]
    slit_fft_stats = nuwt_fft_stats[0]
    FOR i=1,total_num_slits-1 DO BEGIN
        slit_threads = [temporary(slit_threads), nuwt_threads[i]]
        slit_fft_stats = [temporary(slit_fft_stats), nuwt_fft_stats[i]]
    ENDFOR
ENDIF ELSE BEGIN
    ;Typical use, plotting just one slit of data
    IF slitnum GT n_elements(nuwt_fft_stats) THEN BEGIN
        last_slit = n_elements(nuwt_fft_stats)-1
        print, 'WARNING! slit number '+strtrim(slitnum, 2)+' does not exist!'
        print, '   slitnum set to '+strtrim(last_slit, 2)+' (last set of results)'
        slitnum = last_slit
    ENDIF

    slit_threads = nuwt_threads[slitnum]
    slit_fft_stats = nuwt_fft_stats[slitnum]

ENDELSE

n_threads = n_elements(slit_threads)

;###############################################################################
;UNIT CONVERSIONS AND LABELS
;###############################################################################
km_per_arcsec = 725.27
amp_dx = 1.0
freq_dt = 1.0
amp_units = 'km'
period_units = 's'
freq_units = 's^{-1}'

IF NOT keyword_set(res) THEN BEGIN
    res = 1.0
    amp_units = 'pixels'
ENDIF
IF NOT keyword_set(cad) THEN BEGIN
    cad = 1.0
    period_units = 'timestep'
ENDIF

IF (NOT keyword_set(final_units)) AND (amp_units NE 'pixels') THEN BEGIN
    amp_dx = res*km_per_arcsec
    freq_dt = 1.0/cad
ENDIF ELSE BEGIN
    ;TEMP PATCH FOR EXPERIMENTAL VEL AMP CALC
    freq_dt = 1.0/cad
    print, 'plotting alt vel amp'
ENDELSE

;###############################################################################
;FIND VALID PEAK WAVES AND CONVERT TO PROPER UNITS
;###############################################################################
threads_with_waves = where(slit_fft_stats.num_signif_peaks GT 0, /NULL)
num_th_waves = slit_fft_stats[threads_with_waves].num_signif_peaks

;Cap the number of allowed waves to four (or whatever is the limit of the current results)
max_num_waves_per_thread = n_elements(slit_fft_stats[0].peak_amplitude)
loc_too_many_waves = where(num_th_waves GT max_num_waves_per_thread, /NULL)
IF n_elements(loc_too_many_waves) GT 0 THEN BEGIN
    num_th_waves[loc_too_many_waves] = max_num_waves_per_thread
ENDIF

;Loop over all valid wave results (and filtering as we go)
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

all_vel_amps = 2*!PI*all_amps/all_periods

;###############################################################################
;EXTRACT AND CONVERT REFERENCE PARAMETERS
;###############################################################################
ref_count_outliers = {amp:0, period:0, vel_amp:0}
IF n_elements(ref_waves) GT 0 THEN BEGIN
    ;Extract reference wave parameters
    n_ref = n_elements(ref_waves.phase)
    ref_params = {amplitude:fltarr(n_ref), period:fltarr(n_ref), phase:fltarr(n_ref), $
                  velocity_amp:fltarr(n_ref), mean:fltarr(n_ref), slope:fltarr(n_ref), $
                  num_cyc:fltarr(n_ref), num_waves:fltarr(n_ref)}
    IF KEYWORD_SET(nuwt) THEN BEGIN
        ;Results from an alternate run of 'run_nuwt_with_fft.pro'
        ref_params.amplitude = ref_waves.peak_amplitude[0]*amp_dx
        ref_params.period = 1.0/(ref_waves.peak_freq[0]*freq_dt)
        ref_params.velocity_amp = 2*!PI*ref_params.amplitude/ref_params.period
        ; ref_params.num_cyc = (ref_coords.length*cad)/ref_params.period
        ref_params.phase = ref_waves.peak_phase[0]
        ;fixing periods of INF and NaN
        loc_nan = where(~finite(ref_params.period))
        ref_params.period[loc_nan] = 0.0
        ref_params.velocity_amp[loc_nan] = 0.0
        ref_params.num_waves = ref_waves.num_signif_peaks
    ENDIF ELSE BEGIN
        ;Simulated data from 'generate_kink_waves.pro'
        ref_params.amplitude = ref_waves.amp*km_per_arcsec
        ref_params.period = ref_waves.period
        ref_params.velocity_amp = 2*!PI*ref_params.amplitude/ref_params.period
        ; ref_params.num_cyc = (ref_coords.length*cad)/ref_params.period
        ref_params.phase = ref_waves.phase
        loc_shift_phase = where(ref_params.phase GT !PI, /NULL)
        IF n_elements(loc_shift_phase) GT 0 THEN BEGIN
            ;Adjust phase to be in the range of -pi to +pi (as returned by fft)
            ref_params.phase[loc_shift_phase] = ref_params.phase[loc_shift_phase] - 2*!PI
        ENDIF
        ref_params.num_waves[0,-1] = 1
    ENDELSE

    ref_amp_hist = histogram(ref_params.amplitude, LOCATIONS=ref_amp_bins, $
                             nbins=amp_nbins, min=amp_range[0], max=amp_range[1])
    ref_period_hist = histogram(ref_params.period, LOCATIONS=ref_period_bins, $
                                nbins=period_nbins, min=period_range[0], max=period_range[1])
    ref_vel_amp_hist = histogram(ref_params.velocity_amp, LOCATIONS=ref_vel_amp_bins, $
                                 nbins=vel_amp_nbins, min=vel_amp_range[0], max=vel_amp_range[1])
ENDIF

IF n_elements(ref_waves) GT 0 THEN BEGIN
    ;Calculate summary statistics of ref distribution
    ref_summary_stats = DICTIONARY('amp_mean', 0.0, 'amp_median', 0.0, $
                                   'amp_stddev', 0.0, 'amp_MAD', 0.0,$
                                   'period_mean', 0.0, 'period_median', 0.0, $
                                   'period_stddev', 0.0, 'period_MAD', 0.0, $
                                   'vel_amp_mean', 0.0, 'vel_amp_median', 0.0, $
                                   'vel_amp_stddev', 0.0, 'vel_amp_MAD', 0.0, $
                                   'log_norm_amp_mean', 0.0, 'log_norm_amp_median', 0.0, $
                                   'log_norm_amp_mode', 0.0, $
                                   'log_norm_amp_stddev', 0.0, 'log_norm_amp_MAD', 0.0, $
                                   'log_norm_period_mean', 0.0, 'log_norm_period_median', 0.0, $
                                   'log_norm_period_mode', 0.0, $
                                   'log_norm_period_stddev', 0.0, 'log_norm_period_MAD', 0.0,$
                                   'log_norm_vel_amp_mean', 0.0, 'log_norm_vel_amp_median', 0.0, $
                                   'log_norm_vel_amp_mode', 0.0,$
                                   'log_norm_vel_amp_stddev', 0.0, 'log_norm_vel_amp_MAD', 0.0)

    FOREACH VAR, LIST(LIST('amp', ref_params.amplitude), $
                      LIST('period', ref_params.period), $
                      LIST('vel_amp', ref_params.velocity_amp)) DO BEGIN
        ref_summary_stats[VAR[0]+'_mean'] = MEAN(VAR[1])
        ref_summary_stats[VAR[0]+'_median'] = MEDIAN(VAR[1])
        ref_summary_stats[VAR[0]+'_stddev'] = STDDEV(VAR[1])
        ref_summary_stats[VAR[0]+'_MAD'] = MEANABSDEV(VAR[1], /median)
        ref_summary_stats['log_norm_'+VAR[0]+'_mean'] = EXP(MEAN(alog(VAR[1])))
        ref_summary_stats['log_norm_'+VAR[0]+'_median'] = EXP(MEDIAN(alog(VAR[1])))
        ref_summary_stats['log_norm_'+VAR[0]+'_stddev'] = EXP(STDDEV(alog(VAR[1])))
        ref_summary_stats['log_norm_'+VAR[0]+'_MAD'] = EXP(MEANABSDEV(alog(VAR[1]), /median))
        ref_summary_stats['log_norm_'+VAR[0]+'_mode'] =  EXP(MEAN(alog(VAR[1])) - STDDEV(alog(VAR[1]))^2)
    ENDFOREACH

    ; Counting datapoints outside the histogram range
    ref_count_outliers = {amp:0, period:0, vel_amp:0}
    ref_count_outliers.amp = n_elements(where(ref_params.amplitude GT amp_range[1], /NULL))
    ref_count_outliers.period = n_elements(where(ref_params.period GT period_range[1], /NULL))
    ref_count_outliers.vel_amp = n_elements(where(ref_params.velocity_amp GT vel_amp_range[1], /NULL))

    ref_stats_out = ref_summary_stats
ENDIF

;For reference, each element in fft_results amd fft_stats contains a structure with the fft results for a single thread
;   The format of each structure is as follows:
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
;

;Quick key for values in 'bin_flags' :
;   -2 : invalid thread (too litle real data)
;   -1 : empty or invalid frequency bin (not used in fft of particular thread)
;    0 : fft result below selected significance level
;    1 : significant value in fft result
;    2 : local maximum in nearby significant fft results

;###############################################################################
;MAKE THE PLOTS
;###############################################################################
wave_group_titles = ['All waves', 'Primary waves only', 'Secondary waves only', $
                     'Tertiary waves only', 'Quaternary waves only']
wave_group_colors = ['black', 'red', 'orange', 'purple', 'violet']

FOR page_num=1, 5 DO BEGIN
    ;##### INITIALIZE THE FIGURE AND FILTER WAVES
    fig_fft_stats = window(name='fig_fft_stats', dimensions=[600, 720], /buffer)

    IF page_num EQ 1 THEN BEGIN ;All waves
        filter_waves = where(all_wave_order GT 0, /NULL)
    ENDIF ELSE BEGIN ;Groups of ONLY a certain wave order (primary, secondary, etc.)
        filter_waves = where(all_wave_order EQ page_num-1, /NULL)
    ENDELSE

    total_num_waves = n_elements(filter_waves)

    ;##### CALCULATE HISTOGRAMS AND VARIOUS SIMPLE STATISTICS
    summary_stats = DICTIONARY('amp_mean', 0.0, 'amp_stddev', 0.0,$
                               'amp_median', 0.0, 'amp_MAD', 0.0,$
                               'period_mean', 0.0, 'period_stddev', 0.0,$
                               'period_median', 0.0, 'period_MAD', 0.0,$
                               'vel_amp_mean', 0.0, 'vel_amp_stddev', 0.0,$
                               'vel_amp_median', 0.0, 'vel_amp_MAD', 0.0, $
                               'log_norm_amp_mean', 0.0, 'log_norm_amp_stddev', 0.0,$
                               'log_norm_amp_median', 0.0, 'log_norm_amp_MAD', 0.0,$
                               'log_norm_amp_mode', 0.0, $
                               'log_norm_period_mean', 0.0, 'log_norm_period_stddev', 0.0,$
                               'log_norm_period_median', 0.0, 'log_norm_period_MAD', 0.0,$
                               'log_norm_period_mode', 0.0,$
                               'log_norm_vel_amp_mean', 0.0, 'log_norm_vel_amp_stddev', 0.0,$
                               'log_norm_vel_amp_median', 0.0, 'log_norm_vel_amp_MAD', 0.0,$
                               'log_norm_vel_amp_mode', 0.0)

    IF total_num_waves GT 0 THEN BEGIN
        amps_for_plot = all_amps[filter_waves]
        periods_for_plot = all_periods[filter_waves]
        vel_amps_for_plot = all_vel_amps[filter_waves]
        plot_amp_hist = histogram(amps_for_plot, LOCATIONS=plot_amp_bins, $
                                  nbins=amp_nbins, min=amp_range[0], max=amp_range[1])
        plot_period_hist = histogram(periods_for_plot, LOCATIONS=plot_period_bins, $
                                     nbins=period_nbins, min=period_range[0], max=period_range[1])
        plot_vel_amp_hist = histogram(vel_amps_for_plot, LOCATIONS=plot_vel_amp_bins, $
                                      nbins=vel_amp_nbins, min=vel_amp_range[0], max=vel_amp_range[1])

        ;Looping over a list of lists with variable:array pairs allows for more compact code
        color_summary_stats = 'black'
        FOREACH VAR, LIST(LIST('amp', amps_for_plot), $
                          LIST('period', periods_for_plot), $
                          LIST('vel_amp', vel_amps_for_plot)) DO BEGIN
            summary_stats[VAR[0]+'_mean'] = MEAN(VAR[1])
            summary_stats[VAR[0]+'_median'] = MEDIAN(VAR[1])
            summary_stats[VAR[0]+'_stddev'] = STDDEV(VAR[1])
            summary_stats[VAR[0]+'_MAD'] = MEANABSDEV(VAR[1], /median)
            ; summary_stats['log_norm_'+VAR[0]+'_mean'] = 10^(MEAN(alog10(VAR[1])))
            ; summary_stats['log_norm_'+VAR[0]+'_median'] = 10^(MEDIAN(alog10(VAR[1])))
            ; summary_stats['log_norm_'+VAR[0]+'_stddev'] = 10^(STDDEV(alog10(VAR[1])))
            ; summary_stats['log_norm_'+VAR[0]+'_MAD'] = 10^(MEANABSDEV(alog10(VAR[1]), /median))
            ; summary_stats['log_norm_'+VAR[0]+'_mode'] =  10^(MEAN(alog10(VAR[1])) - STDDEV(alog10(VAR[1]))^2)
            summary_stats['log_norm_'+VAR[0]+'_mean'] = EXP(MEAN(alog(VAR[1])))
            summary_stats['log_norm_'+VAR[0]+'_median'] = EXP(MEDIAN(alog(VAR[1])))
            summary_stats['log_norm_'+VAR[0]+'_stddev'] = EXP(STDDEV(alog(VAR[1])))
            summary_stats['log_norm_'+VAR[0]+'_MAD'] = EXP(MEANABSDEV(alog(VAR[1]), /median))
            summary_stats['log_norm_'+VAR[0]+'_mode'] =  EXP(MEAN(alog(VAR[1])) - STDDEV(alog(VAR[1]))^2)
        ENDFOREACH
    ENDIF ELSE BEGIN
        ;If there if is no waves of the given order, create dummy arrays
        amps_for_plot = fltarr(10)
        periods_for_plot = fltarr(10)
        vel_amps_for_plot = fltarr(10)
        plot_amp_hist = fltarr(10)
        plot_amp_bins = findgen(10)
        plot_period_hist = fltarr(10)
        plot_period_bins = findgen(10)
        plot_vel_amp_hist = fltarr(10)
        plot_vel_amp_bins = findgen(10)/10.0
        color_summary_stats = 'dark grey'
    ENDELSE

    ; Counting datapoints outside the histogram range
    test_count_outliers = {amp:0, period:0, vel_amp:0}
    test_count_outliers.amp = n_elements(where(amps_for_plot GT amp_range[1], /NULL))
    test_count_outliers.period = n_elements(where(periods_for_plot GT period_range[1], /NULL))
    test_count_outliers.vel_amp = n_elements(where(vel_amps_for_plot GT vel_amp_range[1], /NULL))

    IF page_num EQ 1 THEN bulk_stats_out = summary_stats

    ;Normalize the histogram by the total number of waves
    IF KEYWORD_SET(normalized) AND total_num_waves GT 0 THEN BEGIN
        plot_amp_hist = float(plot_amp_hist)/float(total_num_waves)
        plot_period_hist = float(plot_period_hist)/float(total_num_waves)
        plot_vel_amp_hist = float(plot_vel_amp_hist)/float(total_num_waves)
        y_axis_label = 'Fraction of waves'
    ENDIF ELSE y_axis_label = 'Number of waves'

    ;##### TOP SUBPLOT #####
    plt_amp = barplot(plot_amp_bins, plot_amp_hist, name='Amplitude', $
                      color='black', fill_color='light grey', linestyle='-', $
                      position=[0.10, 0.7, 0.72, 0.92], /current, histogram=1, xrange=amp_range)
    plt_amp.xtitle = 'Displacement Amplitude [$'+amp_units+'$]'
    plt_amp.ytitle = y_axis_label

    ;##### MIDDLE SUBPLOT #####
    plt_period = barplot(plot_period_bins, plot_period_hist , name='Period', $
                         color='black', fill_color='light grey', linestyle='-', $
                         position=[0.10, 0.39, 0.72, 0.61], /current, histogram=1, xrange=period_range)
    plt_period.xtitle = 'Period [$'+period_units+'$]'
    plt_period.ytitle = y_axis_label

    ;##### BOTTOM SUBPLOT #####
    plt_vel_amp = barplot(plot_vel_amp_bins, plot_vel_amp_hist, name='Velocity Amplitude', $
                          color='black', fill_color='light grey', linestyle='-', $
                          position=[0.10, 0.08, 0.72, 0.30], /current, histogram=1, xrange=vel_amp_range)
    plt_vel_amp.xtitle = 'Velocity Amplitude [$'+amp_units+' '+period_units+'^{-1}$]'
    plt_vel_amp.ytitle = y_axis_label

    IF n_elements(ref_waves) GT 0 AND page_num EQ 1 THEN BEGIN
        ; Plotting Reference distributions
        IF KEYWORD_SET(normalized) AND n_ref GT 0 THEN BEGIN
            ref_amp_hist = float(ref_amp_hist)/float(n_ref)
            ref_period_hist = float(ref_period_hist)/float(n_ref)
            ref_vel_amp_hist = float(ref_vel_amp_hist)/float(n_ref)
        ENDIF
        plt_ref_amp = plot(ref_amp_bins, ref_amp_hist, /histogram, $
                            color='dark red', linestyle='-', overplot=plt_amp)
        plt_ref_period = plot(ref_period_bins, ref_period_hist, /histogram, $
                               color='dark red', linestyle='-', overplot=plt_period)
        plt_ref_vel_amp = plot(ref_vel_amp_bins, ref_vel_amp_hist, /histogram, $
                                color='dark red', linestyle='-', overplot=plt_vel_amp)
    ENDIF

    ; Plotting symbols for outliers
    FOREACH VAR, LIST(LIST(test_count_outliers.amp, 0.69, 0.895, 'black'), $
                      LIST(ref_count_outliers.amp, 0.69, 0.875, 'dark red'), $
                      LIST(test_count_outliers.period, 0.69, 0.585, 'black'), $
                      LIST(ref_count_outliers.period, 0.69, 0.565, 'dark red'), $
                      LIST(test_count_outliers.vel_amp, 0.69, 0.275, 'black'), $
                      LIST(ref_count_outliers.vel_amp, 0.69, 0.255, 'dark red')) DO BEGIN
        ; List of lists of [test_var, x_coord, y_coord, sym_color]
        IF VAR[0] GT 0 THEN BEGIN
            sym_outliers = symbol(VAR[1], VAR[2], sym_text='$\rightarrow$', sym_color=VAR[3], $
                                  label_color=VAR[3], label_position='left', label_shift=[0.0, -0.005], $
                                  label_string=strtrim(VAR[0], 2), label_font_size=10)
        ENDIF
    ENDFOREACH

    IF total_num_waves GT 1 THEN BEGIN
        ;add symbols along the edge showing the mean, median, log-norm mean, and log-norm mode
        rel_mean_xloc = summary_stats.amp_mean/amp_range[1]
        arith_mean_sym = symbol(rel_mean_xloc, -0.035, 'triangle', sym_color='blue', $
                                sym_size=0.9, /relative, target=plt_amp)
        arith_mean_sym = symbol(rel_mean_xloc, 1.035, 'triangle_down', sym_color='blue', $
                                sym_size=0.9, /relative, target=plt_amp)
        rel_median_xloc = summary_stats.amp_median/amp_range[1]
        arith_median_sym = symbol([rel_median_xloc, rel_median_xloc],[-0.04, 1.04], 'vline', sym_color='blue', $
                                  sym_size=0.9, /relative, target=plt_amp)
        rel_log_norm_mean_xloc = summary_stats.log_norm_amp_mean/amp_range[1]
        log_norm_mean_sym = symbol(rel_log_norm_mean_xloc, -0.035, 'triangle', sym_color='green', $
                                   sym_size=0.9, /relative, target=plt_amp)
        log_norm_mean_sym = symbol(rel_log_norm_mean_xloc, 1.035, 'triangle_down', sym_color='green', $
                                   sym_size=0.9, /relative, target=plt_amp)
        rel_log_norm_mode_xloc = summary_stats.log_norm_amp_mode/amp_range[1]
        log_norm_mode_sym = symbol([rel_log_norm_mode_xloc, rel_log_norm_mode_xloc],[-0.04, 1.04], 'vline', sym_color='green', $
                                   sym_size=0.9, /relative, target=plt_amp)

        rel_mean_xloc = summary_stats.period_mean/period_range[1]
        arith_mean_sym = symbol(rel_mean_xloc, -0.035, 'triangle', sym_color='blue', $
                                sym_size=0.9, /relative, target=plt_period)
        arith_mean_sym = symbol(rel_mean_xloc, 1.035, 'triangle_down', sym_color='blue', $
                                sym_size=0.9, /relative, target=plt_period)
        rel_median_xloc = summary_stats.period_median/period_range[1]
        arith_median_sym = symbol([rel_median_xloc, rel_median_xloc], [-0.04, 1.04], 'vline', sym_color='blue', $
                                  sym_size=0.9, /relative, target=plt_period)
        rel_log_norm_mean_xloc = summary_stats.log_norm_period_mean/period_range[1]
        log_norm_mean_sym = symbol(rel_log_norm_mean_xloc, -0.035, 'triangle', sym_color='green', $
                                   sym_size=0.9, /relative, target=plt_period)
        log_norm_mean_sym = symbol(rel_log_norm_mean_xloc, 1.035, 'triangle_down', sym_color='green', $
                                   sym_size=0.9, /relative, target=plt_period)
        rel_log_norm_mode_xloc = summary_stats.log_norm_period_mode/period_range[1]
        log_norm_mode_sym = symbol([rel_log_norm_mode_xloc, rel_log_norm_mode_xloc], [-0.04, 1.04], 'vline', sym_color='green', $
                                   sym_size=0.9, /relative, target=plt_period)

        rel_mean_xloc = summary_stats.vel_amp_mean/vel_amp_range[1]
        arith_mean_sym = symbol(rel_mean_xloc, -0.035, 'triangle', sym_color='blue', $
                                sym_size=0.9, /relative, target=plt_vel_amp)
        arith_mean_sym = symbol(rel_mean_xloc, 1.035, 'triangle_down', sym_color='blue', $
                                sym_size=0.9, /relative, target=plt_vel_amp)
        rel_median_xloc = summary_stats.vel_amp_median/vel_amp_range[1]
        arith_median_sym = symbol([rel_median_xloc, rel_median_xloc], [-0.04, 1.04], 'vline', sym_color='blue', $
                                  sym_size=0.9, /relative, target=plt_vel_amp)
        rel_log_norm_mean_xloc = summary_stats.log_norm_vel_amp_mean/vel_amp_range[1]
        log_norm_mean_sym = symbol(rel_log_norm_mean_xloc, -0.035, 'triangle', sym_color='green', $
                                   sym_size=0.9, /relative, target=plt_vel_amp)
        log_norm_mean_sym = symbol(rel_log_norm_mean_xloc, 1.035, 'triangle_down', sym_color='green', $
                                   sym_size=0.9, /relative, target=plt_vel_amp)
        rel_log_norm_mode_xloc = summary_stats.log_norm_vel_amp_mode/vel_amp_range[1]
        log_norm_mode_sym = symbol([rel_log_norm_mode_xloc, rel_log_norm_mode_xloc], [-0.04, 1.04], 'vline', sym_color='green', $
                                   sym_size=0.9, /relative, target=plt_vel_amp)
    ENDIF

    ;##### PRINTED TEXT #####
    ;Header text
    txt_slit_ID = text(0.01, 0.97, 'Slit # '+strtrim(slitnum, 2), font_size=14, font_style=1)
    txt_header = text(0.20, 0.97, header, font_size=14, font_style=1)
    txt_num_threads = text(0.01, 0.94, strtrim(n_threads, 2)+' Threads', font_size=12)
    txt_wave_group = text(0.25, 0.94, wave_group_titles[page_num-1], font_size=12, $
                          font_style=1, color=wave_group_colors[page_num-1])
    txt_num_waves = text(0.59, 0.94, strtrim(total_num_waves, 2)+' Waves', font_size=12)

    ;Displacement Amplitude summary stats
    fake_legend_arith_mean = symbol(0.74, 0.90, 'triangle_right', sym_color='blue', sym_size=0.9, /normal)
    fake_legend_arith_median = symbol(0.74, 0.85, 'vline', sym_color='blue', sym_size=0.9, /normal)
    fake_legend_log_norm_mean = symbol(0.74, 0.77, 'triangle_right', sym_color='green', sym_size=0.9, /normal)
    fake_legend_log_norm_mode = symbol(0.74, 0.72, 'vline', sym_color='green', sym_size=0.9, /normal)
    tag_arithmetic = text(0.74, 0.925, '--- Arithmetic ---', font_size=11, color='blue')
    txt_amp_mean = text(0.76, 0.88, 'Mean $\pm$ Std.Dev.$\n$'+$
                        strtrim(summary_stats.amp_mean, 2)+' $\pm$ '+$
                        strtrim(summary_stats.amp_stddev, 2), $
                        font_size=11, color=color_summary_stats)
    txt_amp_median = text(0.76, 0.83, 'Median $\pm$ MAD$\n$'+$
                          strtrim(summary_stats.amp_median, 2)+' $\pm$ '+$
                          strtrim(summary_stats.amp_mad, 2), $
                          font_size=11, color=color_summary_stats)
    tag_log_normal = text(0.74, 0.795, '--- Log-Normal ---', font_size=11, color='green')
    txt_ln_amp_mean = text(0.76, 0.75, 'Mean $\pm$ Std.Dev.$\n$'+$
                           strtrim(summary_stats.log_norm_amp_mean, 2)+' $\pm$ '+$
                           strtrim(summary_stats.log_norm_amp_stddev, 2), $
                           font_size=11, color=color_summary_stats)
    txt_ln_amp_mode = text(0.76, 0.70, 'Mode$\n$'+$
                             strtrim(summary_stats.log_norm_amp_mode, 2), $
                             font_size=11, color=color_summary_stats)

    ;Period summary stats
    fake_legend_arith_mean = symbol(0.74, 0.59, 'triangle_right', sym_color='blue', sym_size=0.9, /normal)
    fake_legend_arith_median = symbol(0.74, 0.54, 'vline', sym_color='blue', sym_size=0.9, /normal)
    fake_legend_log_norm_mean = symbol(0.74, 0.46, 'triangle_right', sym_color='green', sym_size=0.9, /normal)
    fake_legend_log_norm_mode = symbol(0.74, 0.41, 'vline', sym_color='green', sym_size=0.9, /normal)
    tag_arithmetic = text(0.74, 0.615, '--- Arithmetic ---', font_size=11, color='blue')
    txt_period_mean = text(0.76, 0.57, 'Mean $\pm$ Std.Dev.$\n$'+$
                           strtrim(summary_stats.period_mean, 2)+' $\pm$ '+$
                           strtrim(summary_stats.period_stddev, 2), $
                           font_size=11, color=color_summary_stats)
    txt_period_median = text(0.76, 0.52, 'Median $\pm$ MAD$\n$'+$
                             strtrim(summary_stats.period_median, 2)+' $\pm$ '+$
                             strtrim(summary_stats.period_mad, 2), $
                             font_size=11, color=color_summary_stats)
    tag_log_norm = text(0.74, 0.485, '--- Log-Normal ---', font_size=11, color='green')
    txt_period_mean = text(0.76, 0.44, 'Mean $\pm$ Std.Dev.$\n$'+$
                           strtrim(summary_stats.log_norm_period_mean, 2)+' $\pm$ '+$
                           strtrim(summary_stats.log_norm_period_stddev, 2), $
                           font_size=11, color=color_summary_stats)
    txt_period_mode = text(0.76, 0.39, 'Mode$\n$'+$
                             strtrim(summary_stats.log_norm_period_mode, 2), $
                             font_size=11, color=color_summary_stats)

    ;Velocity Amplitude summary stats
    fake_legend_arith_mean = symbol(0.74, 0.28, 'triangle_right', sym_color='blue', sym_size=0.9, /normal)
    fake_legend_arith_median = symbol(0.74, 0.23, 'vline', sym_color='blue', sym_size=0.9, /normal)
    fake_legend_log_norm_mean = symbol(0.74, 0.15, 'triangle_right', sym_color='green', sym_size=0.9, /normal)
    fake_legend_log_norm_mode = symbol(0.74, 0.10, 'vline', sym_color='green', sym_size=0.9, /normal)
    tag_arithmetic = text(0.74, 0.305, '--- Arithmetic ---', font_size=11, color='blue')
    txt_vel_amp_mean = text(0.76, 0.26, 'Mean $\pm$ Std. Dev.$\n$'+$
                            strtrim(summary_stats.vel_amp_mean, 2)+' $\pm$ '+$
                            strtrim(summary_stats.vel_amp_stddev, 2), $
                            font_size=11, color=color_summary_stats)
    txt_vel_amp_median = text(0.76, 0.21, 'Median $\pm$ MAD$\n$'+$
                              strtrim(summary_stats.vel_amp_median, 2)+' $\pm$ '+$
                              strtrim(summary_stats.vel_amp_mad, 2), $
                              font_size=11, color=color_summary_stats)
    tag_log_normal = text(0.74, 0.175, '--- Log-Normal ---', font_size=11, color='green')
    txt_vel_amp_mean = text(0.76, 0.13, 'Mean $\pm$ Std. Dev.$\n$'+$
                            strtrim(summary_stats.log_norm_vel_amp_mean, 2)+' $\pm$ '+$
                            strtrim(summary_stats.log_norm_vel_amp_stddev, 2), $
                            font_size=11, color=color_summary_stats)
    txt_vel_amp_mode = text(0.76, 0.08, 'Mode$\n$'+$
                            strtrim(summary_stats.log_norm_vel_amp_mode, 2), $
                            font_size=11, color=color_summary_stats)

    txt_timestamp = text(0.76, 0.01, 'Date & Time Created$\n$'+systime(), font_size=9, color='dark grey')

    IF page_num LT 5 THEN BEGIN
        fig_fft_stats.save, save_folder+filename+'.pdf', page_size=[6.0, 7.2], /append
    ENDIF ELSE BEGIN
        fig_fft_stats.save, save_folder+filename+'.pdf', page_size=[6.0, 7.2], /append, /close
    ENDELSE
ENDFOR

print, 'Finished plotting FFT stats to a multi-page PDF!'
print, 'Save Folder: ', save_folder
print, 'Filename: ', filename+'.pdf'
END
