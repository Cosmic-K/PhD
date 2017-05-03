;+
;NAME: WAVE_FROM_FFT
;
;PURPOSE:
;   Performs a Fast Fourier Transform (FFT) on the positions of each thread
;   found by 'follow_thread.pro' and then calculates the Power Spectral Density
;   (PSD). Results are saved to a list of structures, one for each thread. Also
;   computes and saves a number of other variables and reference values for
;   later plotting. Based on the program 'pwr_frm_fft.pro'.
;
;INPUTS:
;   None directly. Loads in the common block data from 'follow_thread.pro'.
;
;OPTIONAL INPUTS:
;   dx - spatial sampling of data. default is 1
;   dt - temporal sampling of the data. default is 1
;   ctrend - if keyword set will remove linear trend from each thread. If not
;            set, the mean value will be removed instead (needed for a good FFT)
;   smpad - SMooth And PAD - smooths the time-series using 3 pixel box-car average
;           and pads with zeros to increase frequency resolution
;   num_pad_fft - number of zeros to pad onto the end of the data array before
;                 computing the FFT. Defualt it 1000.
;   /bootstrap - if set, will perform bootstrapping to resample the data and obtaining
;                error estimates on the output values
;   num_bootstrap - number of bootstrap resamples to use. Default is 1000
;   include_nyquist - [EXPERIMENTAL!] For even number of data points, will
;                     include the results at the nyquist frequency.
;   window_func - select which windowing (also known as tapering or apodization)
;                 function to apply to the data. Choose from 'split_cosine_bell'
;                 (default) or 'hann'. See FFT tutorials / textbooks for more
;                 information.
;   window_param - set the parameter, p, to use in the generation of the window
;                  function. For the split cosine bell window, p indicates the
;                  total fraction of data that will be tappered (half on each end
;                  of the data series). At p = 1.0, the split cosine bell window
;                  is identical to the hann window. Default is 0.4 (i.e. 40%)
;   /vaughn - if set, will use the significance test from Vaughan 2005. Otherwise
;             will default to the method of Torrence & Compo 1998.
;   signif_levels - sets significance levels for FFT results (default=0.95)
;   /recover_power - [EXPERIMENTAL] attempts to recover leaked PSD by summing adjacent
;                    bins and performing a weighted mean to get the frequency
;
;OUTPUTS (in the 'fft_results_dat' COMMON block):
;   fft_results - LIST of structures with the FFT results for each thread. Since
;                 the length of each thread is different, the FFT PSD spectra will
;                 have different array sizes (hence the list of structures)
;   fft_stats - ARRAY of structures containing parameters for the four most
;               significant waves, GOF test statistics, and various other bits
;               of information used for tracking and plotting
;
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
;EXTERNAL CALLS - locate_things_min.pro, locate_things.pro, follow_thread.pro, cospd.pro
;
;HISTORY: Name---------Date---------Description
;         R Morton   ???, 2014 - Initial coding of 'pwr_frm_fft.pro'
;         R Morton   MAR, 2015 - Updated to include structures. [note: this older
;                                format had much larger, mostly empty, arrays].
;         M Weberg  JUNE, 2016 - Reworked the code. Seperated the FFT code (this
;                                program) from the rest of the NUWT code, cleaned
;                                up the logic a little bit, and added the new
;                                output format (with extra information for plotting)
;
;RESTRICTIONS: LOTS AT THE MOMENT. ROUTINE IS VERY MUCH DEPENDENT UPON USERS
;
;TO DO / RESTRICTIONS:
;   - Currently only saves the wave parameters for the 4 largest, significant
;     peaks. 5 or more peaks should not be all that common but we may be wrong...
;     The 'num_signif_peaks' variable will still count correctly so, it may not
;     be too hard to find the 'missing' wave parameters when needed (if ever).
;   - Include an error estimation for the FFT results by using bootstrapping to
;     create a 2D array of values based on the location and error of each thread
;     position. Will most likely be a seperate script.
;   - Add calculation for the power averaged over all threads. Use the following?
;       "Average is calculated by creating frequency bins, taking the ln of power
;        in frequency bins, plot the ln power PDF and fitting a Gaussian. Centroid
;        of Gaussian (mu) gives median value of distribution, i.e. exp(mu)=median."
;   - Add method to transfer results to a structure of arrays? This would allow
;     cleaner & faster indexing later but will require the output array to be
;     scaled for the largest results (with fill values for the smaller arrays).
;-

;bootstrap resampling of the data for obtaining error estimations
FUNCTION BOOTSTRAP_1D_DATA, data_in, errors_in, num_resamples=num_resamples
    ;Notes: (1) this is designed only for 1D input arrays
    ;       (2) if you are zero padding (for FFT), it is recommended to pad
    ;           AFTER performing bootstrapping on the real data (saves time)
    COMMON bootstrap_seed_num, bootseed

    IF NOT keyword_set(num_resamples) THEN num_resamples = 1000

    len_data = n_elements(data_in)
    output_array = fltarr(len_data, num_resamples)
    mean_array = rebin(data_in, len_data, num_resamples)
    sigma_array = rebin(errors_in, len_data, num_resamples)

    ;generate normally distributed numbers for each data point
    FOR i=0L, (len_data-1) DO BEGIN
        output_array[i,*] = RANDOMN(bootseed, num_resamples)
    ENDFOR

    ;adjust the mean and width to match the data points and their errors
    ;we should then have noramally (i.e. gaussian) distributed numbers
    ;centered on each point and sampled from a dist. with sigma = error
    output_array = temporary(output_array)*sigma_array + mean_array

RETURN, output_array
END

;temporal apodization using a Split Cosine Bell (or Hann for p = 1.0) window
FUNCTION GET_APOD_WINDOW, tn, cpg, parameter=p, win_function=win_function
    w_t = fltarr(tn) + 1

    IF n_elements(p) EQ 0 THEN p = 0.4

    IF win_function EQ 'split_cosine_bell' OR win_function EQ 'hann' THEN BEGIN
        num_taper = tn * (p / 2.0)
        ;w_t[0] = 0.5*(1.0 - cos(2*!PI*findgen(num_taper)/(tn*p)))
        w_t[0] = (sin(!PI*findgen(num_taper)/(tn*p)))^2
        w_t = w_t*shift(rotate(w_t, 2), 1) ;the rotate simply reverses the values
        cpg = total(w_t)/n_elements(w_t)
    ENDIF ELSE BEGIN ;defaults to a rectangular window
        cpg = 1.0
    ENDELSE
RETURN, w_t
END

FUNCTION TEST_FFT_WAVE_FUNCTION, amplitude_in, freq_in, phase_in, total_len, dt=dt
    IF NOT keyword_set(dt) THEN dt = 1.0
    ;print, 'input wave parameters: amp =', amplitude_in, ' freq =', freq_in, ' phase =', phase_in, ' total len', total_len
    wave_func = amplitude_in*cos(2.0*!PI*freq_in*findgen(total_len)*dt+phase_in)
    ;print, 'num vals returned =', n_elements(wave_func)
    ;if n_elements(wave_func) EQ 1 THEN print, 'wave val =', wave_func
RETURN, wave_func
END

PRO WAVE_FROM_FFT, dx=dx, dt=dt, ctrend=ctrend, smpad=smpad, num_pad_fft=num_pad_fft, $
                   bootstrap=bootstrap, num_bootstrap=num_bootstrap, include_nyquist=include_nyquist, $
                   window_func=window_func, window_param=window_param, $
                   vaughan=vaughan, signif_levels=signif_levels, $
                   recover_power=recover_power, adjacent_peaks=adjacent_peaks

;###############################################
;INITIAL HOUSE KEEPING
;###############################################

COMMON located_dat, located, nx, nt
COMMON threads_dat, threads
COMMON fft_results_dat, fft_results, fft_stats

;Setting default values
IF NOT keyword_set(dx) THEN dx = 1.0
IF NOT keyword_set(dt) THEN dt = 1.0
IF NOT keyword_set(num_pad_fft) THEN num_pad_fft = 1000
IF NOT keyword_set(num_bootstrap) THEN num_bootstrap = 1000
IF NOT keyword_set(window_func) THEN window_func = 'split_cosine_bell'
IF NOT keyword_set(window_param) THEN window_param = 0.4
IF strlowcase(window_func) EQ 'hann' THEN window_param = 1.0
IF window_param EQ 1.0 THEN window_func = 'hann'
;note: a split cosine bell window with p = 1.0 is the same as the hann window
IF strlowcase(window_func) EQ 'rectangular' THEN window_param = 0.0
IF NOT keyword_set(signif_levels) THEN signif_levels = 0.95
IF keyword_set(adjacent_peaks) THEN allow_adj_pks = 'yes' ELSE allow_adj_pks = 'no'

sz = size(threads)
n_threads = sz[1]

;create list to contain the results
fft_results = LIST()
;each element in fft_results will contain a structure with the fft results
;for a single thread. See the header for reference of the format.

;###############################################
;PERFORM FOURIER ANALYSIS ON EACH GOOD THREAD
;###############################################
FOR h=0L, (n_threads-1) DO BEGIN

    tpos = threads[h].pos[*] ;temp array with postions of the selected thread
    terr = threads[h].err_pos[*] ;errors on postions of thread

    ;Locate the start and end of thread
    t_start = -1000
    t_end = -1000 ;reset each time

    ; loc_th_vals = where(tpos ne -1.0)
    ; len_thread = n_elements(loc_th_vals)
    ; t_start = loc_th_vals[0]
    ; t_end = loc_th_vals[len_thread-1]

    t_start = threads[h].start_bin
    t_end = threads[h].end_bin
    len_thread = threads[h].length

    ;Determine output array sizes
    IF keyword_set(smpad) THEN len_fft_array = len_thread + num_pad_fft $
        ELSE len_fft_array = len_thread

    IF keyword_set(include_nyquist) THEN BEGIN
        nf = ceil((len_fft_array-1)/2.0) ;INCLUDING Nyquist freq for even N
    ENDIF ELSE BEGIN
        nf = floor((len_fft_array-1)/2.0) ;EXCLUDING Nyquist freq for even N
    ENDELSE

    IF nf LE 0 THEN nf = 1 ;for when there is not enough data to perform an fft

    ;Initialize structures to contain the fft results for this thread
    result_th_fft = {power:fltarr(nf), $
                     amplitude:fltarr(nf), $
                     freq:fltarr(nf), $
                     phase:fltarr(nf), $
                     err_power:fltarr(nf), $
                     err_amplitude:fltarr(nf), $
                     err_phase:fltarr(nf), $
                     trend:fltarr(len_thread), $
                     apod_window:fltarr(len_thread), $
                     bin_flags:intarr(nf), $
                     signif_vals:fltarr(nf)}

    stats_th_fft =  {peak_power:[0.0, 0.0, 0.0, 0.0], $
                     peak_amplitude:[0.0, 0.0, 0.0, 0.0], $
                     peak_freq:[0.0, 0.0, 0.0, 0.0], $
                     peak_phase:[0.0, 0.0, 0.0, 0.0], $
                     err_peak_power:[0.0, 0.0, 0.0, 0.0], $
                     err_peak_amplitude:[0.0, 0.0, 0.0, 0.0], $
                     err_peak_phase:[0.0, 0.0, 0.0, 0.0], $
                     peak_bin:[-1, -1, -1, -1], $
                     KS_stat:[-1.0, -1.0, -1.0, -1.0, -1.0], $
                     KS_prob:[-1.0, -1.0, -1.0, -1.0, -1.0], $
                     AD_stat:[-1.0, -1.0, -1.0, -1.0, -1.0], $
                     AD_crit:[-1.0, -1.0, -1.0, -1.0, -1.0], $
                     LB_stat:[-1.0, -1.0, -1.0, -1.0, -1.0], $
                     LB_chisqrd:[-1.0, -1.0, -1.0, -1.0, -1.0], $
                     num_signif_peaks:0, $
                     fft_length:-1, $
                     window_func:'not_applicable', $
                     window_param:-1.0, $
                     cpg:-1.0, $
                     signif_level:0.0, $
                     signif_test:'not_applicable', $
                     adjacent_peaks:allow_adj_pks, $
                     user_qual_flag:-1, $
                     auto_qual_flag:1}

    ;Initializing some array values
    result_th_fft.apod_window[0:-1] = 1.0
    result_th_fft.bin_flags[0:-1] = -2

    ;Recording some basic information
    stats_th_fft.fft_length = len_fft_array
    stats_th_fft.signif_level = signif_levels

    ;###############################################
    ;SORTING THREADS AND SKIPPING THE BAD ONES
    ;###############################################
    ;skips enteries with less than 2 positive values
    IF (n_elements(where(tpos gt 0.0))) GT 2. THEN BEGIN

        ;skips enteries where half the data points are set to zero, i.e. no
        ;value was obtained at fitting stage.
        IF (n_elements(where(tpos EQ 0.0))) LT 0.5*(n_elements(where(tpos GE 0.))) THEN BEGIN

            ;if value missing set to same as last pixel and set
            ;NOTE: this should not be needed if 'patch_up.pro' is used prior to calling this procedure
            zer = where(tpos EQ 0.0)
            IF zer[0] NE -1 THEN BEGIN
                FOR ii=0,n_elements(zer)-1 DO BEGIN
                    tpos[zer[ii]] = tpos[zer[ii]-1]
                    terr[zer[ii]] = 1.
                ENDFOR
            ENDIF

            ;###############################################
            ;FIT THE GOOD THREADS
            ;###############################################
            IF t_start ge 0 THEN BEGIN

                tpos = dx*tpos[t_start:t_end]
                terr = dx*terr[t_start:t_end]

                ; calculates linear trend or mean
                IF keyword_set(ctrend) THEN BEGIN
                    res = poly_fit(findgen(t_end-t_start+1), tpos, 1, yfit=trend) ;fit linear trend line
                ENDIF ELSE BEGIN
                    trend = fltarr(t_end-t_start+1) + mean(tpos)  ;calculates mean of data
                ENDELSE

                result_th_fft.trend = trend

                ;###############################################
                ;Perform the FFT and calculate the power and phase
                ;###############################################

                oscil = tpos-trend ;detrend (or just subtract the mean from) the time series

       		    s_len = n_elements(oscil) ;length of the source, unpadded time-series array
       		    sd = s_len*dt ;total duration of the series (not currently used)
       		    apodt = get_apod_window(s_len, cpg, parameter=window_param, win_function=window_func)
                ;note: cpg is Coherent Power Gain - correction factor needed for power after apodisation

                result_th_fft.apod_window = apodt
                stats_th_fft.window_func = window_func
                stats_th_fft.window_param = window_param

                ;applying window and padding the array (if asked to)
                IF KEYWORD_SET(bootstrap) THEN BEGIN
                    ;bootstrapping the input data. Returns 2D array with dimensions of [s_len, num_bootstrap]
                    boot_arr = bootstrap_1d_data(oscil, terr, num_resamples = num_bootstrap)

           		    IF NOT keyword_set(smpad) THEN BEGIN
                        oscil = oscil*apodt
                        boot_arr = boot_arr*rebin(apodt, s_len, num_bootstrap)
                  	    n_len = s_len
                    ENDIF ELSE BEGIN
                        ; oscil = smooth(boot_arr, [3, 1], /edge_truncate)
                        oscil = [oscil*apodt , fltarr(num_pad_fft)]
                        boot_arr = smooth(boot_arr, [3, 1], /edge_truncate)
                      	boot_arr = [boot_arr*rebin(apodt, s_len, num_bootstrap), fltarr(num_pad_fft, num_bootstrap)]
                      	n_len = s_len + num_pad_fft
           		    ENDELSE
                ENDIF ELSE BEGIN
           		    IF NOT keyword_set(smpad) THEN BEGIN
                        oscil = oscil*apodt
                  	    n_len = n_elements(oscil)
                    ENDIF ELSE BEGIN
                        ; oscil = smooth(oscil,3, /edge_truncate)
                      	oscil = [oscil*apodt, fltarr(num_pad_fft)]
                      	n_len = n_elements(oscil)
           		    ENDELSE
                ENDELSE

                IF keyword_set(include_nyquist) THEN BEGIN
                    end_fft_index = ceil((n_len-1)/2.0) ;last pos freq result in fft (INCLUDING Nyquist freq for even N)
                ENDIF ELSE BEGIN
                    end_fft_index = floor((n_len-1)/2.0) ;last pos freq result in fft (EXCLUDING Nyquist freq for even N)
                ENDELSE
                df = 1.0/(n_len*dt)
                f = findgen(n_len)*df
                f = f[1:end_fft_index]

                ;Take FFT and then calculate power (PSD) and phase
                IF KEYWORD_SET(bootstrap) THEN BEGIN
                    fft_of_boot = fft(boot_arr, -1, dimension=1)
                    boot_pow = (2.0*(abs(fft_of_boot))^2)[1:end_fft_index,*]
                    boot_phase = atan((fft_of_boot)[1:end_fft_index,*], /phase)

                    pow = mean(boot_pow, dimension=2)
                    phase = mean(boot_phase, dimension=2)
                    pow_err = stddev(boot_pow, dimension=2)
                    phase_err = stddev(boot_phase, dimension=2)
                ENDIF ELSE BEGIN
                    fft_of_oscill = fft(oscil, -1)
                    pow = (2.0*(abs(fft_of_oscill))^2)[1:end_fft_index]
                    phase = atan((fft_of_oscill)[1:end_fft_index], /phase)
                ENDELSE

                power_correction = n_len/(s_len*CPG) ;Assumes power is evenly distributed in apod window (not always true)

                IF end_fft_index LT (nf - 1) THEN BEGIN
                    result_th_fft.bin_flags[end_fft_index-1:-1] = -1 ;flag fft bins outside results with a value of -1
                ENDIF

                ;###############################################
                ;Calculates significance level of the FFT signal
                ; (see e.g., Torrence & Compo 1998 and Vaughan 2005)
                ;###############################################
        	    ;Doesn't let straight lines through (i.e. fft results with very low varience)
       		    IF ((moment(pow))[1]) GT 1e-9 THEN BEGIN

                    ;Computing various results and saving to the output structure
                    result_th_fft.power = pow*(power_correction^2)
                    result_th_fft.amplitude = 2.0*sqrt(pow/2.0)*power_correction
           			result_th_fft.freq = f
                    result_th_fft.phase = phase
                    stats_th_fft.cpg = CPG

                    IF KEYWORD_SET(bootstrap) THEN BEGIN
                        result_th_fft.err_power = pow_err*(power_correction^2)
                        result_th_fft.err_amplitude = 2.0*sqrt(pow/2.0)*power_correction
                        result_th_fft.err_phase = phase_err
                    ENDIF

                    ;Significant tests for power spectra
                    ;Default is Torrence & Compo 1998
                    ;Alternative is Vaughan 2005
         			IF keyword_set(vaughan) THEN BEGIN
                        stats_th_fft.signif_test = 'Vaughan_2005'
           			    ;Normalise power
         				npw = s_len*pow/(moment(oscil))[1]
            			prob = 1.0 - chisqr_pdf(2.0*npw,2)
            			nprob = MAKE_ARRAY(SIZE(prob, /DIM))
            			log_pp = s_len * ALOG(DOUBLE(1.0 - prob))
            			indx = WHERE(log_pp gt -30.0, count, COMPLEMENT=indx_c)

                        IF (count gt 0) THEN nprob[indx] = exp(log_pp[indx])
            			IF (count lt s_len) THEN nprob[indx_c] = 0.0

                        result_th_fft.signif_vals = nprob
                        loc_signif_pow = where(nprob gt signif_levels, /NULL)
         			ENDIF ELSE BEGIN
                        stats_th_fft.signif_test = 'Torrence_&_Compo_1998'
            			; sig_vals = SIGNIF_CONF(oscil, signif_levels)
                        sig_vals = SIGNIF_NOISE_SPEC(oscil, signif_levels, n_elements(f), color='white')
                        ; sig_vals = SIGNIF_NOISE_SPEC_BOOTSTRAP(boot_arr, signif_levels, n_elements(f))
                        ; boot_sig_vals = SIGNIF_NOISE_SPEC(mean(boot_arr, dimension=2), signif_levels, n_elements(f))
                        ; print, 'thread #', h, ' sig val diff.'
                        ; print, boot_sig_vals - sig_vals
                        result_th_fft.signif_vals = sig_vals*(power_correction^2)
            			loc_signif_pow = where(pow gt sig_vals, /NULL)
        			ENDELSE

                    ; Flagging significant power bins
                    result_th_fft.bin_flags[0:end_fft_index-1] = 0 ;fill all valid fft bin flags with a value of 0
                    result_th_fft.bin_flags[loc_signif_pow] = 1 ;flag significant results with a quality value of 1

                    ;Finding peaks in the significant power results (NEW METHOD AS OF JUNE 2016)
                    temp_pow = result_th_fft.power[0:-1] ;temporary array for finding peaks
                    temp_pow[where(result_th_fft.bin_flags LT 1, /NULL)] = 0 ;clear out values that are not significant
                    compare_right = temp_pow - SHIFT(temp_pow, -1)
                    compare_right[-1] = 0 ;ignores the value to the "right" of the last value in the array
                    compare_left = temp_pow - SHIFT(temp_pow, 1)
                    compare_left[0] = 0 ;ignores the value to the "left" of the last value in the array
                    IF allow_adj_pks EQ 'no' THEN BEGIN
                        loc_signif_peaks = where(temp_pow GT 0 and (compare_right GE 0 and compare_left GE 0), /NULL)
                    ENDIF ELSE BEGIN
                        loc_signif_peaks = where(temp_pow GT 0, /NULL)
                    ENDELSE

                    result_th_fft.bin_flags[loc_signif_peaks] = 2 ;flag local peaks in the significant results with a value of 2

                    ;Saving the wave parameters of the largest significant peaks
                    ;note: will only save the FOUR largest peaks, even if there are more
                    stats_th_fft.num_signif_peaks = n_elements(loc_signif_peaks)
                    IF stats_th_fft.num_signif_peaks GT 0.0 THEN BEGIN
                        ;sort based on the power at each peak
         			    pow_at_signif_peaks = result_th_fft.power[loc_signif_peaks]
                        sorted_indices = REVERSE(SORT(pow_at_signif_peaks)) ;sort values high to low
                        jj = 0
                        WHILE jj LT 4 DO BEGIN
                            bin_of_peak = loc_signif_peaks[sorted_indices[jj]]
                            stats_th_fft.peak_power[jj] = result_th_fft.power[bin_of_peak]
                            stats_th_fft.peak_amplitude[jj] = result_th_fft.amplitude[bin_of_peak]
                            stats_th_fft.peak_freq[jj] = f[bin_of_peak]
                            stats_th_fft.peak_phase[jj] = phase[bin_of_peak]
                            stats_th_fft.peak_bin[jj] = bin_of_peak
                            IF KEYWORD_SET(bootstrap) THEN BEGIN
                                stats_th_fft.err_peak_power[jj] = result_th_fft.err_power[bin_of_peak]
                                stats_th_fft.err_peak_amplitude[jj] = result_th_fft.err_amplitude[bin_of_peak]
                                stats_th_fft.err_peak_phase[jj] = phase_err[bin_of_peak]
                            ENDIF
                            jj = jj + 1 ;count number of wave results saved so far
                            IF jj EQ stats_th_fft.num_signif_peaks THEN jj = 9001
                        ENDWHILE
                    ENDIF

                    ; [EXPERIMENTAL] Recoving leaked PSD
                    ; note: this will ONLY work if adjacent peaks are disallowed
                    ;       currently only set to recover power from directly adjacent bins
                    IF KEYWORD_SET(recover_power) AND allow_adj_pks EQ 'no' THEN BEGIN
                        print, 'Attempting to recover leaked power in FFT!'
                        FOR w=0, 3 DO BEGIN
                            center_bin = stats_th_fft.peak_bin[w]
                            IF center_bin GT -1 THEN BEGIN
                                left_bin = center_bin - 1
                                right_bin = center_bin + 1
                                ; cap bins to end of array
                                IF left_bin LT 0 THEN left_bin = 0
                                IF right_bin GT (nf-1) THEN right_bin = nf-1
                                num_recover_bins = 1 + right_bin - left_bin

                                ; Get weights for sum (will share power between peaks adjacent to same bin)
                                sum_weights = fltarr(num_recover_bins) + 1.0
                                IF center_bin GE 2 THEN BEGIN
                                    IF result_th_fft.bin_flags[left_bin-1] EQ 2 THEN sum_weights[0] = 0.5
                                ENDIF
                                IF center_bin LE (nf-3) THEN BEGIN
                                    IF result_th_fft.bin_flags[right_bin+1] EQ 2 THEN sum_weights[-1] = 0.5
                                ENDIF

                                ; sum power, average freq, and average phase
                                tot_power = total(result_th_fft.power[left_bin:right_bin]*sum_weights)
                                mean_weights = (result_th_fft.power[left_bin:right_bin]/tot_power)*sum_weights
                                mean_freq = total(f[left_bin:right_bin]*mean_weights)/total(mean_weights)
                                mean_phase= total(phase[left_bin:right_bin]*mean_weights)/total(mean_weights)

                                ; recalc amplitude (should already have the correct power correction)
                                tot_amp = 2.0*sqrt(tot_power/2.0)

                                ; overwrite the saved results with these adjusted values
                                stats_th_fft.peak_power[w] = tot_power
                                stats_th_fft.peak_amplitude[w] = tot_amp
                                stats_th_fft.peak_freq[w] = mean_freq
                                stats_th_fft.peak_phase[w] = mean_phase
                            ENDIF
                        ENDFOR
                    ENDIF

                    ; GOF tests of residuals
                    combined_wave_vals = fltarr(s_len)
                    total_fit_params = 0
                    ; note: the results for the first N waves combined is stored in the i = N position.
                    ;       In other words, the 1st result (index 0) is the stats for NO waves,
                    ;       the 2nd (index 1) results is for the first wave, the 3rd (index 2)
                    ;       is for the first TWO waves inclusive, and so on and so forth.
                    FOR w=0, min([stats_th_fft.num_signif_peaks, 4]) DO BEGIN
                        IF w GT 0 AND stats_th_fft.peak_bin[w-1] GT -1 THEN BEGIN
                            add_wave = TEST_FFT_WAVE_FUNCTION(stats_th_fft.peak_amplitude[w-1], stats_th_fft.peak_freq[w-1], $
                                                              stats_th_fft.peak_phase[w-1], s_len)
                            combined_wave_vals = combined_wave_vals + add_wave
                            total_fit_params += 3
                        ENDIF
                        residuals = tpos - (combined_wave_vals + trend)
                        resid_stats = test_residuals(residuals, num_fit_params=total_fit_params, $
                                                     signif_level=stats_th_fft.signif_level)
                        stats_th_fft.KS_stat[w] = resid_stats.KS_stat
                        stats_th_fft.KS_prob[w] = resid_stats.KS_prob
                        stats_th_fft.AD_stat[w] = resid_stats.AD_stat
                        stats_th_fft.AD_crit[w] = resid_stats.AD_crit
                        stats_th_fft.LB_stat[w] = resid_stats.LB_stat
                        stats_th_fft.LB_chisqrd[w] = resid_stats.LB_chisqrd
                    ENDFOR

       		    ENDIF
            ENDIF
        ENDIF
    ENDIF

    ;Add the fft results to output LIST of structures (with variable size array tags)
    fft_results.ADD, result_th_fft

    ;Create or concat fft stats to output ARRAY of structures (with fixed size and shape)
    IF h EQ 0 THEN BEGIN
        fft_stats = [stats_th_fft]
    ENDIF ELSE BEGIN
        fft_stats = [temporary(fft_stats), stats_th_fft]
    ENDELSE
ENDFOR
END
