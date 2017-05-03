;+
;NAME: PLOT_NUWT_COMPARE_RUNS
;
;PURPOSE:
;   Plots a number of histograms and scatterplots comparing the ouput of NUWT to
;   either input simulated wave parameters or another run of NUWT
;
;INPUTS:
;   ref_threads - stucture containing the locations of the reference waves. Can
;                 accept either the coords of the input simulated wave parameters (from
;                 'generate_kink_waves.pro') or the 'threads' output from a different
;                 run of NUWT.
;   ref_waves - stucture containing either input simulated wave parameters or the
;               'fft_stats' output from a different run of NUWT.
;   Note: additional input data is provided by COMMON blocks
;
;OPTIONAL INPUTS:
;   res - spatial resolution of data (in arcsec). Default is 1
;   cad - temporal cadence of the data (in s). Default is 1
;   /simulated - if set, will assume the ref_waves structure contains input
;                simulated parameters (default). Note: since the 'sim_waves'
;                structures contains both the simulated thread coords and the
;                wave parameters, you need to only pass the structure once to
;                either ref_threads OR ref_waves and the program will sort it out.
;   /nuwt - if set, will assume the ref_waves structure contains output from
;           a seperate NUWT run. Note: if both /simulated and /nuwt are set, the
;           program will give precedence to the /simulated option.
;   max_pos_offset - maximum offset (in pixels) between the test and ref threads.
;                    Used to assist in correcting pairing threads in noisy data.
;   max_bin_offset - maximum offset (in timesteps) between the test and ref threads.
;   VVV_range - range to plot for amp, period, vel_amp, phase, thread length (th_len),
;               & number of observed cycles (num_cyc). Defaults are [0,2000], [0,2000],
;               [0,50], [-3.5, 3.5], [0,150], [0,3] respectively.
;   VVV_nbins - number of histogram bins for amp, period, vel_amp, phase, th_len,
;               & num_cyc. Defaults are max_amp/25, max_period/25, & max_vel_amp/1.0,
;               2*max_phase/0.25, max_th_len/5, & max_num_cyc/0.125 respectivly
;   plot_dist_units - string indicating what units to use for distance in the plot
;                     labels. Defaults to units of 'pixels' unless 'res' is set,
;                     in which case the default is 'arcsec'
;   plot_time_units - string indicating what units to use for time in the plot
;                     labels. Defaults to units of 'steps' unless 'cad' is set,
;                     in which case the default is 's'
;   /final_units - if set, will assume wave parameters are already in the correct
;                  units of [km] for amplitude and [s^-1] for frequency
;   /screen - if set, will plot to the screen rather than a file
;   /use_temp_common_blocks - if set, will use the temporary common blocks rather
;                             than the all_nuwt_dat block. Use with care!
;   slitnum - virtual slit number for header text. By defualt assumes 1
;   header - custom header text. Useful for identifying the source data.
;            Defaults to 'NUWT FFT stats'
;   run_tag - custom string that will be appended to the end of the header.
;             Normally used to keep track of differnt runs of the same dataset.
;             There is no default run_tag string
;   save_folder - folder in which to save the plots. Defaults to the user's home folder.
;                 Will also append a '/' to the end if not included.
;   filename - name for the output PDF file. Default is 'NUWT_selected_peaks'
;
;OUTPUTS:
;   PDF_file - multi-page PDF containing (1) the input td-diagram, (2) selected
;              peaks, (3) threads found, and (4) histograms of gradients
;
;HISTORY: Name---------Date---------Description
;         M Weberg   OCT, 2016  Initial coding.
;
;TO-DO / LIMITATIONS:
;   - Add inputs for metadata indicating the data and source of observations.
;-

PRO PLOT_NUWT_COMPARE_RUNS, ref_threads=ref_threads, ref_waves=ref_waves, $
            res=res, cad=cad, simulated=simulated, nuwt=nuwt, $
            max_pos_offset=max_pos_offset, max_bin_offset=max_bin_offset,$
            amp_range=amp_range, amp_nbins=amp_nbins, $
            period_range=period_range, period_nbins=period_nbins, $
            vel_amp_range=vel_amp_range, vel_amp_nbins=vel_amp_nbins, $
            phase_range=phase_range, phase_nbins=phase_nbins, $
            th_len_range=th_len_range, th_len_nbins=th_len_nbins, $
            num_cyc_range=num_cyc_range, num_cyc_nbins=num_cyc_nbins, $
            free_num_cyc_limits=free_num_cyc_limits, $
            plot_dist_units=plot_dist_units, plot_time_units=plot_time_units, $
            final_units=final_units, screen=screen, $
            use_temp_common_blocks=use_temp_common_blocks, $
            slitnum=slitnum, header=header, run_tag=run_tag, $
            save_folder=save_folder, filename=filename, $
            paired_out=paired_out, ref_out=ref_out, test_out=test_out

;###############################################################################
;SETTING DEFAULT VALUES
;###############################################################################
IF NOT KEYWORD_SET(simulated) AND NOT KEYWORD_SET(nuwt) THEN simulated = 1 ;default
IF KEYWORD_SET(simulated) AND KEYWORD_SET(nuwt) THEN nuwt = 0 ;simulated takes precedence

;Checking for the required input variables
IF KEYWORD_SET(nuwt) THEN BEGIN
    IF (n_elements(ref_threads) LT 1) OR (n_elements(ref_waves) LT 1) THEN BEGIN
        MESSAGE, 'ERROR: must input BOTH ref_threads and ref_waves when comparing two NUWT runs.'
    ENDIF
ENDIF ELSE BEGIN
    ;default for simulated waves
    IF (n_elements(ref_threads) LT 1) AND (n_elements(ref_waves) LT 1) THEN BEGIN
        MESSAGE, 'ERROR: must input EITHER ref_threads or ref_waves when comparing to a simulation.'
    ENDIF
ENDELSE

;Note: Simulated data stores both thread location and wave values in same structure
IF KEYWORD_SET(simulated) AND NOT KEYWORD_SET(ref_waves) THEN ref_waves = ref_threads
IF KEYWORD_SET(simulated) AND NOT KEYWORD_SET(ref_threads) THEN ref_threads = ref_waves

IF n_elements(max_pos_offset) EQ 0 THEN max_pos_offset = 5
IF n_elements(max_bin_offset) EQ 0 THEN max_bin_offset = 5

IF KEYWORD_SET(res) AND KEYWORD_SET(cad) THEN BEGIN
    ;defaults for units of [km] and [s]
    IF NOT KEYWORD_SET(amp_range) THEN amp_range = [0.0, 2000.0]
    IF NOT KEYWORD_SET(period_range) THEN period_range = [0.0, 2000.0]
    IF NOT KEYWORD_SET(vel_amp_range) THEN vel_amp_range = [0.0, 50.0]
    IF NOT KEYWORD_SET(amp_nbins) THEN amp_nbins = amp_range[1]/25
    IF NOT KEYWORD_SET(period_nbins) THEN period_nbins = amp_range[1]/25
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

;variables indifferent to units
IF NOT KEYWORD_SET(phase_range) THEN phase_range = [-3.5, 3.5]
IF NOT KEYWORD_SET(th_len_range) THEN th_len_range = [0.0, 150.0]
IF NOT KEYWORD_SET(num_cyc_range) THEN num_cyc_range = [0.0, 5.0]
IF NOT KEYWORD_SET(phase_nbins) THEN phase_nbins = 2*phase_range[1]/0.25
IF NOT KEYWORD_SET(th_len_nbins) THEN th_len_nbins = th_len_range[1]/5
IF NOT KEYWORD_SET(num_cyc_nbins) THEN num_cyc_nbins = num_cyc_range[1]/0.2

IF n_elements(slitnum) EQ 0 THEN slitnum = 0
IF NOT KEYWORD_SET(header) THEN header = 'NUWT wave results (Test) vs Input simulated waves (Reference)'
IF KEYWORD_SET(run_tag) THEN header = header + ' - '+run_tag
IF NOT KEYWORD_SET(filename) THEN filename = 'NUWT_compare_runs'
IF NOT KEYWORD_SET(save_folder) THEN save_folder = '' ;i.e. defaults to home folder
IF strlen(save_folder) GT 0 AND NOT save_folder.endswith('/') THEN save_folder = save_folder+'/'

;Colors to help differentiate wave degeneracy (i.e. number of TEST threads assigned to the same REF thread)
degen_color_list = ['violet', 'dodger blue', 'goldenrod', 'crimson', 'dark red']
;note: a degeneracy of '0' indicates multiple REF threads connected within the same TEST thread

;Shapes to indicate number of significant TEST waves (i.e. peaks in FFT spectrum)
wave_shape_list = ['plus', 'circle', 'triangle', 'square', 'star']

ref_color_list = ['dark orange', 'green']
ref_shape_list = ['diamond', 'diamond']

;Colors to help differentiate threads
th_color_list = ['white', 'dark grey', 'light coral', 'crimson', 'gold', 'light green', $
                 'forest green', 'aqua', 'dodger blue', 'violet', 'dark violet']

;###############################################################################
;LOADING DATA AND SELECTING CORRECT SET OF RESULTS
;###############################################################################
IF NOT KEYWORD_SET(use_temp_common_blocks) THEN BEGIN
    ;Default and preferred data source
    COMMON all_nuwt_dat, nuwt_located, nuwt_threads, nuwt_fft_results, nuwt_fft_stats

    IF slitnum GT n_elements(nuwt_located) THEN BEGIN
        last_slit = n_elements(nuwt_located)-1
        print, 'WARNING! slit number '+strtrim(slitnum, 2)+' does not exist!'
        print, '   slitnum set to '+strtrim(last_slit, 2)+' (last set of results)'
        slitnum = last_slit
    ENDIF

    slit_located = nuwt_located[slitnum]
    slit_threads = nuwt_threads[slitnum]
    slit_fft_results = nuwt_fft_results[slitnum]
    slit_fft_stats = nuwt_fft_stats[slitnum]
ENDIF ELSE BEGIN
    ;Used for running NUWT in interactive mode. NOT recommended unless you are careful
    COMMON located_dat, located, nx, nt
    COMMON threads_dat, threads
    COMMON fft_results_dat, fft_results, fft_stats

    slit_located = located
    slit_threads = threads
    slit_fft_results = fft_results
    slit_fft_stats = fft_stats
ENDELSE

n_threads = n_elements(slit_threads)
IF KEYWORD_SET(nuwt) THEN n_ref = n_elements(ref_waves) ELSE n_ref = n_elements(ref_waves.phase)
num_th_colors = n_elements(th_color_list)

sz = size(slit_located.peaks)
nx = sz[1]
nt = sz[2]

;###############################################################################
;UNIT CONVERSIONS AND LABELS
;###############################################################################
km_per_arcsec = 725.27
dx = 1.0
dt = 1.0
amp_dx = 1.0
freq_dt = 1.0
amp_units = 'km'
period_units = 's'
freq_units = 's^{-1}'

IF NOT keyword_set(plot_dist_units) THEN plot_dist_units = 'arcsec'
IF NOT keyword_set(plot_time_units) THEN plot_time_units = 's'
IF NOT keyword_set(res) THEN BEGIN
    res = 1.0
    amp_units = 'pixels'
    plot_dist_units = 'pixels'
ENDIF
IF NOT keyword_set(cad) THEN BEGIN
    cad = 1.0
    period_units = 'timestep'
    plot_time_units = 'steps'
ENDIF

IF plot_dist_units EQ 'arcsec' THEN dx = res
IF plot_dist_units EQ 'Mm' THEN dx = res*km_per_arcsec/1000.0
IF plot_time_units EQ 's' THEN dt = cad
IF plot_time_units EQ 'min' THEN dt = cad/60.0

IF NOT keyword_set(final_units) AND amp_units NE 'pixels' THEN BEGIN
    amp_dx = res*km_per_arcsec
    freq_dt = 1.0/cad
    convert_to_pxls = 1.0
ENDIF ELSE BEGIN
    convert_to_pxls = 1.0/res*km_per_arcsec
ENDELSE

;###############################################################################
;EXTRACTING REFERENCE DATA TO STANDARDIZED STRUCTURES
;###############################################################################
;Extract reference wave start positions and bins (in pixels)
ref_coords = {start_pos:fltarr(n_ref), end_pos:fltarr(n_ref), $
              min_pos:fltarr(n_ref), max_pos:fltarr(n_ref), $
              start_bin:intarr(n_ref), end_bin:intarr(n_ref), $
              length:intarr(n_ref), paired_index:intarr(n_ref), $
              paired_flag:intarr(n_ref)}
ref_coords.paired_index[0:-1] = -1
IF KEYWORD_SET(nuwt) THEN BEGIN
    ;Results from an alternate run of 'run_nuwt_with_fft.pro'
    ref_coords.start_bin = ref_threads.start_bin
    ref_coords.end_bin = ref_threads.end_bin
    ref_coords.length = ref_threads.length
    FOR rr=0L, (n_ref-1) DO BEGIN
        ref_coords.start_pos[rr] = ref_threads[rr].pos[ref_coords.start_bin[rr]]
        ref_coords.end_pos[rr] = ref_threads[rr].pos[ref_coords.end_bin[rr]]
    ENDFOR
    min_start_pos = ref_coords.start_pos - ref_waves.peak_amplitude[0]
    max_start_pos = ref_coords.start_pos + ref_waves.peak_amplitude[0]
    min_end_pos = ref_coords.end_pos - ref_waves.peak_amplitude[0]
    max_end_pos = ref_coords.end_pos + ref_waves.peak_amplitude[0]
    ref_coords.min_pos = min_start_pos < min_end_pos
    ref_coords.max_pos = max_start_pos > max_end_pos
ENDIF ELSE BEGIN
    ;Simulated data from 'generate_kink_waves.pro'
    ref_coords.start_pos = ref_threads.start_dist/res
    ref_coords.min_pos = reform(ref_threads.box_coords[0,*])
    ref_coords.max_pos = reform(ref_threads.box_coords[2,*])
    ref_coords.end_pos = (ref_coords.max_pos - ref_coords.min_pos)/2
    ref_coords.start_bin = reform(ref_threads.box_coords[1,*])
    ref_coords.end_bin = reform(ref_threads.box_coords[3,*])
    ref_coords.length = ref_coords.end_bin - ref_coords.start_bin + 1
ENDELSE

;Extract wave parameters
ref_params = {amplitude:fltarr(n_ref), period:fltarr(n_ref), phase:fltarr(n_ref), $
              velocity_amp:fltarr(n_ref), mean:fltarr(n_ref), slope:fltarr(n_ref), $
              num_cyc:fltarr(n_ref), num_waves:fltarr(n_ref)}
IF KEYWORD_SET(nuwt) THEN BEGIN
    ;Results from an alternate run of 'run_nuwt_with_fft.pro'
    ref_params.amplitude = ref_waves.peak_amplitude[0]*amp_dx
    ref_params.period = 1.0/(ref_waves.peak_freq[0]*freq_dt)
    ref_params.velocity_amp = 2*!PI*ref_params.amplitude/ref_params.period
    ref_params.num_cyc = (ref_coords.length*cad)/ref_params.period
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
    ref_params.num_cyc = (ref_coords.length*cad)/ref_params.period
    ref_params.phase = ref_waves.phase
    loc_shift_phase = where(ref_params.phase GT !PI, /NULL)
    IF n_elements(loc_shift_phase) GT 0 THEN BEGIN
        ;Adjust phase to be in the range of -pi to +pi (as returned by fft)
        ref_params.phase[loc_shift_phase] = ref_params.phase[loc_shift_phase] - 2*!PI
    ENDIF
    ref_params.num_waves[0,-1] = 1
ENDELSE

;###############################################################################
;EXTRACTING TEST DATA TO STANDARDIZED STRUCTURES
;###############################################################################
;Extract start positions and bins of each test thread
test_coords = {start_pos:fltarr(n_threads), end_pos:fltarr(n_threads), $
               min_pos:fltarr(n_threads), max_pos:fltarr(n_threads), $
               start_bin:intarr(n_threads), end_bin:intarr(n_threads), $
               length:intarr(n_threads), paired_index:intarr(n_threads)}
test_coords.paired_index[0:-1] = -1
test_coords.start_bin = slit_threads.start_bin
test_coords.end_bin = slit_threads.end_bin
test_coords.length = slit_threads.length
FOR h=0L, (n_threads-1) DO BEGIN
    test_coords.start_pos[h] = slit_threads[h].pos[test_coords.start_bin[h]]
    test_coords.end_pos[h] = slit_threads[h].pos[test_coords.end_bin[h]]
ENDFOR
min_start_pos = test_coords.start_pos - slit_fft_stats.peak_amplitude[0]*convert_to_pxls
max_start_pos = test_coords.start_pos + slit_fft_stats.peak_amplitude[0]*convert_to_pxls
min_end_pos = test_coords.end_pos - slit_fft_stats.peak_amplitude[0]*convert_to_pxls
max_end_pos = test_coords.end_pos + slit_fft_stats.peak_amplitude[0]*convert_to_pxls
test_coords.min_pos = min_start_pos < min_end_pos
test_coords.max_pos = max_start_pos > max_end_pos


;Peak wave fft results from a run of 'run_nuwt_with_fft.pro'
test_params = {amplitude:fltarr(n_threads), period:fltarr(n_threads), phase:fltarr(n_threads), $
               velocity_amp:fltarr(n_threads), mean:fltarr(n_threads), slope:fltarr(n_threads), $
               num_cyc:fltarr(n_threads), num_waves:fltarr(n_threads), $
               user_qual_flag:intarr(n_threads), auto_qual_flag:intarr(n_threads)}
test_params.amplitude = slit_fft_stats.peak_amplitude[0]*amp_dx
test_params.period = 1.0/(slit_fft_stats.peak_freq[0]*freq_dt)
test_params.velocity_amp = 2*!PI*test_params.amplitude/test_params.period
test_params.phase = slit_fft_stats.peak_phase[0]
test_params.num_cyc = (test_coords.length*cad)/test_params.period
test_params.num_waves = slit_fft_stats.num_signif_peaks
test_params.user_qual_flag = slit_fft_stats.user_qual_flag
test_params.auto_qual_flag = slit_fft_stats.auto_qual_flag

;fixing periods of INF and NaN
loc_nan = where(~finite(test_params.period))
test_params.period[loc_nan] = 0.0
test_params.velocity_amp[loc_nan] = 0.0

;###############################################################################
;PAIRING NUWT LOCATED THREADS WITH SIMULATED DATA
;###############################################################################
;Pairing threads
paired_indices = {test:indgen(n_threads), ref:intarr(n_threads), $
                  degen:intarr(n_threads), num_ref:intarr(n_threads)}
FOR h=0L, (n_threads-1) DO BEGIN
    ; Search in a capital "I" shaped box (seems to give more accurate pairing)
    loc_nearest_ref = where(((ref_coords.min_pos - max_pos_offset LE test_coords.start_pos[h]) AND $
                             (ref_coords.max_pos + max_pos_offset GE test_coords.start_pos[h]) AND $
                             (ref_coords.start_bin - max_bin_offset LE test_coords.start_bin[h]) AND $
                             (ref_coords.start_bin + max_bin_offset GE test_coords.start_bin[h])) OR $
                            ((ref_coords.min_pos LE test_coords.start_pos[h]) AND $
                             (ref_coords.max_pos GE test_coords.start_pos[h]) AND $
                             (ref_coords.start_bin LE test_coords.start_bin[h]) AND $
                             (ref_coords.end_bin GE test_coords.start_bin[h])) OR $
                            ((ref_coords.min_pos - max_pos_offset LE test_coords.end_pos[h]) AND $
                             (ref_coords.max_pos + max_pos_offset GE test_coords.end_pos[h]) AND $
                             (ref_coords.end_bin - max_bin_offset LE test_coords.end_bin[h]) AND $
                             (ref_coords.end_bin + max_bin_offset GE test_coords.end_bin[h])))

    ;pick the nearest ref wave to the start of test thread, if none is found will return -1 (crude but works)
    paired_indices.ref[h] = loc_nearest_ref[0]
    IF loc_nearest_ref[0] NE -1 THEN ref_coords.paired_index[loc_nearest_ref[0]] = h

    ;Search in an upside-down "T" shaped box
    loc_all_connected_ref = where(((ref_coords.start_pos GE test_coords.min_pos[h]) AND $
                                   (ref_coords.start_pos LE test_coords.max_pos[h]) AND $
                                   (ref_coords.start_bin GE test_coords.start_bin[h] - max_bin_offset) AND $
                                   (ref_coords.start_bin LE test_coords.end_bin[h])) OR $
                                  ((ref_coords.end_pos GE test_coords.min_pos[h]) AND $
                                   (ref_coords.end_pos LE test_coords.max_pos[h]) AND $
                                   (ref_coords.end_bin GE test_coords.start_bin[h] - max_bin_offset) AND $
                                   (ref_coords.end_bin LE test_coords.end_bin[h])), /NULL)

    ;count number of ref threads that might be stiched together to make the one test thread
    IF n_elements(loc_all_connected_ref) GT 0 THEN BEGIN
        paired_indices.num_ref[h] = n_elements(loc_all_connected_ref)
        ref_coords.paired_index[loc_all_connected_ref] = h
    ENDIF
ENDFOR
test_coords.paired_index = paired_indices.ref

;Determine degeneracy (i.e. number of TEST threads paired to the same REF thread)
FOR h=0L, (n_threads-1) DO BEGIN
    IF paired_indices.ref[h] EQ -1 THEN BEGIN
        ;unpaired TEST threads
        paired_indices.degen[h] = -1
    ENDIF ELSE BEGIN
        IF paired_indices.num_ref[h] GT 1 THEN BEGIN
            ;multiple REF threads connected to same TEST thread
            paired_indices.degen[h] = 0
        ENDIF ELSE BEGIN
            ;one (or more) TEST threads connected to same REF thread
            loc_same_ref = where(paired_indices.ref EQ paired_indices.ref[h])
            num_same_ref = n_elements(loc_same_ref)
            paired_indices.degen[h] = num_same_ref
        ENDELSE
    ENDELSE
ENDFOR

loc_ref_paired = where(ref_coords.paired_index GE 0, /NULL)
loc_ref_unpaired = where(ref_coords.paired_index EQ -1, /NULL)
ref_coords.paired_flag[loc_ref_paired] = 1

;Outputing structures for debugging
paired_out = paired_indices
ref_out = ref_coords
test_out = test_coords

;###############################################################################
;CALCULATING PARAMETER DIFFERENCES
;###############################################################################
param_diffs = {amplitude:fltarr(n_threads), period:fltarr(n_threads), phase:fltarr(n_threads), $
               velocity_amp:fltarr(n_threads), mean:fltarr(n_threads), slope:fltarr(n_threads), $
               num_cyc:fltarr(n_threads), length:intarr(n_threads)}

param_diffs.amplitude = test_params.amplitude - ref_params.amplitude[paired_indices.ref]
param_diffs.period = test_params.period - ref_params.period[paired_indices.ref]
param_diffs.velocity_amp = test_params.velocity_amp - ref_params.velocity_amp[paired_indices.ref]
param_diffs.phase = test_params.phase - ref_params.phase[paired_indices.ref]
param_diffs.length = test_coords.length - ref_coords.length[paired_indices.ref]
param_diffs.num_cyc = test_params.num_cyc - ref_params.num_cyc[paired_indices.ref]

;###############################################################################
;COUNTING NUMBER OF EVENTS IN DIFFERNT CATAGORIES
;###############################################################################
test_counts = {multi_ref:0, one_degen:0, two_degen:0, three_degen:0, more_degen:0, unpaired:0, $
               no_waves:0, one_wave:0, two_waves:0, three_waves:0, more_waves:0}
test_counts.multi_ref = n_elements(where(paired_indices.degen EQ 0, /NULL))
test_counts.one_degen = n_elements(where(paired_indices.degen EQ 1, /NULL))
test_counts.two_degen = n_elements(where(paired_indices.degen EQ 2, /NULL))
test_counts.three_degen = n_elements(where(paired_indices.degen EQ 3, /NULL))
test_counts.more_degen = n_elements(where(paired_indices.degen GE 4, /NULL))
test_counts.unpaired = n_elements(where(paired_indices.degen EQ -1, /NULL))

test_counts.no_waves = n_elements(where(test_params.num_waves LE 0, /NULL))
test_counts.one_wave = n_elements(where(test_params.num_waves EQ 1, /NULL))
test_counts.two_waves = n_elements(where(test_params.num_waves EQ 2, /NULL))
test_counts.three_waves = n_elements(where(test_params.num_waves EQ 3, /NULL))
test_counts.more_waves = n_elements(where(test_params.num_waves GE 4, /NULL))

ref_counts = {same_test:0, paired:0, unpaired:0}
ref_counts.paired = n_elements(loc_ref_paired)
ref_counts.unpaired = n_elements(loc_ref_unpaired)

;###############################################################################
;HISTOGRAMMING BOTH REF AND TEST DATA
;###############################################################################
ref_amp_hist = histogram(ref_params.amplitude, LOCATIONS=ref_amp_bins, $
                         nbins=amp_nbins, min=amp_range[0], max=amp_range[1])
ref_period_hist = histogram(ref_params.period, LOCATIONS=ref_period_bins, $
                            nbins=period_nbins, min=period_range[0], max=period_range[1])
ref_vel_amp_hist = histogram(ref_params.velocity_amp, LOCATIONS=ref_vel_amp_bins, $
                             nbins=vel_amp_nbins, min=vel_amp_range[0], max=vel_amp_range[1])
ref_phase_hist = histogram(ref_params.phase, LOCATIONS=ref_phase_bins, $
                           nbins=phase_nbins, min=phase_range[0], max=phase_range[1])
ref_th_len_hist = histogram(ref_coords.length, LOCATIONS=ref_th_len_bins, $
                            nbins=th_len_nbins, min=th_len_range[0], max=th_len_range[1])
ref_num_cyc_hist = histogram(ref_params.num_cyc, LOCATIONS=ref_num_cyc_bins, $
                             nbins=num_cyc_nbins, min=num_cyc_range[0], max=num_cyc_range[1])
ref_count_outliers = {amp:0, period:0, vel_amp:0, th_len:0, num_cyc:0}
ref_count_outliers.amp = n_elements(where(ref_params.amplitude GT amp_range[1], /NULL))
ref_count_outliers.period = n_elements(where(ref_params.period GT period_range[1], /NULL))
ref_count_outliers.vel_amp = n_elements(where(ref_params.velocity_amp GT vel_amp_range[1], /NULL))
ref_count_outliers.th_len = n_elements(where(ref_coords.length GT th_len_range[1], /NULL))
ref_count_outliers.num_cyc = n_elements(where(ref_params.num_cyc GT num_cyc_range[1], /NULL))

test_amp_hist = histogram(test_params.amplitude, LOCATIONS=test_amp_bins, $
                          nbins=amp_nbins, min=amp_range[0], max=amp_range[1])
test_period_hist = histogram(test_params.period, LOCATIONS=test_period_bins, $
                             nbins=period_nbins, min=period_range[0], max=period_range[1])
test_vel_amp_hist = histogram(test_params.velocity_amp, LOCATIONS=test_vel_amp_bins, $
                              nbins=vel_amp_nbins, min=vel_amp_range[0], max=vel_amp_range[1])
test_phase_hist = histogram(test_params.phase, LOCATIONS=test_phase_bins, $
                         nbins=phase_nbins, min=phase_range[0], max=phase_range[1])
test_th_len_hist = histogram(test_coords.length, LOCATIONS=test_th_len_bins, $
                          nbins=th_len_nbins, min=th_len_range[0], max=th_len_range[1])
test_num_cyc_hist = histogram(test_params.num_cyc, LOCATIONS=test_num_cyc_bins, $
                           nbins=num_cyc_nbins, min=num_cyc_range[0], max=num_cyc_range[1])
test_count_outliers = {amp:0, period:0, vel_amp:0, th_len:0, num_cyc:0}
test_count_outliers.amp = n_elements(where(test_params.amplitude GT amp_range[1], /NULL))
test_count_outliers.period = n_elements(where(test_params.period GT period_range[1], /NULL))
test_count_outliers.vel_amp = n_elements(where(test_params.velocity_amp GT vel_amp_range[1], /NULL))
test_count_outliers.th_len = n_elements(where(test_coords.length GT th_len_range[1], /NULL))
test_count_outliers.num_cyc = n_elements(where(test_params.num_cyc GT num_cyc_range[1], /NULL))

;###############################################################################
;MAKING PLOTS (so many plots...)
;###############################################################################

;##### PAGE 1 - SUMMARY STATS AND PLOTS #####
;--------------------------------------------
IF NOT KEYWORD_SET(screen) THEN BEGIN
    fig_summary = window(name='fig_summary', dimensions=[1200, 725], /buffer)
ENDIF ELSE BEGIN
    fig_summary = window(name='fig_summary', dimensions=[1200, 725])
ENDELSE

;Plotting histograms (two columns of three rows)
plt_amp_hist = barplot(test_amp_bins, test_amp_hist, histogram=1, $
                       color='black', fill_color='light grey', linestyle='-', $
                       position=[0.28, 0.69, 0.58, 0.90], /current, xrange=amp_range)
plt_amp_hist.xtitle = 'Amplitude [$'+amp_units+'$]'
plt_amp_hist.ytitle = 'Number of Events'

plt_period_hist = barplot(test_period_bins, test_period_hist, histogram=1, $
                          color='black', fill_color='light grey', linestyle='-', $
                          position=[0.28, 0.40, 0.58, 0.61], /current, xrange=v_range)
plt_period_hist.xtitle = 'Period [$'+period_units+'$]'
plt_period_hist.ytitle = 'Number of Events'

plt_vel_amp_hist = barplot(test_vel_amp_bins, test_vel_amp_hist, histogram=1, $
                           color='black', fill_color='light grey', linestyle='-', $
                           position=[0.28, 0.1, 0.58, 0.32], /current, xrange=vel_amp_range)
plt_vel_amp_hist.xtitle = 'Velocity Amplitude [$'+amp_units+' '+period_units+'^{-1}$]'
plt_vel_amp_hist.ytitle = 'Number of Events'

plt_phase_hist = barplot(test_phase_bins, test_phase_hist, histogram=1, $
                         color='black', fill_color='light grey', linestyle='-', $
                         position=[0.66, 0.69, 0.96, 0.90], /current, xrange=phase_range)
plt_phase_hist.xtitle = 'Phase [rad]'
plt_phase_hist.ytitle = 'Number of Events'

plt_th_len_hist = barplot(test_th_len_bins, test_th_len_hist, histogram=1, $
                          color='black', fill_color='light grey', linestyle='-', $
                          position=[0.66, 0.40, 0.96, 0.61], /current, xrange=th_len_range)
plt_th_len_hist.xtitle = 'Thread Length [timesteps]'
plt_th_len_hist.ytitle = 'Number of Events'

plt_num_cyc_hist = barplot(test_num_cyc_bins, test_num_cyc_hist, histogram=1, $
                           color='black', fill_color='light grey', linestyle='-', $
                           position=[0.66, 0.1, 0.96, 0.32], /current, xrange=num_cyc_range)
plt_num_cyc_hist.xtitle = 'Number of Cycles Observed'
plt_num_cyc_hist.ytitle = 'Number of Events'

plt_ref_amp_hist = plot(ref_amp_bins, ref_amp_hist, /histogram, $
                        color='dark red', linestyle='-', overplot=plt_amp_hist)

plt_ref_period_hist = plot(ref_period_bins, ref_period_hist, /histogram, $
                           color='dark red', linestyle='-', overplot=plt_period_hist)

plt_ref_vel_amp_hist = plot(ref_vel_amp_bins, ref_vel_amp_hist, /histogram, $
                            color='dark red', linestyle='-', overplot=plt_vel_amp_hist)

plt_ref_phase_hist = plot(ref_phase_bins, ref_phase_hist, /histogram, $
                          color='dark red', linestyle='-', overplot=plt_phase_hist)

plt_ref_th_len_hist = plot(ref_th_len_bins, ref_th_len_hist, /histogram, $
                           color='dark red', linestyle='-', overplot=plt_th_len_hist)

plt_ref_num_cyc_hist = plot(ref_num_cyc_bins, ref_num_cyc_hist, /histogram, $
                            color='dark red', linestyle='-', overplot=plt_num_cyc_hist)

; Outlier count tags
FOREACH VAR, LIST(LIST(test_count_outliers.amp, 0.565, 0.875, 'black'), $
                  LIST(ref_count_outliers.amp, 0.565, 0.855, 'dark red'), $
                  LIST(test_count_outliers.period, 0.565, 0.585, 'black'), $
                  LIST(ref_count_outliers.period, 0.565, 0.565, 'dark red'), $
                  LIST(test_count_outliers.vel_amp, 0.565, 0.295, 'black'), $
                  LIST(ref_count_outliers.vel_amp, 0.565, 0.275, 'dark red'), $
                  LIST(test_count_outliers.th_len, 0.945, 0.585, 'black'), $
                  LIST(ref_count_outliers.th_len, 0.945, 0.565, 'dark red'), $
                  LIST(test_count_outliers.num_cyc, 0.945, 0.295, 'black'), $
                  LIST(ref_count_outliers.num_cyc, 0.945, 0.275, 'dark red')) DO BEGIN
    ; List of lists of [test_var, x_coord, y_coord, sym_color]
    IF VAR[0] GT 0 THEN BEGIN
        sym_outliers = symbol(VAR[1], VAR[2], sym_text='$\rightarrow$', sym_color=VAR[3], $
                              label_color=VAR[3], label_position='left', label_shift=[0.0, -0.005], $
                              label_string=strtrim(VAR[0], 2), label_font_size=10)
    ENDIF
ENDFOREACH

;PAGE 1 text
txt_slit_ID = text(0.01, 0.97, 'Slit # '+strtrim(slitnum, 2), font_size=14, font_style=1)
txt_header = text(0.16, 0.97, header, font_size=14, font_style=1)
txt_header = text(0.32, 0.92, 'Statistics and Overview Histograms', font_size=14, font_style=1)
txt_timestamp = text(0.85, 0.01, 'Date & Time Created$\n$'+systime(), font_size=10, color='dark grey')

;Printing event statistics
txt_pairing_header = text(0.02, 0.88, 'Pairing Ratios (Test:Ref)', font_size=14, font_style=1)
txt_total_test = text(0.03, 0.85, strtrim(n_threads, 2)+' total test threads', font_size=12)
sym_mult_ref = symbol(0.035, 0.83, 'o', label_string=strtrim(test_counts.multi_ref, 2)+' one : two', $
                      sym_color=degen_color_list[0], /sym_filled, sym_size=0.9, label_font_size=12)
sym_degen_1 = symbol(0.035, 0.80, 'o', label_string=strtrim(test_counts.one_degen, 2)+' one : one', $
                      sym_color=degen_color_list[1], /sym_filled, sym_size=0.9, label_font_size=12)
sym_degen_2 = symbol(0.035, 0.77, 'o', label_string=strtrim(test_counts.two_degen, 2)+' two : one', $
                      sym_color=degen_color_list[2], /sym_filled, sym_size=0.9, label_font_size=12)
sym_degen_3  = symbol(0.035, 0.74, 'o', label_string=strtrim(test_counts.three_degen, 2)+' three : one', $
                      sym_color=degen_color_list[3], /sym_filled, sym_size=0.9, label_font_size=12)
sym_degen_4  = symbol(0.035, 0.71, 'o', label_string=strtrim(test_counts.more_degen, 2)+' four+ : one', $
                      sym_color=degen_color_list[4], /sym_filled, sym_size=0.9, label_font_size=12)
txt_unpaired_test = text(0.045, 0.67, strtrim(test_counts.unpaired, 2)+' unpaired', font_size=12)

txt_waves_header = text(0.02, 0.62, 'Number of Waves', font_size=14, font_style=1)
txt_total_test = text(0.03, 0.59, strtrim(n_threads, 2)+' total test threads', font_size=12)
sym_no_waves = symbol(0.035, 0.57, wave_shape_list[0], label_string=strtrim(test_counts.no_waves, 2)+' no significant waves', $
                      sym_color='black', sym_size=0.9, label_font_size=12)
sym_1_wave = symbol(0.035, 0.54, wave_shape_list[1], label_string=strtrim(test_counts.one_wave, 2)+' one wave', $
                      sym_color='black', sym_size=0.9, label_font_size=12)
sym_2_waves = symbol(0.035, 0.51, wave_shape_list[2], label_string=strtrim(test_counts.two_waves, 2)+' two waves', $
                      sym_color='black', sym_size=0.9, label_font_size=12)
sym_3_waves  = symbol(0.035, 0.48, wave_shape_list[3], label_string=strtrim(test_counts.three_waves, 2)+' three waves', $
                      sym_color='black', sym_size=0.9, label_font_size=12)
sym_4_waves  = symbol(0.035, 0.45, wave_shape_list[4], label_string=strtrim(test_counts.more_waves, 2)+' four+ waves', $
                      sym_color='black', sym_size=0.9, label_font_size=12)

txt_ref_header = text(0.02, 0.39, 'Reference Threads', font_size=14, font_style=1)
txt_total_ref = text(0.03, 0.36, strtrim(n_ref, 2)+' total ref threads', font_size=12)
txt_paired_test = symbol(0.035, 0.34, ref_shape_list[1], label_string=strtrim(ref_counts.paired, 2)+' located', $
                         sym_color=ref_color_list[1], sym_size=0.9, label_font_size=12)
txt_unpaired_test = symbol(0.035, 0.31, ref_shape_list[0], label_string=strtrim(ref_counts.unpaired, 2)+' missed', $
                           sym_color=ref_color_list[0], sym_size=0.9, label_font_size=12)

; Saving the plot to PDF (default)
IF NOT KEYWORD_SET(screen) THEN BEGIN
   fig_summary.save, save_folder+filename+'.pdf', page_size=[12.0, 7.25], /append
ENDIF


;##### PAGE 2 - VAR vs VAR PLOTS #####
;-------------------------------------
IF NOT KEYWORD_SET(screen) THEN BEGIN
    fig_vars = window(name='fig_vars', dimensions=[1200, 725], /buffer)
ENDIF ELSE BEGIN
    fig_vars = window(name='fig_vars', dimensions=[1200, 725])
ENDELSE

;Initializing plots (two rows of three plots)
plt_amps = plot(amp_range, amp_range, linestyle='-', symbol='none', color='dark grey', $
                position=[0.06, 0.55, 0.32, 0.9], /current)

plt_periods = plot(period_range, period_range, linestyle='-', symbol='none', color='dark grey', $
                   position=[0.40, 0.55, 0.64, 0.9], /current)

plt_vel_amps = plot(vel_amp_range, vel_amp_range, linestyle='-', symbol='none', color='dark grey', $
                    position=[0.72, 0.55, 0.96, 0.9], /current)

plt_phases = plot(phase_range, phase_range, linestyle='-', symbol='none', color='dark grey', $
                  position=[0.06, 0.1, 0.32, 0.45], /current)

plt_th_lens = plot(th_len_range, th_len_range, linestyle='-', symbol='none', color='dark grey', $
                   position=[0.40, 0.1, 0.64, 0.45], /current)

plt_num_cyc = plot(num_cyc_range, num_cyc_range, linestyle='-', symbol='none', color='dark grey', $
                   position=[0.72, 0.1, 0.96, 0.45], /current)

;Looping over each group of degenerate events
FOR d=0,4 DO BEGIN
    FOR w=0,4 DO BEGIN
        loc_degen = where((paired_indices.degen EQ d) AND (test_params.num_waves EQ w), /NULL)
        IF n_elements(loc_degen) GT 0 THEN BEGIN
            plt_amps = PLOT(ref_params.amplitude[paired_indices.ref[loc_degen]], $
                            test_params.amplitude[paired_indices.test[loc_degen]], $
                            linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                            sym_size=0.5, overplot=plt_amps)

            plt_periods = PLOT(ref_params.period[paired_indices.ref[loc_degen]], $
                               test_params.period[paired_indices.test[loc_degen]], $
                               linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                               sym_size=0.5, overplot=plt_periods)

            plt_vel_amps = PLOT(ref_params.velocity_amp[paired_indices.ref[loc_degen]], $
                                test_params.velocity_amp[paired_indices.test[loc_degen]], $
                                linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                sym_size=0.5, overplot=plt_vel_amps)

            plt_phases = PLOT(ref_params.phase[paired_indices.ref[loc_degen]], $
                              test_params.phase[paired_indices.test[loc_degen]], $
                              linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                              sym_size=0.5, overplot=plt_phases)

            plt_th_lens = PLOT(ref_coords.length[paired_indices.ref[loc_degen]], $
                               test_coords.length[paired_indices.test[loc_degen]], $
                               linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                               sym_size=0.5, overplot=plt_th_lens)

            plt_num_cyc = PLOT(ref_params.num_cyc[paired_indices.ref[loc_degen]], $
                               test_params.num_cyc[paired_indices.test[loc_degen]], $
                               linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                               sym_size=0.5, overplot=plt_num_cyc)
            ; print, 'wave paired degen =', d, ' --- num waves =', n_elements(loc_degen)
            ; print, test_params.period[paired_indices.test[loc_degen]]
        ENDIF
    ENDFOR
ENDFOR

;Setting axis labels and ranges
; plt_amps.xrange = amp_range
plt_amps.xtitle = 'Reference Amplitude [$'+amp_units+'$]'
plt_amps.ytitle = 'Test Amplitude [$'+amp_units+'$]'

; plt_periods.xrange = period_range
plt_periods.xtitle = 'Reference Period [$'+period_units+'$]'
plt_periods.ytitle = 'Test Period [$'+period_units+'$]'

; plt_vel_amps.xrange = vel_amp_range
plt_vel_amps.xtitle = 'Reference Velocity Amplitude [$'+amp_units+' '+period_units+'^{-1}$]'
plt_vel_amps.ytitle = 'Test Velocity Amplitude [$'+amp_units+' '+period_units+'^{-1}$]'

plt_phases.xrange = phase_range
plt_phases.yrange = phase_range
plt_phases.xtitle = 'Reference Phase [rad]'
plt_phases.ytitle = 'Test Phase [rad]'

; plt_th_lens.xrange = th_len_range
; plt_th_lens.yrange = th_len_range
plt_th_lens.xtitle = 'Reference Thread Length [timesteps]'
plt_th_lens.ytitle = 'Test Thread Length [timesteps]'

IF NOT KEYWORD_SET(free_num_cyc_limits) THEN BEGIN
    plt_num_cyc.xrange = num_cyc_range
    plt_num_cyc.yrange = num_cyc_range
ENDIF
plt_num_cyc.xtitle = 'Reference Number of Cycles Observed'
plt_num_cyc.ytitle = 'Test Number of Cycles Observed'

;PAGE 2 text
txt_slit_ID = text(0.01, 0.97, 'Slit # '+strtrim(slitnum, 2), font_size=14, font_style=1)
txt_header = text(0.16, 0.97, header, font_size=14, font_style=1)
txt_header = text(0.32, 0.92, 'Test Variables vs Reference Variables', font_size=14, font_style=1)
txt_timestamp = text(0.85, 0.01, 'Date & Time Created$\n$'+systime(), font_size=10, color='dark grey')

; Saving the plot to PDF (default)
IF NOT KEYWORD_SET(screen) THEN BEGIN
    fig_vars.save, save_folder+filename+'.pdf', page_size=[12.0, 7.25], /append
ENDIF


;##### PAGE 3 - VAR DIFF vs TEST LENGTH PLOTS #####
;--------------------------------------------------
IF NOT KEYWORD_SET(screen) THEN BEGIN
    fig_diffs = window(name='fig_diffs', dimensions=[1200, 725], /buffer)
ENDIF ELSE BEGIN
    fig_diffs = window(name='fig_diffs', dimensions=[1200, 725])
ENDELSE

;Initializing plots (two rows of three plots)
plt_amp_diff = plot(th_len_range, [0,0], linestyle='-', symbol='none', color='dark grey', $
                position=[0.06, 0.55, 0.32, 0.9], /current)

plt_period_diff = plot(th_len_range, [0,0], linestyle='-', symbol='none', color='dark grey', $
                   position=[0.40, 0.55, 0.64, 0.9], /current)

plt_vel_amp_diff = plot(th_len_range, [0,0], linestyle='-', symbol='none', color='dark grey', $
                    position=[0.71, 0.55, 0.96, 0.9], /current)

plt_phase_diff = plot(th_len_range, [0,0], linestyle='-', symbol='none', color='dark grey', $
                  position=[0.06, 0.1, 0.32, 0.45], /current)

plt_th_len_diff = plot(th_len_range, [0,0], linestyle='-', symbol='none', color='dark grey', $
                   position=[0.40, 0.1, 0.64, 0.45], /current)

plt_num_cyc_diff = plot(th_len_range, [0,0], linestyle='-', symbol='none', color='dark grey', $
                   position=[0.72, 0.1, 0.96, 0.45], /current)

;Looping over each group of degenerate events
FOR d=0,4 DO BEGIN
    FOR w=0,4 DO BEGIN
        loc_degen = where((paired_indices.degen EQ d) AND (test_params.num_waves EQ w), /NULL)
        IF n_elements(loc_degen) GT 0 THEN BEGIN
            plt_amp_diff = PLOT(test_coords.length[paired_indices.test[loc_degen]], $
                                param_diffs.amplitude[paired_indices.test[loc_degen]], $
                                linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                sym_size=0.5, overplot=plt_amp_diff)

            plt_period_diff = PLOT(test_coords.length[paired_indices.test[loc_degen]], $
                                   param_diffs.period[paired_indices.test[loc_degen]], $
                                   linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                   sym_size=0.5, overplot=plt_period_diff)

            plt_vel_amp_diff = PLOT(test_coords.length[paired_indices.test[loc_degen]], $
                                    param_diffs.velocity_amp[paired_indices.test[loc_degen]], $
                                    linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                    sym_size=0.5, overplot=plt_vel_amp_diff)

            plt_phase_diff = PLOT(test_coords.length[paired_indices.test[loc_degen]], $
                                  param_diffs.phase[paired_indices.test[loc_degen]], $
                                  linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                  sym_size=0.5, overplot=plt_phase_diff)

            plt_th_len_diff = PLOT(test_coords.length[paired_indices.test[loc_degen]], $
                                   param_diffs.length[paired_indices.test[loc_degen]], $
                                   linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                   sym_size=0.5, overplot=plt_th_len_diff)

            plt_num_cyc_diff = PLOT(test_coords.length[paired_indices.test[loc_degen]], $
                                    param_diffs.num_cyc[paired_indices.test[loc_degen]], $
                                    linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                    sym_size=0.5, overplot=plt_num_cyc_diff)
        ENDIF
    ENDFOR
ENDFOR

plt_amp_diff.xtitle = 'Test Thread Length [timesteps]'
plt_amp_diff.ytitle = 'Amplitude Difference (test - ref) [$'+amp_units+'$]'

plt_period_diff.xtitle = 'Test Thread Length [timesteps]'
plt_period_diff.ytitle = 'Period Difference (test - ref)[$'+period_units+'$]'

plt_vel_amp_diff.xtitle = 'Test Thread Length [timesteps]'
plt_vel_amp_diff.ytitle = 'Vel. Amp. Difference (test - ref) [$'+amp_units+' '+period_units+'^{-1}$]'

plt_phase_diff.xtitle = 'Test Thread Length [timesteps]'
plt_phase_diff.ytitle = 'Phase Difference (test - ref) [rad]'

plt_th_len_diff.xtitle = 'Test Thread Length [timesteps]'
plt_th_len_diff.ytitle = 'Length Difference (test - ref) [timesteps]'

IF NOT KEYWORD_SET(free_num_cyc_limits) THEN BEGIN
    plt_num_cyc_diff.yrange = [-num_cyc_range[1], num_cyc_range[1]]
ENDIF
plt_num_cyc_diff.xtitle = 'Test Thread Length [timesteps]'
plt_num_cyc_diff.ytitle = 'Obs. Cycles Difference (test - ref)'

;PAGE 3 text
txt_slit_ID = text(0.01, 0.97, 'Slit # '+strtrim(slitnum, 2), font_size=14, font_style=1)
txt_header = text(0.16, 0.97, header, font_size=14, font_style=1)
txt_header = text(0.32, 0.92, 'Variable Difference (Test - Reference) vs Test Thread Length', font_size=14, font_style=1)
txt_timestamp = text(0.85, 0.01, 'Date & Time Created$\n$'+systime(), font_size=10, color='dark grey')

; Saving the plot to PDF (default)
IF NOT KEYWORD_SET(screen) THEN BEGIN
    fig_diffs.save, save_folder+filename+'.pdf', page_size=[12.0, 7.25], /append
ENDIF


;##### PAGE 4 - VAR vs THREAD LENGTH #####
;--------------------------------------------------
IF NOT KEYWORD_SET(screen) THEN BEGIN
    fig_vs_len_1 = window(name='fig_vs_th_len_1', dimensions=[1200, 725], /buffer)
ENDIF ELSE BEGIN
    fig_vs_len_1 = window(name='fig_vs_th_len_1', dimensions=[1200, 725])
ENDELSE

;Initializing plots (two rows of three plots)
plt_test_amp_vs_len = plot(th_len_range, amp_range, linestyle='-', symbol='none', color='white', $
                              position=[0.06, 0.55, 0.32, 0.9], /current)

plt_test_period_vs_len = plot(th_len_range, period_range, linestyle='-', symbol='none', color='white', $
                                  position=[0.40, 0.55, 0.64, 0.9], /current)

plt_test_vel_amp_vs_len  = plot(th_len_range, vel_amp_range, linestyle='-', symbol='none', color='white', $
                                   position=[0.71, 0.55, 0.96, 0.9], /current)

plt_ref_amp_vs_len = plot(th_len_range, amp_range, linestyle='-', symbol='none', color='white', $
                             position=[0.06, 0.1, 0.32, 0.45], /current)

plt_ref_period_vs_len = plot(th_len_range, period_range, linestyle='-', symbol='none', color='white', $
                                 position=[0.40, 0.1, 0.64, 0.45], /current)

plt_ref_vel_amp_vs_len = plot(th_len_range, vel_amp_range, linestyle='-', symbol='none', color='white', $
                                 position=[0.72, 0.1, 0.96, 0.45], /current)

;Looping over each group of degenerate events
FOR d=0,4 DO BEGIN
    FOR w=0,4 DO BEGIN
        loc_degen = where((paired_indices.degen EQ d) AND (test_params.num_waves EQ w), /NULL)
        IF n_elements(loc_degen) GT 0 THEN BEGIN
            plt_test_amp_vs_len = PLOT(test_coords.length[paired_indices.test[loc_degen]], $
                                       test_params.amplitude[paired_indices.test[loc_degen]], $
                                       linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                       sym_size=0.5, overplot=plt_test_amp_vs_len)

            plt_test_period_vs_len = PLOT(test_coords.length[paired_indices.test[loc_degen]], $
                                          test_params.period[paired_indices.test[loc_degen]], $
                                          linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                          sym_size=0.5, overplot=plt_test_period_vs_len)

            plt_test_vel_amp_vs_len = PLOT(test_coords.length[paired_indices.test[loc_degen]], $
                                           test_params.velocity_amp[paired_indices.test[loc_degen]], $
                                           linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                           sym_size=0.5, overplot=plt_test_vel_amp_vs_len)
        ENDIF
    ENDFOR
ENDFOR

FOR pf=0,1 DO BEGIN
    loc_subset = where((ref_coords.paired_flag EQ pf), /NULL)
    IF n_elements(loc_subset) GT 0 THEN BEGIN
        plt_ref_amp_vs_len = PLOT(ref_coords.length[loc_subset], $
                                  ref_params.amplitude[loc_subset], $
                                  linestyle='none', symbol=ref_shape_list[pf], color=ref_color_list[pf], $
                                  sym_size=0.5, overplot=plt_ref_amp_vs_len)

        plt_ref_period_vs_len = PLOT(ref_coords.length[loc_subset], $
                                     ref_params.period[loc_subset], $
                                     linestyle='none', symbol=ref_shape_list[pf], color=ref_color_list[pf], $
                                     sym_size=0.5, overplot=plt_ref_period_vs_len)

        plt_ref_vel_amp_vs_len = PLOT(ref_coords.length[loc_subset], $
                                      ref_params.velocity_amp[loc_subset], $
                                      linestyle='none', symbol=ref_shape_list[pf], color=ref_color_list[pf], $
                                      sym_size=0.5, overplot=plt_ref_vel_amp_vs_len)
    ENDIF
ENDFOR

plt_test_amp_vs_len.xtitle = 'Test Thread Length [timesteps]'
plt_test_amp_vs_len.ytitle = 'Test Amplitude [$'+amp_units+'$]'

plt_test_period_vs_len.xtitle = 'Test Thread Length [timesteps]'
plt_test_period_vs_len.ytitle = 'Test Period [$'+period_units+'$]'

plt_test_vel_amp_vs_len.xtitle = 'Test Thread Length [timesteps]'
plt_test_vel_amp_vs_len.ytitle = 'Test Velocity Amplitude [$'+amp_units+' '+period_units+'^{-1}$]'

plt_ref_amp_vs_len.xtitle = 'Reference Thread Length [timesteps]'
plt_ref_amp_vs_len.ytitle = 'Reference Amplitude [$'+amp_units+'$]'

plt_ref_period_vs_len.xtitle = 'Reference Thread Length [timesteps]'
plt_ref_period_vs_len.ytitle = 'Reference Period [$'+period_units+'$]'

plt_ref_vel_amp_vs_len.xtitle = 'Reference Thread Length [timesteps]'
plt_ref_vel_amp_vs_len.ytitle = 'Reference Velocity Amplitude [$'+amp_units+' '+period_units+'^{-1}$]'

;PAGE 4 text
txt_slit_ID = text(0.01, 0.97, 'Slit # '+strtrim(slitnum, 2), font_size=14, font_style=1)
txt_header = text(0.16, 0.97, header, font_size=14, font_style=1)
txt_header = text(0.32, 0.92, 'Variables vs Thread Length (1/2)', font_size=14, font_style=1)
txt_timestamp = text(0.85, 0.01, 'Date & Time Created$\n$'+systime(), font_size=10, color='dark grey')

; Saving the plot to PDF (default)
IF NOT KEYWORD_SET(screen) THEN BEGIN
    fig_vs_len_1.save, save_folder+filename+'.pdf', page_size=[12.0, 7.25], /append
ENDIF


;##### PAGE 5 - MORE VARS vs THREAD LENGTH #####
;--------------------------------------------------
IF NOT KEYWORD_SET(screen) THEN BEGIN
    fig_vs_len_2 = window(name='fig_vs_th_len_2', dimensions=[1200, 725], /buffer)
ENDIF ELSE BEGIN
    fig_vs_len_2 = window(name='fig_vs_th_len_2', dimensions=[1200, 725])
ENDELSE

;Initializing plots (two rows of three plots)
plt_test_phase_vs_len = plot(th_len_range, phase_range, linestyle='-', symbol='none', color='white', $
                              position=[0.06, 0.55, 0.32, 0.9], /current)

plt_test_th_len_vs_len = plot(th_len_range, th_len_range, linestyle='-', symbol='none', color='white', $
                                  position=[0.40, 0.55, 0.64, 0.9], /current)

plt_test_num_cyc_vs_len  = plot(th_len_range, num_cyc_range, linestyle='-', symbol='none', color='white', $
                                   position=[0.71, 0.55, 0.96, 0.9], /current)

plt_ref_phase_vs_len = plot(th_len_range, phase_range, linestyle='-', symbol='none', color='white', $
                             position=[0.06, 0.1, 0.32, 0.45], /current)

plt_ref_th_len_vs_len = plot(th_len_range, th_len_range, linestyle='-', symbol='none', color='white', $
                                 position=[0.40, 0.1, 0.64, 0.45], /current)

plt_ref_num_cyc_vs_len = plot(th_len_range, num_cyc_range, linestyle='-', symbol='none', color='white', $
                                 position=[0.72, 0.1, 0.96, 0.45], /current)

;Looping over each group of degenerate events
FOR d=0,4 DO BEGIN
    FOR w=0,4 DO BEGIN
        loc_degen = where((paired_indices.degen EQ d) AND (test_params.num_waves EQ w), /NULL)
        IF n_elements(loc_degen) GT 0 THEN BEGIN
            plt_test_phase_vs_len = PLOT(test_coords.length[paired_indices.test[loc_degen]], $
                                         test_params.phase[paired_indices.test[loc_degen]], $
                                         linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                         sym_size=0.5, overplot=plt_test_phase_vs_len)

            plt_test_th_len_vs_len = PLOT(test_coords.length[paired_indices.test[loc_degen]], $
                                          test_coords.length[paired_indices.test[loc_degen]], $
                                          linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                          sym_size=0.5, overplot=plt_test_th_len_vs_len)

            plt_test_num_cyc_vs_len = PLOT(test_coords.length[paired_indices.test[loc_degen]], $
                                           test_params.num_cyc[paired_indices.test[loc_degen]], $
                                           linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                           sym_size=0.5, overplot=plt_test_num_cyc_vs_len)
        ENDIF
    ENDFOR
ENDFOR

FOR pf=0,1 DO BEGIN
    loc_subset = where((ref_coords.paired_flag EQ pf), /NULL)
    IF n_elements(loc_subset) GT 0 THEN BEGIN
        plt_ref_phase_vs_len = PLOT(ref_coords.length[loc_subset], $
                                    ref_params.phase[loc_subset], $
                                    linestyle='none', symbol=ref_shape_list[pf], color=ref_color_list[pf], $
                                    sym_size=0.5, overplot=plt_ref_phase_vs_len)

        plt_ref_th_len_vs_len = PLOT(ref_coords.length[loc_subset], $
                                     ref_coords.length[loc_subset], $
                                     linestyle='none', symbol=ref_shape_list[pf], color=ref_color_list[pf], $
                                     sym_size=0.5, overplot=plt_ref_th_len_vs_len)

        plt_ref_num_cyc_vs_len = PLOT(ref_coords.length[loc_subset], $
                                      ref_params.num_cyc[loc_subset], $
                                      linestyle='none', symbol=ref_shape_list[pf], color=ref_color_list[pf], $
                                      sym_size=0.5, overplot=plt_ref_num_cyc_vs_len)
    ENDIF
ENDFOR

plt_test_phase_vs_len.yrange = phase_range
plt_test_phase_vs_len.xtitle = 'Test Thread Length [timesteps]'
plt_test_phase_vs_len.ytitle = 'Test Phase [rad]'

plt_test_th_len_vs_len.xtitle = 'Test Thread Length [timesteps]'
plt_test_th_len_vs_len.ytitle = 'Test Thread Length [timesteps]'

IF NOT KEYWORD_SET(free_num_cyc_limits) THEN BEGIN
    plt_test_num_cyc_vs_len.yrange = num_cyc_range
ENDIF
plt_test_num_cyc_vs_len.xtitle = 'Test Thread Length [timesteps]'
plt_test_num_cyc_vs_len.ytitle = 'Test Num. Cycles Observed'

plt_ref_phase_vs_len.yrange = phase_range
plt_ref_phase_vs_len.xtitle = 'Reference Thread Length [timesteps]'
plt_ref_phase_vs_len.ytitle = 'Reference Phase [rad]'

plt_ref_th_len_vs_len.xtitle = 'Reference Thread Length [timesteps]'
plt_ref_th_len_vs_len.ytitle = 'Reference Thread Length [timesteps]'

IF NOT KEYWORD_SET(free_num_cyc_limits) THEN BEGIN
    plt_ref_num_cyc_vs_len.yrange = num_cyc_range
ENDIF
plt_ref_num_cyc_vs_len.xtitle = 'Reference Thread Length [timesteps]'
plt_ref_num_cyc_vs_len.ytitle = 'Reference Num. Cycles Observed'

;PAGE 5 text
txt_slit_ID = text(0.01, 0.97, 'Slit # '+strtrim(slitnum, 2), font_size=14, font_style=1)
txt_header = text(0.16, 0.97, header, font_size=14, font_style=1)
txt_header = text(0.32, 0.92, 'Variables vs Thread Length (2/2)', font_size=14, font_style=1)
txt_timestamp = text(0.85, 0.01, 'Date & Time Created$\n$'+systime(), font_size=10, color='dark grey')

; Saving the plot to PDF (default)
IF NOT KEYWORD_SET(screen) THEN BEGIN
    fig_vs_len_2.save, save_folder+filename+'.pdf', page_size=[12.0, 7.25], /append
ENDIF


;##### PAGE 6 - VAR vs PERIOD #####
;--------------------------------------------------
IF NOT KEYWORD_SET(screen) THEN BEGIN
    fig_vs_period_1 = window(name='fig_vs_period_1', dimensions=[1200, 725], /buffer)
ENDIF ELSE BEGIN
    fig_vs_period_1 = window(name='fig_vs_period_1', dimensions=[1200, 725])
ENDELSE

;Initializing plots (two rows of three plots)
plt_test_amp_vs_period = plot(period_range, amp_range, linestyle='-', symbol='none', color='white', $
                              position=[0.06, 0.55, 0.32, 0.9], /current)

plt_test_period_vs_period = plot(period_range, period_range, linestyle='-', symbol='none', color='white', $
                                 position=[0.40, 0.55, 0.64, 0.9], /current)

plt_test_vel_amp_vs_period  = plot(period_range, vel_amp_range, linestyle='-', symbol='none', color='white', $
                                   position=[0.71, 0.55, 0.96, 0.9], /current)

plt_ref_amp_vs_period = plot(period_range, amp_range, linestyle='-', symbol='none', color='white', $
                             position=[0.06, 0.1, 0.32, 0.45], /current)

plt_ref_period_vs_period = plot(period_range, period_range, linestyle='-', symbol='none', color='white', $
                                position=[0.40, 0.1, 0.64, 0.45], /current)

plt_ref_vel_amp_vs_period = plot(period_range, vel_amp_range, linestyle='-', symbol='none', color='white', $
                                 position=[0.72, 0.1, 0.96, 0.45], /current)

;Looping over each group of degenerate events
FOR d=0,4 DO BEGIN
    FOR w=0,4 DO BEGIN
        loc_degen = where((paired_indices.degen EQ d) AND (test_params.num_waves EQ w), /NULL)
        IF n_elements(loc_degen) GT 0 THEN BEGIN
            plt_test_amp_vs_period = PLOT(test_params.period[paired_indices.test[loc_degen]], $
                                          test_params.amplitude[paired_indices.test[loc_degen]], $
                                          linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                          sym_size=0.5, overplot=plt_test_amp_vs_period)

            plt_test_period_vs_period = PLOT(test_params.period[paired_indices.test[loc_degen]], $
                                             test_params.period[paired_indices.test[loc_degen]], $
                                             linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                             sym_size=0.5, overplot=plt_test_period_vs_period)

            plt_test_vel_amp_vs_period = PLOT(test_params.period[paired_indices.test[loc_degen]], $
                                              test_params.velocity_amp[paired_indices.test[loc_degen]], $
                                              linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                              sym_size=0.5, overplot=plt_test_vel_amp_vs_period)
        ENDIF
    ENDFOR
ENDFOR

FOR pf=0,1 DO BEGIN
    loc_subset = where((ref_coords.paired_flag EQ pf), /NULL)
    IF n_elements(loc_subset) GT 0 THEN BEGIN
        plt_ref_amp_vs_period = PLOT(ref_params.period[loc_subset], $
                                      ref_params.amplitude[loc_subset], $
                                      linestyle='none', symbol=ref_shape_list[pf], color=ref_color_list[pf], $
                                      sym_size=0.5, overplot=plt_ref_amp_vs_period)

        plt_ref_period_vs_period = PLOT(ref_params.period[loc_subset], $
                                          ref_params.period[loc_subset], $
                                          linestyle='none', symbol=ref_shape_list[pf], color=ref_color_list[pf], $
                                          sym_size=0.5, overplot=plt_ref_period_vs_period)

        plt_ref_vel_amp_vs_period = PLOT(ref_params.period[loc_subset], $
                                          ref_params.velocity_amp[loc_subset], $
                                          linestyle='none', symbol=ref_shape_list[pf], color=ref_color_list[pf], $
                                          sym_size=0.5, overplot=plt_ref_vel_amp_vs_period)
    ENDIF
ENDFOR

plt_test_amp_vs_period.xtitle = 'Test Period [$'+period_units+'$]'
plt_test_amp_vs_period.ytitle = 'Test Amplitude [$'+amp_units+'$]'

plt_test_period_vs_period.xtitle = 'Test Period [$'+period_units+'$]'
plt_test_period_vs_period.ytitle = 'Test Period [$'+period_units+'$]'

plt_test_vel_amp_vs_period.xtitle = 'Test Period [$'+period_units+'$]'
plt_test_vel_amp_vs_period.ytitle = 'Test Velocity Amplitude [$'+amp_units+' '+period_units+'^{-1}$]'

plt_ref_amp_vs_period.xtitle = 'Reference Period [$'+period_units+'$]'
plt_ref_amp_vs_period.ytitle = 'Reference Amplitude [$'+amp_units+'$]'

plt_ref_period_vs_period.xtitle = 'Reference Period [$'+period_units+'$]'
plt_ref_period_vs_period.ytitle = 'Reference Period [$'+period_units+'$]'

plt_ref_vel_amp_vs_period.xtitle = 'Reference Period [$'+period_units+'$]'
plt_ref_vel_amp_vs_period.ytitle = 'Reference Velocity Amplitude [$'+amp_units+' '+period_units+'^{-1}$]'

;PAGE 6 text
txt_slit_ID = text(0.01, 0.97, 'Slit # '+strtrim(slitnum, 2), font_size=14, font_style=1)
txt_header = text(0.16, 0.97, header, font_size=14, font_style=1)
txt_header = text(0.32, 0.92, 'Variables vs Wave Period (1/2)', font_size=14, font_style=1)
txt_timestamp = text(0.85, 0.01, 'Date & Time Created$\n$'+systime(), font_size=10, color='dark grey')

; Saving the plot to PDF (default)
IF NOT KEYWORD_SET(screen) THEN BEGIN
    fig_vs_period_1.save, save_folder+filename+'.pdf', page_size=[12.0, 7.25], /append
ENDIF


;##### PAGE 7 - MORE VARS vs PERIOD #####
;--------------------------------------------------
IF NOT KEYWORD_SET(screen) THEN BEGIN
    fig_vs_period_2 = window(name='fig_vs_period_2', dimensions=[1200, 725], /buffer)
ENDIF ELSE BEGIN
    fig_vs_period_2 = window(name='fig_vs_period_2', dimensions=[1200, 725])
ENDELSE

;Initializing plots (two rows of three plots)
plt_test_phase_vs_period = plot(period_range, phase_range, linestyle='-', symbol='none', color='white', $
                              position=[0.06, 0.55, 0.32, 0.9], /current)

plt_test_th_len_vs_period = plot(period_range, th_len_range, linestyle='-', symbol='none', color='white', $
                                 position=[0.40, 0.55, 0.64, 0.9], /current)

plt_test_num_cyc_vs_period  = plot(period_range, num_cyc_range, linestyle='-', symbol='none', color='white', $
                                   position=[0.71, 0.55, 0.96, 0.9], /current)

plt_ref_phase_vs_period = plot(period_range, phase_range, linestyle='-', symbol='none', color='white', $
                               position=[0.06, 0.1, 0.32, 0.45], /current)

plt_ref_th_len_vs_period = plot(period_range, th_len_range, linestyle='-', symbol='none', color='white', $
                                position=[0.40, 0.1, 0.64, 0.45], /current)

plt_ref_num_cyc_vs_period = plot(period_range, num_cyc_range, linestyle='-', symbol='none', color='white', $
                                 position=[0.72, 0.1, 0.96, 0.45], /current)

;Looping over each group of degenerate events
FOR d=0,4 DO BEGIN
    FOR w=0,4 DO BEGIN
        loc_degen = where((paired_indices.degen EQ d) AND (test_params.num_waves EQ w), /NULL)
        IF n_elements(loc_degen) GT 0 THEN BEGIN
            plt_test_phase_vs_period = PLOT(test_params.period[paired_indices.test[loc_degen]], $
                                            test_params.phase[paired_indices.test[loc_degen]], $
                                            linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                            sym_size=0.5, overplot=plt_test_phase_vs_period)

            plt_test_th_len_vs_period = PLOT(test_params.period[paired_indices.test[loc_degen]], $
                                             test_coords.length[paired_indices.test[loc_degen]], $
                                             linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                             sym_size=0.5, overplot=plt_test_th_len_vs_period)

            plt_test_num_cyc_vs_period = PLOT(test_params.period[paired_indices.test[loc_degen]], $
                                              test_params.num_cyc[paired_indices.test[loc_degen]], $
                                              linestyle='none', symbol=wave_shape_list[w], color=degen_color_list[d], $
                                              sym_size=0.5, overplot=plt_test_num_cyc_vs_period)
        ENDIF
    ENDFOR
ENDFOR

FOR pf=0,1 DO BEGIN
    loc_subset = where((ref_coords.paired_flag EQ pf), /NULL)
    IF n_elements(loc_subset) GT 0 THEN BEGIN
        plt_ref_phase_vs_period = PLOT(ref_params.period[loc_subset], $
                                       ref_params.phase[loc_subset], $
                                       linestyle='none', symbol=ref_shape_list[pf], color=ref_color_list[pf], $
                                       sym_size=0.5, overplot=plt_ref_phase_vs_period)

        plt_ref_th_len_vs_period = PLOT(ref_params.period[loc_subset], $
                                        ref_coords.length[loc_subset], $
                                        linestyle='none', symbol=ref_shape_list[pf], color=ref_color_list[pf], $
                                        sym_size=0.5, overplot=plt_ref_th_len_vs_period)

        plt_ref_num_cyc_vs_period = PLOT(ref_params.period[loc_subset], $
                                         ref_params.num_cyc[loc_subset], $
                                         linestyle='none', symbol=ref_shape_list[pf], color=ref_color_list[pf], $
                                         sym_size=0.5, overplot=plt_ref_num_cyc_vs_period)
    ENDIF
ENDFOR

plt_test_phase_vs_period.yrange = phase_range
plt_test_phase_vs_period.xtitle = 'Test Period [$'+period_units+'$]'
plt_test_phase_vs_period.ytitle = 'Test Phase [rad]'

plt_test_th_len_vs_period.xtitle = 'Test Period [$'+period_units+'$]'
plt_test_th_len_vs_period.ytitle = 'Test Thread Length [timesteps]'

IF NOT KEYWORD_SET(free_num_cyc_limits) THEN BEGIN
    plt_test_num_cyc_vs_period.yrange = num_cyc_range
ENDIF
plt_test_num_cyc_vs_period.xtitle = 'Test Period [$'+period_units+'$]'
plt_test_num_cyc_vs_period.ytitle = 'Test Num. Cycles Observed'

plt_ref_phase_vs_period.yrange = phase_range
plt_ref_phase_vs_period.xtitle = 'Reference Period [$'+period_units+'$]'
plt_ref_phase_vs_period.ytitle = 'Reference Phase [rad]'

plt_ref_th_len_vs_period.xtitle = 'Reference Period [$'+period_units+'$]'
plt_ref_th_len_vs_period.ytitle = 'Reference Thread Length [timesteps]'

IF NOT KEYWORD_SET(free_num_cyc_limits) THEN BEGIN
    plt_ref_num_cyc_vs_period.yrange = num_cyc_range
ENDIF
plt_ref_num_cyc_vs_period.xtitle = 'Reference Period [$'+period_units+'$]'
plt_ref_num_cyc_vs_period.ytitle = 'Reference Num. Cycles Observed'

;PAGE 7 text
txt_slit_ID = text(0.01, 0.97, 'Slit # '+strtrim(slitnum, 2), font_size=14, font_style=1)
txt_header = text(0.16, 0.97, header, font_size=14, font_style=1)
txt_header = text(0.32, 0.92, 'Variables vs Wave Period (2/2)', font_size=14, font_style=1)
txt_timestamp = text(0.85, 0.01, 'Date & Time Created$\n$'+systime(), font_size=10, color='dark grey')

; Saving the plot to PDF (default)
IF NOT KEYWORD_SET(screen) THEN BEGIN
    fig_vs_period_2.save, save_folder+filename+'.pdf', page_size=[12.0, 7.25], /append, /close
    print, 'Finished plotting comparison of NUWT runs!'
    print, 'Save Folder: ', save_folder
    print, 'Filename: ', filename+'.pdf'
ENDIF

END
