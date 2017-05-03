;+
;NAME: RUN_NUWT_WITH_FFT
;
;PURPOSE:
;   One of the top level Northumbria University Wave Tracking (NUWT) run scripts.
;   Takes a time distance diagram, picks out peaks in the intensity (also called
;   'threads'), then feeds them into an FFT to calculate the power and frequencies
;   of oscillation (if any). See the user guide (once available) and the code of
;   each sub-program for more imformation.
;
;
;INPUTS:
;   data - an array of time-distance (t-d) diagrams of the form (x,t,z)
;
;OPTIONAL INPUTS:
;   errors - estimated errors on intensity values, supplied to gaussian fitting routine.
;            if not supplied a default value of 1% of intensity values is used
;   res - spatial resolution of data (in arcsec). Default is 1
;   cad - temporal cadence of the data (in s). Default is 1
;   /gauss - uses a gaussian fit to locate centre of thread, provides sub-pixel measurements
;   /full_gauss - similar to the above. Will also return the fitted gaussian widths (for debugging)
;   /pad_fft - if set, will pad the array with zeros to improve the precision
;              of peak frequencies returned by the fft. Note, this does NOT
;              actually increase the resolution and can results in extra spectral
;              leakage. Use with care.
;   pad_length - number of zeros to pad to the end of the array when /pad_fft is set.
;                If /fill_pad is ALSO set, will instead pad the array until the
;                total length equals pad_length (or just pad with 1 zero if the
;                array is already longer than pad_length). Default is 1000.
;   /fill_pad - if set, will fill the array with extra zeros until the length equals
;               "pad_length" instead of appending a set number zeros (default).
;               Note: if the pad_length is set too low, using /fill_pad may
;               introducing "banding" in the possible frequency values returned.
;   /bootstrap - if set, will perform bootstrapping to resample the data and obtaining
;                error estimates on the output values
;   num_bootstrap - number of bootstrap resamples to use. Default is 1000
;   /interactive - if set, will allow for interatively picking gradient and min thread length
;   /save_results - if set, will save the 'all_nuwt_dat' COMMON block to a file
;   save_folder - folder in which to save the results. Defaults to the user's home folder.
;                 Will also append a '/' to the end if not included.
;   filetag - unique tag to append to the end of the output files
;
;OUTPUTS:
;   common blocks contain the results and are used for all internal passing of
;   data between subprograms.
;
;EXTERNAL CALLS - locate_things.pro, follow_thread.pro, patch_up.pro, wave_from_fft.pro
;
;HISTORY: Name---------Date---------Description
;         M Weberg  JUNE, 2016  Initial coding.
;         M weberg  SEPT, 2016  Made some quality-of-life improvements
;                               - Added a master COMMON block that compiles the
;                                 results from all of the data slits into lists.
;                                 Can also output this block to a '.save' file
;                               - Added an interactive mode for selecting gradients
;                                 and minimum thread lengths
;
;TO DO / RESTRICTIONS:
;   - Add funcationality for passing a structure with the desired keyword
;     arguments (kwargs) for each subprogram.
;   - Add options for plotting the data after processing
;   - Add options for tracking the metadata (data source, date & time of
;     observation, ect...) and creating better labeled output.
;-


PRO RUN_NUWT_WITH_FFT, data, errors=errors, res=res, cad=cad, gauss=gauss, full_gauss=full_gauss, $
                       pad_fft=pad_fft, pad_length=pad_length, fill_pad=fill_pad, $
                       bootstrap=bootstrap, num_bootstrap=num_bootstrap, $
                       interactive=interactive, save_results=save_results, $
                       save_folder=save_folder, filetag=filetag

print, ' ' ;just an empty line to make console output easier to read
print, '##### Running Northumbria Wave Tracking Code (NUWT) #####'
;###############################################
;INITIALIZATION OF COMMON BLOCKS
;###############################################
;Structures, arrays, and list containing the results for a single data slit
COMMON located_dat, located, nx, nt
COMMON threads_dat, threads
COMMON fft_results_dat, fft_results, fft_stats

;Create a master COMMON block that holds the results for ALL slits
COMMON all_nuwt_dat, nuwt_located, nuwt_threads, nuwt_fft_results, nuwt_fft_stats
nuwt_located = LIST()
nuwt_threads = LIST()
nuwt_fft_results = LIST()
nuwt_fft_stats = LIST()

;###############################################
;SETTING DEFAULT VALUES
;###############################################
IF KEYWORD_SET(full_gauss) THEN gauss = 1
IF NOT keyword_set(full_gauss) THEN full_gauss = 0
IF NOT keyword_set(num_bootstrap) THEN num_bootstrap = 1000

IF NOT KEYWORD_SET(pad_length) THEN pad_length = 1000
IF KEYWORD_SET(pad_fft) AND NOT KEYWORD_SET(fill_pad) THEN fill_pad = 0
IF KEYWORD_SET(fill_pad) AND NOT KEYWORD_SET(pad_fft) THEN pad_fft = 1

IF NOT KEYWORD_SET(filetag) THEN filetag='' ELSE filetag = '_'+filetag
IF NOT KEYWORD_SET(save_folder) THEN save_folder = '' ;i.e. defaults to home folder
IF strlen(save_folder) GT 0 AND NOT save_folder.endswith('/') THEN save_folder = save_folder+'/'

sz = size(data)
IF sz[0] EQ 3 THEN nslits=sz[3] ELSE nslits=1

IF NOT keyword_set(dx) THEN dx=1.0
IF NOT keyword_set(dt) THEN dt=1.0
IF NOT keyword_set(signif_levels) THEN signif_levels=0.95

IF NOT KEYWORD_SET(gauss) THEN BEGIN
    print, 'Finding whole pixel peak locations'
ENDIF ELSE BEGIN
    print, 'Gaussian location enabled. Finding sub-pixel peak locations'
ENDELSE

;###############################################
;ASKING FOR GRAD AND MIN THREAD LENGTH
;###############################################
IF NOT KEYWORD_SET(interactive) THEN BEGIN
    print, '   ' ;another empty line to make console output easier to read
    grad_val = 1d
    READ, grad_val, PROMPT='Please enter gradient: '

    min_tlen_val = 1d
    READ, min_tlen_val, PROMPT='Please enter minimum thread length: '
ENDIF ELSE BEGIN
    ;Initialize the threads structure (prevents odd behavior in interactive mode)
    threads = {pos:fltarr(10), err_pos:fltarr(10), bin_flags:intarr(10), $
               start_bin:-1, end_bin:-1, length:-1}

    print, 'Interactive mode activated!'
    print, 'NOTE: plots will not close until after a command prompt is answered'
    print, '   ' ;and another...
    grad_val = 1d
    READ, grad_val, PROMPT='Please enter initial gradient to try: '

    min_tlen_val = 1d
    READ, min_tlen_val, PROMPT='Please enter initial minimum thread length to try: '
ENDELSE

;###############################################
;PROCESS EACH TIME-DISTANCE DIAGRAM
;###############################################
print, '   ' ;last empty line printed
FOR k=0, nslits-1 DO BEGIN
    print, 'Processing Slit Number: '+strtrim(k, 2)

    ;Finding peak locations
    print, '   Finding local intensity peaks ...'
    loop_grad = 1
    ask_grad = 'n'
    WHILE loop_grad DO BEGIN
        IF not keyword_set(gauss) THEN BEGIN
            ;Whole pixel resolution (no gaussian fitting)
            LOCATE_THINGS_MIN_TESTING, data[*,*,k], grad=grad_val
        ENDIF ELSE BEGIN
            ;Sub-pixel resolution using gaussin fitting of intensity maxima
            IF n_elements(errors) GT 0 THEN errorsi=errors[*,*,k]

            ;If errors are not supplied then error level set to default 10% of intensity
            IF n_elements(errors) EQ 0 THEN BEGIN
                errorsi = fltarr(sz(1),sz(2))
                errorsi = abs(data[*,*,k])*0.1
            ENDIF
            LOCATE_THINGS_TESTING, data[*,*,k], errors=errorsi, grad=grad_val, full_gauss=full_gauss

            ; ;### EXPERIMENTAL CODE - please ingore for now....
            ; LOCATE_THINGS_ALT, data[*,*,k], errors=errorsi, grad=grad_val, full_gauss=full_gauss, /weighted_mean
            ; LOCATE_THINGS_DEBUG, data[*,*,k], errors=errorsi, grad=grad_val, full_gauss=full_gauss
            ; LOCATE_THINGS_DEBUG, data[*,*,k], errors=errorsi, grad=grad_val, full_gauss=full_gauss, cut_chisq=1e6
            ; ;### Test with a more generous cutoff
            ; print, 'cut_chisq set to 4 sigma!'
            ; LOCATE_THINGS_TESTING, data[*,*,k], errors=errorsi, grad=grad_val, full_gauss=full_gauss, cut_chisq = chisqr_cvf(1.0 - 0.9999, 7-5)
        ENDELSE

        IF KEYWORD_SET(interactive) THEN BEGIN
            PLOT_NUWT_PEAKS_AND_THREADS, data[*,*,k], slitnum=k, grad=grad_val, /plot_peaks, /screen, /use_temp_common_blocks
            READ, ask_grad, PROMPT='Is the selected gradient acceptable? (y/n) '
            ask_grad = strlowcase(ask_grad)
            IF ask_grad.startswith('y') THEN BEGIN
                loop_grad = 0
            ENDIF ELSE BEGIN
                READ, grad_val, PROMPT='Please enter the next gradient to try: '
            ENDELSE
        ENDIF ELSE BEGIN
            ;end 'loop' if not in interactive mode
            loop_grad = 0
        ENDELSE
    ENDWHILE

    ;Identify connected 'threads' of peaks and track them over time
    print, '   Identifing and tracking threads ...'
    loop_tlen = 1
    ask_tlen = 'n'
    WHILE loop_tlen DO BEGIN
        FOLLOW_THREAD_TESTING, min_tlen=min_tlen_val
        ;FOLLOW_THREAD_DEBUG, min_tlen=min_tlen_val, area=area

        n_threads = n_elements(threads)
        print, '   Number of threads found: '+strtrim(n_threads, 2)

        IF KEYWORD_SET(interactive) THEN BEGIN
            PLOT_NUWT_PEAKS_AND_THREADS, data[*,*,k], slitnum=k, grad=grad_val, /plot_threads, /screen, /use_temp_common_blocks
            READ, ask_tlen, PROMPT='Is the selected min thread length acceptable? (y/n) '
            ask_tlen = strlowcase(ask_tlen)
            IF ask_tlen.startswith('y') THEN BEGIN
                loop_tlen = 0
            ENDIF ELSE BEGIN
                READ, min_tlen_val, PROMPT='Please enter the next min thread length to try: '
            ENDELSE
        ENDIF ELSE BEGIN
            ;end 'loop' if not in interactive mode
            loop_tlen = 0
        ENDELSE
    ENDWHILE

    ;Fill in data gaps and then find waves using FFT
    print, '   Filling data gaps ...'
    PATCH_UP_TEST, /simp_fill

    ; ;### EXPERIMENTAL - increasing number of data points via interpolation
    ; print, '   Upsampling threads ...'
    ; UPSAMPLE_THREADS, scale_factor=2

    IF keyword_set(bootstrap) THEN BEGIN
        IF KEYWORD_SET(pad_fft) THEN BEGIN
            print, '   Running FFT with both padding and bootstrapping ...'
            WAVE_FROM_FFT, /pad_fft, pad_length=pad_length, fill_pad=fill_pad, $
                           /bootstrap, num_bootstrap=num_bootstrap
        ENDIF ELSE BEGIN
            print, '   Running FFT with bootstrapping ...'
            WAVE_FROM_FFT, /bootstrap, num_bootstrap=num_bootstrap
        ENDELSE
    ENDIF ELSE BEGIN
        IF KEYWORD_SET(pad_fft) THEN BEGIN
            print, '   Running FFT with padding ...'
            WAVE_FROM_FFT, /pad_fft, pad_length=pad_length, fill_pad=fill_pad
            ; WAVE_FROM_FFT, /pad_fft, pad_length=100
            ; WAVE_FROM_FFT, /pad_fft, pad_length=1000
            ; WAVE_FROM_FFT, /pad_fft, pad_length=10000
            ; WAVE_FROM_FFT, /pad_fft, pad_length=4096, /fill_pad
            ; WAVE_FROM_FFT, /pad_fft, /iterate_padding
        ENDIF ELSE BEGIN
            print, '   Running FFT ...'
            WAVE_FROM_FFT
            ; WAVE_FROM_FFT, /ctrend
            ; WAVE_FROM_FFT, /adjacent_peaks
            ; WAVE_FROM_FFT, /ctrend, /adjacent_peaks
            ; WAVE_FROM_FFT, window_func='rectangular'
        ENDELSE
    ENDELSE

    ;Append all results to master lists
    nuwt_located.ADD, located
    nuwt_threads.ADD, threads
    nuwt_fft_results.ADD, fft_results
    nuwt_fft_stats.ADD, fft_stats

ENDFOR

IF KEYWORD_SET(save_results) THEN BEGIN
    save_filename = 'nuwt_results_all_slits'+filetag+'.sav'
    print, 'Saving COMMON block data for all slits in:'
    print, '   Save folder: '+save_folder
    print, '   filename: '+save_filename
    SAVE, nuwt_located, nuwt_threads, nuwt_fft_results, nuwt_fft_stats, FILENAME=save_folder+save_filename
ENDIF

print, '##### Finished running NUWT #####'
END
