;+
;NAME: UPSAMPLE_THREADS
;
;FUNCTION:
;   'Patchs up' the threads found from 'locate_things.pro', followed by
;   'follow_thread.pro'. Follow thread can skip forward time frames leaving
;   zero results. This uses interpolation (or simple value filling) to fix
;   these gaps and weights these results with a larger error.
;
;PROCEDURE OUTLINE:
;   The first step is to remove any examples where the number of pixels in the
;   thread is less than 2 and any threads where more than half the pixels could
;   not be found with locate things (i.e. given the value 0). The routine then
;   locates any points that have the value 0. These values are then replaced via
;   linear interpolations given a large error (1 pixel). This is to ensure the
;   weighted fitting routines give limited significance to this value.
;
;OPTIONAL INPUTS:
;   fit_flag - {0-3} defines which time series to work on (see follow_thread_fg.pro/moscill.pro)
;   /simp_fill - if set, will fill missing values with the last non-zero value instead of interpolating
;
;HISTORY:
;   R Morton  May 2014  created initial program
;   M Weberg  June 2016  Cleaned-up code, fixed the call to LINFILL,
;                        and added a simple fill option
;
;TO DO OR THINK ABOUT:
;   - Is linear interpolation the best option?
;   - Should weighting be calculated using traditional error analysis?
;   - Does funny things if first elements in array is a 0 - need to look into this
;-

pro UPSAMPLE_THREADS, scale_factor=scale_factor, simp_fill=simp_fill, linear=linear, cubic=cubic
;note: this program REQUIRES all threads to have already been patched up with "patch_up_test.pro"

;Default values

;The scale factor MUST be an integer and effectivly sets the number of extra, sub-timestep
;data points to be added between each pair of source data (num_extra_points = scale_factor - 1)
IF KEYWORD_SET(scale_factor) THEN scale_factor = fix(floor(scale_factor)) ELSE scale_factor = 2
IF scale_factor LT 1 THEN scale_factor = 1 ;i.e. no scaling



;###############################################
;LOADING DATA
;###############################################

COMMON located_dat, located, nx, nt             ;generated by 'locate_things.pro'
COMMON threads_dat, threads                     ;generated by 'follow_thread.pro'

n_threads = n_elements(threads)

upsample_nt = scale_factor*nt
upsample_bins = indgen(upsample_nt)

convert_bins = indgen(nt)*scale_factor ;converts source bin numbers to upsample bins

;Temporary structure to hold the results before appending to output array
temp_th = {pos:fltarr(upsample_nt), err_pos:fltarr(upsample_nt), bin_flags:intarr(upsample_nt), $
           start_bin:-1, end_bin:-1, length:1}


tval = threads.pos
terr = threads.err_pos

FOR h=0, (n_threads-1) DO BEGIN

    ;reset values in temp structure
    temp_th.pos[0:-1] = 0.0
    temp_th.err_pos[0:-1] = 0.0
    temp_th.pos[0:-1] = -1.0
    temp_th.err_pos[0:-1] = -1.0
    temp_th.bin_flags[0:-1] = -1
    temp_th.start_bin = -1
    temp_th.end_bin = -1
    temp_th.length = 1

    ;Transfer & translate tracking information
    temp_th.start_bin = convert_bins[threads[h].start_bin]
    temp_th.end_bin = convert_bins[threads[h].end_bin]
    temp_th.length = temp_th.end_bin - temp_th.start_bin + 1
    temp_th.bin_flags[convert_bins] = threads[h].bin_flags

    ;Perform interpolation
    source_bin_vals = convert_bins[threads[h].start_bin:threads[h].end_bin] ;in upsample bin numbers "coordinates"
    source_pos_vals = threads[h].pos[threads[h].start_bin:threads[h].end_bin] ;thread position
    source_err_vals = threads[h].err_pos[threads[h].start_bin:threads[h].end_bin] ;thread position
    upsample_bin_vals = upsample_bins[temp_th.start_bin:temp_th.end_bin]

    upsample_pos_vals = interpol(source_pos_vals, source_bin_vals, upsample_bin_vals)

    ;Copy values to temp structure
    temp_th.pos[upsample_bin_vals] = upsample_pos_vals
    temp_th.err_pos[upsample_bin_vals] = 1.0

    ;Overwrite with REAL data where available (just to be safe)
    temp_th.pos[source_bin_vals] = source_pos_vals
    temp_th.err_pos[source_bin_vals] = source_err_vals

    ;Save upsampled thread to output array
    IF h EQ 0 THEN BEGIN
        upsampled_threads = [temp_th]
    ENDIF ELSE BEGIN
        upsampled_threads = [temporary(upsampled_threads), temp_th]
    ENDELSE
ENDFOR

;Overwrite original threads with upsampled versions
threads = upsampled_threads

END