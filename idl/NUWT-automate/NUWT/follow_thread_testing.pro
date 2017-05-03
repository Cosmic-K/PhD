;+
;NAME: FOLLOW_THREAD_TESTING
;
;PURPOSE:
;   Find and follow "threads" of local intensity peaks identified by "locate_things.pro"
;
;INPUTS:
;   Main input is the "located_dat" COMMON block with the peak locations
;
;OPTIONAL INPUTS:
;   min_tlen - minimum thread length to be saved in the output structures
;   max_dist_jump - maximum allowable distance (in pixels) between two peaks in
;                   the same thread. Default is 3 pixels
;   max_time_skip - maximum allowable timesteps beweewn two peaks in the same
;                   thread. Default is 4 time steps.
;
;OUTPUTS:
;   threads - array of structures containing the threads found and saved to the
;             "threads_dat" COMMON block. Each thread structure (array element)
;             has the following format.
;               .pos - [nt] long array with the thread position at each timestep.
;                      Values of -1 indicate timesteps outside of the thread and
;                      a 0 indicates timesteps skipped due to no nearby peaks
;               .err_pos - [nt] long array with the thread position errors
;               .bin_flags - [nt] array with flags indicating the type of peak
;                            found in each timestep of the thread. Possible values are:
;                               -1 : time-step not part of thread
;                                0 : data gap inside thread
;                                1 : lower quality data found (not currently used)
;                                    (may be used for filling with rejected peaks)
;                                2 : higher quality data found
;               .start_bin - timestep bin with the first thread position
;               .end_bin - timestep bin with the last thread position
;               .length - total length (in timesteps) of the thread
;
;HISTORY: Name---------Date---------Description
;         R Morton  OCT, 2012 - Initial coding
;         R Morton  ???, 2014 - updated code so 'out' array is same size as No. features found
;         R Morton  NOV, 2014 - super update! Added structure format to remove all
;                               the arrays. Also added COMMON variables so re-used
;                               values/structures are passed automatically
;         R Morton  APR, 2015 - moved some IF statements from moscill.pro to here
;         R Morton  14 MAR, 2016 - Released as version 1.0 of NUWT
;         M Weberg  JULY, 2016 - Modified the program to avoid saving blank results
;                                as the first "thread"
;         M Weberg  SEPT, 2016 - moderate update:
;                                 - fixed a few small bugs
;                                 - if multiple peaks are found in the same timestep
;                                   of the search box, will now select the peak with
;                                   the smallest shift in position
;                                 - added tracking of start_bin, end_bin, and length
;
;TO DO:
;   - merge in the saving of extra gaussian fit variables saved by "locate_things"
;     when the /full_gauss option is set.
;-

PRO FOLLOW_THREAD_TESTING, min_tlen=min_tlen, max_dist_jump=max_dist_jump, max_time_skip=max_time_skip

;Loads common data generated from locate_things
COMMON located_dat, located, nx, nt
COMMON threads_dat, threads

mini = min(located.peaks[*,*,0]) ; min data val possible (normally the 'invalid data' fill value)

;Set minimum thread length to output
IF n_elements(min_tlen) EQ 0 THEN min_tlen = 30

;Setting variables which control the search box size
IF NOT keyword_set(max_dist_jump) THEN max_dist_jump = 3
IF NOT keyword_set(max_time_skip) THEN max_time_skip = 4
IF typename(max_dist_jump) NE 'INT' THEN max_dist_jump = fix(max_dist_jump)
IF typename(max_time_skip) NE 'INT' THEN max_time_skip = fix(max_time_skip)
search_box_width = 2*max_dist_jump + 1
search_box_height = max_time_skip

;Set up dummy arrays
image = located.peaks[*,*,0]
im_err = located.errs[*,*,0]

;Initialize the output array of structures
threads = {pos:fltarr(nt), err_pos:fltarr(nt), bin_flags:intarr(nt), $
           start_bin:-1, end_bin:-1, length:-1}
;Quick key for values in 'bin_flags':
;   -1 : time-step not part of thread
;    0 : data gap inside thread
;    1 : lower quality data found (in the future, may use for filling with rejected peaks)
;    2 : higher quality data found

;Temporary structure to hold the results before appending to output array (or not)
temp_th = {pos:fltarr(nt), err_pos:fltarr(nt), bin_flags:intarr(nt), $
           start_bin:-1, end_bin:-1, length:1}

;Indices used to reorder the search box values such that the thread tracking method is
;correctly biased towards connecting peaks with smaller spatial seperation when given
;the choice between two peaks in the same timestep.
reorder_cols = intarr(search_box_width)
sub_cols = indgen(max_dist_jump)*2 + 1 ;every other col starting at the second
abs_col_shift = indgen(max_dist_jump) + 1 ;relative col shifted from the center
reorder_cols[0] = max_dist_jump ;shift the middle col (no dist change) to the start
reorder_cols[sub_cols] = max_dist_jump - abs_col_shift
reorder_cols[sub_cols+1] = max_dist_jump + abs_col_shift

;Create look-up arrays for finding pixel shift values (cleaner and easier to follow)
dist_pxl_shift = indgen(search_box_width) - max_dist_jump
dist_pxl_shift = rebin(dist_pxl_shift, search_box_width, search_box_height)
dist_pxl_shift = dist_pxl_shift[reorder_cols, *]

time_pxl_shift = indgen(search_box_height) + 1
time_pxl_shift = rebin(reform(time_pxl_shift, 1, search_box_height), search_box_width, search_box_height)

last_timestep = max([max_time_skip, min_tlen])
; - no need to keep looking if the longest thread you could find is too short!
found_first_thread = 0
;this will be set to 1 ('True') once the first valid thread is found and saved
FOR j=0L, nt-(last_timestep+1) DO BEGIN ;Search over time
    FOR i=max_dist_jump, nx-(max_dist_jump+1) DO BEGIN ; Search over x

        IF image[i,j] GT mini THEN BEGIN
            ;Found the start of a possible thread

            ;reset values in temp structure
            temp_th.pos[0:-1] = 0.0
            temp_th.err_pos[0:-1] = 0.0
            temp_th.pos[0:j] = -1.0
            temp_th.err_pos[0:j] = -1.0
            temp_th.bin_flags[0:-1] = -1
            temp_th.start_bin = -1
            temp_th.end_bin = -1
            temp_th.length = 1

            ;Copy over the first data point
            ;(no need to clear since start points cannot get selected by search boxes)
            temp_th.pos[j] = image[i,j]
            temp_th.err_pos[j] = im_err[i,j]

            ;Select the initial search box:
            ;   width = 2*max_dist_jump (in space)
            ;   height = max_time_skip (in time)
            a = image[(i-max_dist_jump):(i+max_dist_jump),j+1:j+max_time_skip]
            a = a[reorder_cols, *]

            h = j ;current peak timestep
            k = i ;current peak distance

			WHILE max(a) GT mini DO BEGIN
                ;keep following the thread as long as there is a pixel with a
                ;value greater than the minimum in the current search box

                b = where(a GT mini) ;returns -1 when it fails to find anything

                IF b[0] LT 0 THEN BEGIN
                    ;no peaks in box; set 'a' to min value so loop is broken
                    a = mini
                ENDIF ELSE BEGIN
                    ;find the coordinates (relative to the current peak) of the
                    ;nearest non-minimum point in the box
                    xm = dist_pxl_shift[b[0]]
                    ym = time_pxl_shift[b[0]]

                    k = k+xm ;update current distance
                    h = h+ym ;update current timestep

                    ;saves values at the new coords then erases them from the
                    ;dummy array so values are not used twice
                    temp_th.pos[h] = image[k,h]
                    temp_th.err_pos[h] = im_err[k,h]

                    image[k,h] = mini
                    im_err[k,h] = mini

                    ;End loop or prepare for next loop
                    IF k LT max_dist_jump OR k GE nx-max_dist_jump THEN BEGIN
                        ;stops loop from crashing as it reaches the side edges
                        a = mini
                    ENDIF ELSE BEGIN
                        IF h GT nt-(max_time_skip+2) THEN BEGIN
                            a = mini
                        ENDIF ELSE BEGIN
                            ;update the search box
                            a = image[(k-max_dist_jump):(k+max_dist_jump),h+1:h+max_time_skip]
                            a = a[reorder_cols, *]
                        ENDELSE
                    ENDELSE

                    temp_th.length = 1 + h - j

                ENDELSE

            ENDWHILE

            ;fill remaining values NOT in the given thread
            temp_th.pos[h+1:-1] = -1
            temp_th.pos[h+1:-1] = -1

            IF temp_th.length GE min_tlen THEN BEGIN

                IF n_elements(where(temp_th.pos GT 0.0)) GT 2.0 THEN BEGIN
                    ;skips threads with less than two real, positive values

                    IF n_elements(where(temp_th.pos EQ 0.0)) LT 0.5*(n_elements(where(temp_th.pos GE 0.0))) THEN BEGIN
                        ;skips enteries where half the data points are set to zero, i.e. no
                        ;value was obtained at fitting stage.

                        loc_good_peaks = where(temp_th.pos GT 0.0, /NULL)
                        loc_gaps = where(temp_th.pos EQ 0.0)
                        temp_th.bin_flags[loc_good_peaks] = 2
                        temp_th.bin_flags[loc_gaps] = 0
                        temp_th.start_bin = j
                        temp_th.end_bin = h
                        IF NOT found_first_thread THEN BEGIN
                            threads = [temp_th]
                            found_first_thread = 1 ;i.e. 'True'
                        ENDIF ELSE BEGIN
                            threads = [temporary(threads), temp_th]
                        ENDELSE
                    ENDIF
                ENDIF
            ENDIF

        ENDIF
    ENDFOR
ENDFOR

out=threads ; used only for testing

END
