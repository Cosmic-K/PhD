;+
;Program picks up individual threads in binary images. 

;Inputs - data - use binary x-t diagram from locate_things.pro
;         errs - array of errors on position from locate_things.pro
;Outputs - out - array containg individual threads out[*,*,0] and
;                errors out [*,*,1]
;                threads beginning and end marked by -1. values
;                where routine misses pixel in thread a 0. value is recorded
;Optional inputs - min_tlen - change minimum thread length, default is 30
;	         - area - change size of search area used for locating neighbouring pixels
;                         default is 5 by 5 box
;
;HISTORY: 2012 - Created by RJM
;         2014 APR - updated code so 'out' array is same size as No. features found
;         R Morton NOV 2014 - super update! Added structure format to remove all the arrays.
;                             Also added COMMON variables so re-used values/structures are passed automatically
;         R Morton APR 2015 - moved some IF statements from moscill.pro to here.
;         14 MAR 2016 - Release as version 1.0 of NUWT
;         M Weberg - Modified the program to avoid saving blank results as the first "thread"
;-

pro FOLLOW_THREAD_DEBUG, t, min_tlen=min_tlen, area=area, out=out

;Loads common data generated from locate_things_min
COMMON located_dat, located, nx, nt
COMMON threads_dat, threads
COMMON threads_debug, debug

debug = {thread:fltarr(nx, nt)}

mini = min(located.peaks(*,*,0))

;Set minimum thread length if nothing specified
IF n_elements(min_tlen) EQ 0 THEN min_tlen = 30

;Sets size of search area
IF NOT keyword_set(area) THEN area = 6

;Set up dummy arrays
image = located.peaks(*,*,0)
im_err = located.errs(*,*,0)

;Initialize the output array of structures
threads = {pos:fltarr(nt), err_pos:fltarr(nt), bin_flags:intarr(nt)}
;Quick key for values in 'bin_flags':
;   -1 : time-step not part of thread
;    0 : data gap inside thread
;    1 : lower quality data found (not used at the moment)
;    2 : higher quality data found

;Temporary arrays to hold the results before appending to output array (or not)
posi = fltarr(nt)
pos_err = fltarr(nt)
flag_bins = intarr(nt) ;indicates invalid values (-1), data gaps (0), and found values (1)

val = fix(area)/2 ; old - area-2

th_count = 1;total number of POSSIBLE threads found
found_first_thread = 0 ;will be set to 1 (i.e. 'True') once the first valid thread is found and saved to the output array
FOR j=0L,nt-(area+1) DO BEGIN ;Search over time
    FOR i=area,nx-(area+1) DO BEGIN ; Search over x
        ;tlen=0.
        ;posi(0:nt-1)=0. & pos_err(0:nt-1)=0. ; reset pos
        ;posi(0:j)=-1. & pos_err(0:j)=-1
        ;h=j+1

        IF image[i,j] GT mini THEN BEGIN
            ;reset values before finding thread
            tlen = 0.0
            posi[0:nt-1] = 0.0 
            pos_err[0:nt-1] = 0.0
            posi[0:j] = -1.0 
            pos_err[0:j] = -1
            flag_bins[0:-1] = -1
            h = j+1

            ;create box thats (2*val+1) by area-1
            a = image[(i-val):(i+val),h:h+(area/2)+1]
            k = i
            ;set up exit from loop - if no pixel with value greater than minimum
            ;                        in box then end
			WHILE max(a) GT mini DO BEGIN

                ;determines how many points have values greater than minimum
                b = where(a gt mini)

                IF b[0] LT 0 THEN BEGIN
                    a = mini ;if no points then set a to minimum value so loop is broken
                ENDIF ELSE BEGIN
                    ;find coordinates of first non-minimum point in the box
                    xm = b[0] mod (val*2+1)
                    ym = b[0]/(val*2+1)

                    ;saves coordinates then erases them from dummy array 
                    ;so values not used twice
                    k = k-val+xm
                    posi[h+ym] = image[k,h+ym]
                    pos_err[h+ym] = im_err[k,h+ym]

                    image[k,h+ym] = mini
                    im_err[k,h+ym] = mini

                    debug.thread[k,h+ym] = th_count ;assign each pixel to a possbile thread

                    IF ym EQ 0 THEN ym=1

                    h = h+ym

                    ;stops loop from crashing as it reaches end and
                    ;sets box (a) for next loop
                    IF k LT val THEN a=mini ELSE $
                    IF h GT nt-(area/2)-2 THEN a = mini ELSE a = image[(k-val):(k+val),h:h+(area/2)+1]

                    tlen = tlen+1

                ENDELSE

            ENDWHILE

            posi[h:nt-1] = -1
            pos_err[h:nt-1] = -1
            th_count = th_count + 1

            IF tlen GT min_tlen THEN BEGIN

                ;skips enteries with less than 2 positive values
                IF n_elements(where(posi[*] gt 0.0)) GT 2. THEN BEGIN

                    ;skips enteries where half the data points are set to zero, i.e. no
                    ;value was obtained at fitting stage.
                    IF n_elements(where(posi[*] EQ 0.0)) LT 0.5*(n_elements(where(posi[*] GE 0.0))) THEN BEGIN
                        loc_good_peaks = where(posi GT 0.0, /NULL)
                        loc_gaps = where(posi EQ 0.0)
                        flag_bins[loc_good_peaks] = 2
                        flag_bins[loc_gaps] = 0
                        IF NOT found_first_thread THEN BEGIN
                            threads.pos = posi
                            threads.err_pos = pos_err
                            threads.bin_flags = flag_bins
                            found_first_thread = 1 ;i.e. 'True'
                        ENDIF ELSE BEGIN
                            threads = [temporary(threads), {pos:posi, err_pos:pos_err, bin_flags:flag_bins}]
                        ENDELSE
                    ENDIF
                ENDIF
            ENDIF

        ENDIF
    ENDFOR
ENDFOR

out=threads ;FOR testing


END
