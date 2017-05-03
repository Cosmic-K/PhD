;+
;NAME: PATCH_UP_TEST
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

pro PATCH_UP_TEST, fit_flag=fit_flag, simp_fill=simp_fill

;Loads common data generated from follow_thread_fg
COMMON threads_dat, threads

IF n_elements(fit_flag) EQ 0 THEN fit_flag=0

sz = size(threads)
n_threads = sz[1]

;Loading in the correct variables to be patched
CASE fit_flag OF
    ;Option for simple fitting structures
    0: BEGIN
        tval = threads.pos
        terr = threads.err_pos
        END
    ;Options for advanced fitting structures (_FG procedures)
    1: BEGIN
        tval = threads.pos
        terr = threads.err_pos
        END
    2: BEGIN
        tval = threads.inten
        terr = threads.err_inten
        END
    3: BEGIN
        tval = threads.wid
        terr = threads.err_wid
        END
ENDCASE

IF NOT keyword_set(simp_fill) THEN BEGIN
    ;Filling gaps with linear interpolation
    FOR j=0,(n_threads-1) DO BEGIN
        ;skips enteries with less than 2 positive values
        IF (n_elements(where(tval[*,j] gt 0.0))) GT 2 THEN BEGIN

            ;skips enteries where half the data points are set to zero, i.e. no
            ;value was obtained at fitting stage.
            IF (n_elements(where(tval[*,j] EQ 0.0))) LT 0.5*(n_elements(where(tval[*,j] GE 0.0))) THEN BEGIN
                ;FIND CONSECUTIVE ZEROS IN THREAD
                b = where(tval[*,j] EQ 0)
                IF b[0] GT -1 THEN CONSEC, b, lo, hi, num

                ;FILL IN USING LINEAR INTERPOLATION
                IF b[0] GT -1 THEN BEGIN
                    FOR i=0,num-1 DO BEGIN
                        ;note: the messy variable juggling is needed to pass data to LINFILL
                        ;using pass-by-reference (so we actually get the results "passed" back out)
                        temp_sub_arr = tval[*,j]
                        LINFILL, temp_sub_arr, b[lo[i]]-1, b[hi[i]]+1
                        tval[*,j] = temp_sub_arr
                        terr[lo[i]:hi[i],j] = 1.0
                    ENDFOR
                ENDIF
                IF b[0] GT -1 THEN FOR i=0,num-1 DO terr[lo[i]:hi[i],j] = 1.

                ;FIND SINGLE ZEROS AND FILL VIA LINEAR INTERPOLATION
                b = where(tval[*,j] EQ 0)
                IF b[0] GT -1 THEN BEGIN
                    FOR i=0,n_elements(b)-1 DO BEGIN
                        tval[b[i],j,0] = 0.5*tval[b[i]-1,j] + 0.5*tval[b[i]+1,j]
                        terr[b[i],j] = sqrt(tval[b[i]-1,j]^2+tval[b[i]+1,j]^2)
                    ENDFOR
                ENDIF

            ENDIF ELSE BEGIN
                ;Erases all entries where number of 0 elements gt 1/2 positive elements
                tval[*,j] = -1.0
            ENDELSE

        ENDIF ELSE BEGIN
            ;Erases all entries where number of positive elements lt 2
            tval[*,j] = -1.0
        ENDELSE
    ENDFOR
ENDIF ELSE BEGIN
    ;Simple filling of gaps with the previous positive value
    FOR j=0,(n_threads-1) DO BEGIN
        ;skips enteries with less than 2 positive values
        IF (n_elements(where(tval[*,j] gt 0.0))) GT 2 THEN BEGIN

            ;skips enteries where half the data points are set to zero, i.e. no
            ;value was obtained at fitting stage.
            IF (n_elements(where(tval[*,j] EQ 0.0))) LT 0.5*(n_elements(where(tval[*,j] GE 0.0))) THEN BEGIN

                ;if value missing set to same as last pixel and set
                zer = where(tval EQ 0.)
                IF zer[0] NE -1 THEN BEGIN
                    FOR ii=0,n_elements(zer)-1 DO BEGIN
                        tval[zer[ii]] = tval[zer[ii]-1]
                        terr[zer[ii]] = 1.0
                    ENDFOR
                ENDIF

            ENDIF ELSE BEGIN
                ;Erases all entries where number of 0 elements gt 1/2 positive elements
                tval[*,j] = -1.0
            ENDELSE

        ENDIF ELSE BEGIN
            ;Erases all entries where number of positive elements lt 2
            tval[*,j] = -1.0
        ENDELSE
    ENDFOR
ENDELSE

;Saving the patched values back to the input variable arrays
CASE fit_flag OF
    ;Option for simple fitting structures
    0: BEGIN
        threads.pos = tval
        threads.err_pos = terr
        END
    ;Options for advanced fitting structures (_FG procedures)
    1: BEGIN
        threads.pos = tval
        threads.err_pos = terr
        END
    2: BEGIN
        threads.inten = tval
        threads.err_inten = terr
        END
    3: BEGIN
        threads.wid = tval
        threads.err_wid = terr
        END
ENDCASE

END
