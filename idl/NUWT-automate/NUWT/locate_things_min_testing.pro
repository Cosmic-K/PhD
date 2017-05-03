;+
;NAME: LOCATE_THINGS_MIN_TESTING
;
;PURPOSE:
;   Locates peaks in time-distance plot with pixel accuracy - reduced version
;   of 'locate_things_testing.pro'
;
;PROCEDURE OUTLINE:
;   Takes an unsharp masked (and high-pass filtered) time-distance diagram and
;   locates the peaks using a find_max crawling routine.The maximum then has
;   to have a specific gradient (default > 0.5) to be classed as a peak.
;
;INPUTS:
;   data - time-distance diagram
;   errors - errors on intensity values in the td diagram. Used only for
;            weighting the data for linear fitting & finding the gradient
;   grad - limits on gradient (default = 0.5) - Key paramater to modify!
;
;OPTIONAL INPUTS:
;   num_search_bins - number of bins to look on either side of a potential peak
;                     for determining if it is a local maxima. Elsewhere known
;                     as the 'order' of the search algorithm. Default is 5.
;   /simp_grad - set to use an analytic least-sqaures estimate for gradient.
;                This may be faster for very large input arrays!
;   test_var - dummy variable used for testing paramater passing methods.
;              Will be removed at some point!
;
;OUTPUTS:
;   located - struture containing result arrays that is then saved to the
;             'located_dat' COMMON block. Format is as follows:
;               .peaks - [nx,nt,2] array showing nearest whole pixel positions
;                        of located maximum values (.peaks[*,*,0]) and value of
;                        maximum (.peaks[*,*,1]).
;               .errs - [nx,nt] array showing errors on fits to position
;               .allpeaks - [nx,nt] array containing binary flags for ALL
;                           local maxima, regardless of gradient. A value of '1'
;                           indicates a potential peak.
;               .grad_left - [nx,nt] array with the gradient values on the
;                            lefthand side of ALL potential peaks
;               .grad_right - same as as the above but for right-hand side gradients
;
;HISTORY: Name---------Date---------Description
;         R Morton  OCT, 2012 - Initial coding
;         R Morton  NOV, 2014 - super update! Added structure format to remove all
;                               the arrays. Also added COMMON variables so re-used
;                               values/structures are passed automatically
;         R Morton  MAR, 2015 - fixed bug with definition of initial minimum value
;                               to set all located.peaks values to
;         R Morton  14 MAR, 2016 - Release as version 1.0 of NUWT
;         R Morton  MAY, 2016 - Added option for analytic LS calc for graident in
;                               hope of speed increase. Also removed needless FOR
;                               loop at bottom!!
;         M Weberg  MAY, 2016 - Implemented faster inital peak finding. Still requires
;                               looping over the potential peaks for computing gradients
;         M Weberg  JUL, 2016 - Added arrays to output stucture for saving the left- and
;                               right-hand gradient values.
;TO DO:
;   - add better handling of flat-topped local maxima. Currently, if two pixels
;     have the same max value, the program will select the left-most as the peak
;-

PRO LOCATE_THINGS_MIN_TESTING, data, errors=errors, grad=grad, num_search_bins=num_search_bins, $
                               simp_grad=simp_grad, test_var=test_var

COMMON located_dat, located, nx, nt

IF KEYWORD_SET(test_var) THEN BEGIN
    print, "The test vars is set to: ", test_var
ENDIF

;Sets default gradient if non specified
IF n_elements(grad) EQ 0 THEN grad = 0.5

;Sets the number of bins to search on either side of a potential peak
IF NOT KEYWORD_SET(num_search_bins) THEN num_search_bins=5

;Initialize the "output" structure
sz = size(data)
nx = sz[1]
nt = sz[2]
located = {peaks:fltarr(nx,nt,2), errs:fltarr(nx,nt), $
           allpeaks:intarr(nx,nt), grad_left:fltarr(nx, nt), grad_right:fltarr(nx,nt)}
located.peaks[*,*,*] = (min(data)-10.)<(-10) ;Fills with negitive values to ensure no confusion with found data

;IF NO ERRORS GIVEN USE WEIGHTS EQUAL TO 1
IF n_elements(errors) LE 1 THEN BEGIN
   errors = fltarr(nx,nt)
   errors[0:nx-1,0:nt-1] = 1.
ENDIF

;New, faster loop for finding all peaks at once
allpeaks = intarr(nx,nt)
allpeaks[*,*] = 1 ;all pixels are potentially peaks to start
FOR bin=1, num_search_bins DO BEGIN
    ;note: if two pixels have the same value (i.e. flat-topped), this algorithm
    ;will select the left-most pixel as the local max (IMPROVE IN THE FUTURE?)
    compare_right = data - SHIFT(data, -bin, 0)
    compare_left = data - SHIFT(data, bin, 0)
    not_pk_locs = where(allpeaks GT 0 and (compare_left LE 0 or compare_right LT 0), /NULL)
    allpeaks[not_pk_locs] = 0 ;removes incorrectly flagged peaks
ENDFOR

;Cleans out false peaks too close to the edges of the array
allpeaks[0:num_search_bins, *] = 0
allpeaks[-num_search_bins:-1, *] = 0

;Finds the i,j indices of each potential peak
pk_locs = where(allpeaks GT 0, /NULL)
num_peaks_to_test = N_ELEMENTS(pk_locs)
pk_indices = array_indices(allpeaks, pk_locs)

;Loops over each potential peak and tests for the required gradient
grad_right = fltarr(nx,nt)
grad_left = fltarr(nx,nt)
;grad_right[*,*] = 999.9
;grad_left[*,*] = -999.9
in = [-2,-1,0,1,2]
j = -1
FOR p=0L, num_peaks_to_test-1 DO BEGIN
    i = pk_indices[0, p]
    IF pk_indices[1, p] GT j THEN BEGIN
        j = pk_indices[1, p]
        image=reform(data[0:nx-1,j,0])
        err_dat=reform(errors[0:nx-1,j,0])
    ENDIF

    mx = image[i,0,0] ;value at the peak

    ;Finds gradients either side of the maximum
	IF NOT keyword_set(simp_grad) THEN BEGIN
        ;(default) finding slope on either side
        res = poly_fit(in,image[i-4:i],1,yfit=fit,measure_errors=err_dat[i-4:i])
        res2 = poly_fit(in,image[i:i+4],1,yfit=fit2,measure_errors=err_dat[i:i+4])
        grad_left[i,j] = res[1]
        grad_right[i,j] = res2[1]
    ENDIF ELSE BEGIN
        ;Analytic gradient
        grad_left[i,j] = total(in*(image[i-4:i]-total(image[i-4:i])/5.0))/10.0
        grad_right[i,j] = total(in*(image[i:i+4]-total(image[i:i+4])/5.0))/10.0
    ENDELSE

	;If gradients greater than a certain value begin
	IF (grad_left[i,j] GT grad) AND (grad_right[i,j] LT (-1.)*grad) THEN BEGIN
	    ;maxi.vals(h,0:1)=[i,mx]
	    ;maxi.errs(h,0)=0.5
        ;h=h+1
        located.peaks[i,j,0:1] = [i,mx]
        located.errs[i,j] = 0.5
        allpeaks[i,j] = 2
	ENDIF
ENDFOR

;transferring more values to the 'output' structure - 'located'
located.allpeaks = allpeaks
located.grad_left = grad_left
located.grad_right = grad_right

END
