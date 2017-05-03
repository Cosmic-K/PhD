;+
;NAME: LOCATE_THINGS_DEBUG
;
;PURPOSE:
;   Locates peaks in time-distance plot with sub-pixel accuracy
;
;PROCEDURE OUTLINE:
;   Takes an unsharp masked (and high-pass filtered) time-distance diagram
;   and locates the peaks using a find_max crawling routine.The maximum then
;   has to have a specific gradient (default > 0.5) to be classed as a peak.
;   Once a maximum is found a Gaussian fit (with linear terms) is applied to
;   the surrouding 5 pixels (two either side) to provide subpixel position
;   for the maximum.
;
;EXTERNAL CALLS: mpfitfun, mygauss_plus_linear, fifteenb (only with /check)
;
;INPUTS:
;   data - time-distance diagram
;   errors - errors on intensity for each pixel, i.e., estimates for Photon
;            noise, array should be same size as data array. Supplied to
;            Gaussian fitting routine. If not set, default is 1
;   grad - limits on gradient (default = 0.5) - Key paramater to modify!
;
;OPTIONAL INPUTS:
;   num_search_bins - number of bins to look on either side of a potential peak
;                     for determining if it is a local maxima. Elsewhere known
;                     as the 'order' of the search algorithm. Default is 5.
;   meas_size - sets number of pixels used in Gaussian fit, default value is 5,
;               please input odd numbers only
;   /simp_grad - set to use an analytic least-sqaures estimate for gradient.
;                This may be faster for very large input arrays!
;   cut_chisq - cut off value for unacceptable chi squared. Default set at 3
;               sigma confidence level (based on number of points used in fit)
;   shift_cut - cut off value for maximum allowable peak shift. If the fit
;               location shifts more than this, will instead use the peak
;               location with an error of 0.5 pixels. default is 1.5
;   /full_gauss - set to output the 'full_gaussian' results (i.e. save the
;                 widths). Replicates the old output of 'locate_things_fg.pro'
;   /check - set to plot various variables about each good peak to a
;            window. NOT recommended for long periods of observations.
;
;OUTPUTS:
;   located - struture containing result arrays that is then saved to the
;             "located_dat" COMMON block. Format is as follows:
;               .peaks - [nx,nt,2] or [nx,nt,6] array with sub-pixel locations
;                        and maximum values at each located peak
;                         - [*,*,0] - sub-pixel location of peak
;                         - [*,*,1] - maximum intensity value at peak
;                         - [*,*,2] - gaussian width (/full_gauss only)
;                         - [*,*,3] - constant coeff (/full_gauss only)
;                         - [*,*,4] - slope of linear tern (/full_gauss only)
;                         - [*,*,5] - i index of maximum data point (/full_gauss only)
;               .errs - [nx,nt] or [nx,nt,6] array with errors on the fit values.
;                       Format is similar to .peaks (but for the errors instead)
;               .allpeaks - [nx,nt] array containing integer flags for ALL
;                           local maxima. The values are as folows:
;                               1 - local peak rejected by choosen gradient
;                               2 - gaussian fit failed. Defaulted to whole pixels
;                               3 - gaussian fit obtained for subpixel resolution
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
;         R Morton  MAR, 2016 - updated so similar to locate_things_fg
;         R Morton  14 MAR, 2016 - Release as version 1.0 of NUWT
;         R Morton  MAY, 2016 - Added option for analytic LS calc for graident in
;                               hope of speed increase.
;                               Also removed needless for loop at bottom!!
;                               Added calcuation of peak intensity
;                               Added better estimate for Chi^2 cutoff
;         M Weberg  MAY, 2016 - Implemented faster inital peak finding. Still requires
;                               looping over the potential peaks for computing gradients
;         M Weberg  JUL, 2016 - Added arrays to output stucture for saving the left- and
;                               right-hand gradient values.
;         M Weberg  JUL, 2016 - IMPORTANT! merged the program 'locate_things.pro' with
;                               'locate_things_fg.pro' and added the /full_gauss keyword
;
;TO DO:
;   - add better handling of flat-topped local maxima. Currently, if two pixels
;     have the same max value, the program will select the left-most as the peak
;   - Still need to work out the best way to describe intensity values for gaussian fit!
;-

pro LOCATE_THINGS_DEBUG, data, errors=errors, grad=grad, num_search_bins=num_search_bins, meas_size=meas_size, $
                         simp_grad=simp_grad, cut_chisq=cut_chisq, shift_cut=shift_cut, full_gauss=full_gauss, check=check

COMMON located_dat, located, nx, nt
COMMON loc_debug_dat, loc_debug

;Sets default gradient if non specified
IF n_elements(grad) EQ 0 THEN grad = 0.5

;Sets the number of bins to search on either side of a potential peak
IF NOT KEYWORD_SET(num_search_bins) THEN num_search_bins = 5

;Sets total number of pixels to use for the gaussian fit
;Note 1: a default value of 7 was also used for measuring fibrils in ROSA data
;Note 2: 5 data points were previosly fit by Thurgood et al. HOWEVER this unfortunatly
;        will not work well with our chosen "guassian plus linear" fit function which
;        attempts to find 5 different fit parameters (therby reducing DOF to zero if
;        we use just 5 bins).
IF NOT KEYWORD_SET(meas_size) THEN meas_size = 7
meas_size = fix(meas_size)
len = floor(meas_size/2.0)

;Set cut off value for unacceptable chi squared
;Set at value of 3 sigma confidence level
IF NOT KEYWORD_SET(cut_chisq) THEN cut_chisq = chisqr_cvf(1.0 - 0.9973, meas_size-5)

;Set cut off value for unacceptable peak shift
IF NOT KEYWORD_SET(shift_cut) THEN shift_cut = 1.5

;Initialize the "output" structure
sz = size(data)
nx = sz[1]
nt = sz[2]
IF KEYWORD_SET(full_gauss) THEN BEGIN
    print, 'Returning full gaussian results'
    ;'full gaussian' output including the fitted widths
    ; Replicates the output of a previously seperate code, 'locate_things_fg.pro'
    located = {peaks:fltarr(nx,nt,6), errs:fltarr(nx,nt,6), $
               allpeaks:intarr(nx,nt), grad_left:fltarr(nx, nt), grad_right:fltarr(nx,nt)}
    last_peaks_index = 5
    last_errs_index = 5
    cut_case_last_peaks_index = 2 ;default for results outside 'shift_cut' range
    cut_case_last_errs_index = 2
ENDIF ELSE BEGIN
    ;reduced output
    located = {peaks:fltarr(nx,nt,2), errs:fltarr(nx,nt), $
               allpeaks:intarr(nx,nt), grad_left:fltarr(nx, nt), grad_right:fltarr(nx,nt)}
    last_peaks_index = 1
    last_errs_index = 0
    cut_case_last_peaks_index = 1 ;default for results outside 'shift_cut' range
    cut_case_last_errs_index = 0
ENDELSE

;Filling the 'peaks' array with invalid negative values
;doing this should avoid confusion with real results (which in theory could be negitive valued)
located.peaks[0:nx-1,0:nt-1,0:last_peaks_index] = (min(data)-10.)<(-10)

mini = min(located.peaks)

;Array for debugging
loc_debug = {shift:fltarr(nx,nt), sigma:fltarr(nx,nt), width:fltarr(nx,nt), $
             chisq:fltarr(nx,nt), coeff:fltarr(nx, nt), $
             cut_shift:shift_cut, cut_sigma:1.5, cut_width:meas_size, cut_chisq:cut_chisq, cut_coeff:mini}

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
in2 = -len + indgen(meas_size)
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
    ;Gradient of quadratic evaluated at x=0
    IF (grad_left[i,j] GT grad) AND (grad_right[i,j] LT (-1.)*grad) THEN BEGIN

        ;find gaussian fit to surrounding points
        estimates = [image[i],0.,2.,min(image[(i-len):(i+len)]),0.1]
        coeff = mpfitfun('mygauss_plus_linear',in2,image[(i-len):(i+len)],err_dat[(i-len):(i+len)],estimates,$
                         perror=sigma,bestnorm=bestnorm,dof=dof,/quiet)
        chisq = bestnorm
        chisq_red = bestnorm/dof

        ;Plots useful stuff to a window
        IF keyword_set(check) THEN BEGIN
            x=i-len+findgen(2*len+1)
            toplo=image[(i-len):(i+len)]
            erplo=err_dat[(i-len):(i+len)]
            yran=[min(toplo)-0.01*abs(mean(toplo)),max(toplo)+0.01*abs(mean(toplo))]
            xran=[min(x)-1,max(x)+1]
            plot,x,toplo,thick=3,yst=1,title='Time frame '+strtrim(j,2),yrange=yran,xrange=xran,xst=1,$
                                       xtitle='Pixels',ytitle='Intensity units',position=[.1,.15,.8,.9],/norm,psym=1
            oploterror,x,toplo,erplo,thick=3,psym=1
            oplot,x,mygauss_plus_linear(in2,coeff),linestyle=2
            oplot,x,coeff[3]+coeff[4]*in2
            xyouts,[0.82,0.82],[0.85,0.85],'GAUSSIAN FIT PARAM',/norm,charsize=1.2
            xyouts,[0.82,0.82],[0.8,0.8],'Center - '+strtrim(string(i+coeff[1],format='(3f0.3)'),2),/norm,charsize=1.2
            xyouts,[0.82,0.82],[0.76,0.76],'Half width - '+strtrim(string(coeff[2],format='(3f0.3)'),2),/norm,charsize=1.2
            xyouts,[0.82,0.82],[0.72,0.72],'Peak - '+strtrim(string(coeff[0],format='(3f0.3)'),2),/norm,charsize=1.2
            xyouts,[0.82,0.82],[0.68,0.68],'Chi!e2!n!dv!n - '+strtrim(string(chisq_red,format='(3f0.3)'),2),/norm,charsize=1.2
            xyouts,[0.82,0.82],[0.64,0.64],'Center error - '+string(sigma[1],format='(3f0.2)'),/norm,charsize=1.2

            xyouts,[0.82,0.82],[0.55,0.55],'ACCEPTABLE LIMITS',/norm,charsize=1.2
            xyouts,[0.82,0.82],[0.5,0.5],'Center- '+string(i,format='(3f0.1)')+'pm'+string(shift_cut,format='(3f0.1)'),/norm,charsize=1.2
            xyouts,[0.82,0.82],[0.46,0.46],'Half width- '+'<'+string(meas_size,format='(3f0.1)'),/norm,charsize=1.2
            xyouts,[0.82,0.82],[0.42,0.42],'Peak- '+'>0',/norm,charsize=1.2
            xyouts,[0.82,0.82],[0.38,0.38],'Chi!e2!n!dv!n- '+'<'+string(cut_chisq/dof,format='(3f0.1)'),/norm,charsize=1.2
            xyouts,[0.82,0.82],[0.34,0.34],'Center error - '+string(1.5,format='(3f0.1)'),/norm,charsize=1.2

            IF (abs(coeff[1]) LT shift_cut) AND (sigma[1] LT 1.5) AND (coeff[2] lt meas_size) $
            AND (chisq lt cut_chisq) AND (sigma[1] GT 0.) AND (coeff[0] gt mini) THEN BEGIN
                xyouts,[0.82,0.82],[0.20,0.20],'Meets criteria',/norm,charsize=1.2
            ENDIF

            clearline=fifteenb()
            form="($,'pause',a,a)"
            print, form=form, '         ',clearline
            pause,/quiet
        ENDIF

        ;For Gaussian fit results to be used the coefficients have to
        ;be less than one pixel from maximum and with an error less than
        ;one pixel. Otherwise position of maximum is used with 0.5 pixel error.
        IF (abs(coeff[1]) LT shift_cut) AND (sigma[1] LT 1.5) AND (coeff[2] LT meas_size) $
        AND (chisq LT cut_chisq) AND (sigma[1] GT 0.) AND (coeff[0] GT mini) THEN BEGIN
            ;Calculate value at peak
            peak_val = mygauss_plus_linear(coeff[1],coeff)
            erpeak = mygauss_plus_linear(coeff[1]+sigma[1],coeff)
            sig_peak = peak_val - erpeak ;estimate of error

            located.peaks[round(i+coeff[1]),j,0:last_peaks_index] = ([i+coeff[1],peak_val,abs(coeff[2]), $
                                                                      coeff[3],coeff[4],i])[0:last_peaks_index]
            located.errs[round(i+coeff[1]),j,0:last_errs_index] = ([sigma[1],sig_peak,sigma[2], $
                                                                    sigma[3],sigma[4],1.])[0:last_errs_index]
            allpeaks[i,j] = 0 ;clear out the shifted peak
            allpeaks[round(i+coeff[1]),j] = 3

            ;Copy over values for debugging
            loc_debug.shift[round(i+coeff[1]),j] = abs(coeff[1])
            loc_debug.sigma[round(i+coeff[1]),j] = sigma[1]
            loc_debug.width[round(i+coeff[1]),j] = coeff[2]
            loc_debug.chisq[round(i+coeff[1]),j] = chisq
            loc_debug.coeff[round(i+coeff[1]),j] = coeff[0]
        ENDIF ELSE BEGIN
            ;gaussian fit failed, defaulting to nearest pixel
            located.peaks[i,j,0:cut_case_last_peaks_index] = ([i,mx,0.0])[0:cut_case_last_peaks_index]
            located.errs[i,j,0:cut_case_last_errs_index] = ([0.5,err_dat[i],0.0])[0:cut_case_last_errs_index]
            allpeaks[i,j] = 2

            ;Copy over values for debugging
            loc_debug.shift[i,j] = abs(coeff[1])
            loc_debug.sigma[i,j] = sigma[1]
            loc_debug.width[i,j] = coeff[2]
            loc_debug.chisq[i,j] = chisq
            loc_debug.coeff[i,j] = coeff[0]
        ENDELSE
    ENDIF
ENDFOR

;transferring more values to the 'output' structure - 'located'
located.allpeaks = allpeaks
located.grad_left = grad_left
located.grad_right = grad_right

;old code for refernce:

;fg results (good peak)
; maxi.vals(h,0)=i+coeff[1]    & maxi.errs(h,0)=sigma[1]
; maxi.vals(h,1)=coeff[0]+coeff[3]      & maxi.errs(h,1)=sqrt(sigma[0]^2+sigma[3]^2)
; maxi.vals(h,2)=abs(coeff[2]) & maxi.errs(h,2)=sigma[2]
; maxi.vals(h,3)=coeff[3]      & maxi.errs(h,3)=sigma[3]
; maxi.vals(h,4)=coeff[4]      & maxi.errs(h,4)=sigma[4]
; maxi.vals(h,5)=i             & maxi.errs(h,5)=1. ;????? Check Error

;fg results (default for results outside cut_shift)
; maxi.vals(h,0)=i             & maxi.errs(h,0)=0.5
; maxi.vals(h,1)=mx            & maxi.errs(h,1)=errors[i,j,0]
; maxi.vals(h,2)=0.            & maxi.errs(h,2)=0.
END
