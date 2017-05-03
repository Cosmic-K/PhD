;+
;NAME: NORMAL_QUANTILES
;
;PURPOSE:
;   Compute the quantiles of the normal (gaussian) distribution. Useful for making
;   Q-Q plots and hypothesis testing. Can specify mean and stddev. Currently only
;   accurate to second decimal (i.e. 0.01).
;   Note: the alternative inv_erf calculation is currently not working!
;
;INPUTS:
;   p_vals - quantile values (between 0 and 1) to be computed
;
;OPTIONAL INPUTS:
;   mu - mean value of the gaussian. Default is 0
;   sigma - stddev of the gaussian. Default is 1
;   num_quantiles - If set to an integer, will compute the specified number
;                   of equally spaced quantiles. Note: this will override the
;                   given q_vals!
;
;OUTPUTS:
;   norm_Q - normal quantile values at the given locations
;
;HISTORY: Name---------Date---------Description
;         M Weberg  July, 2016 - Initial coding
;-

FUNCTION NORMAL_QUANTILES, p_vals, mu=mu, sigma=sigma, num_quantiles=num_quantiles, alt=alt
IF n_elements(mu) EQ 0 THEN mu = 0
IF n_elements(sigma) EQ 0 THEN sigma = 1.0

IF n_elements(num_quantiles) GT 0 THEN BEGIN
    N = fix(num_quantiles[0]) ; if given an array, will just use the first element
    ;generate N numbers between 0 & 1 centered in their respectfive value range
    halfstep = 1.0/(N*2.0)
    p_vals = findgen(N)/N + halfstep
ENDIF

x_vals = 2*p_vals - 1.0 ; for calcuating quantiles using inv_ERF

IF KEYWORD_SET(alt) THEN BEGIN ; WARNING THIS IS BROKEN! DO NOT SET!
    print, 'Alternate inv_erf equation is currently broken! Results for testing only!'
    ck = [1, (!PI)/12, (7*!PI^2)/480, (127*!PI^3)/40320, (4369*!PI^4)/5806080]
    inv_erf = 0.5*sqrt(!PI)*(ck[0]*x_vals + ck[1]*x_vals^3 + ck[2]*x_vals^5 $
                             + ck[3]*x_vals^7 + ck[4]*x_vals^9)
ENDIF ELSE BEGIN
    a = 0.147 ;Constant for calculating inverse ERF
    ln_term = ALOG(1.0 - (x_vals)^2)
    combo_term = 2.0/(!PI*a) + ln_term/2.0

    ;Uses the approximation inv_erf(x) = [-2/Pi*a - ln(1-x^2)/2 + SQRT((2/Pi*a + ln(1-x^2)/2)^2 - ln(1-x^2)/a)]^1/2
    inv_erf = SQRT(-combo_term + SQRT(combo_term^2 - ln_term/a))
ENDELSE

;Uses the equation Normal Quantiles = SQRT(2)*inv_erf(2p-1)
norm_Q = mu + sigma*sqrt(2)*inv_erf

;Correct the sign and magnitude of p_vals < 0.5
loc_fix = where(p_vals LT 0.5, /null)
IF n_elements(loc_fix) GT 0 THEN BEGIN
    norm_Q[loc_fix] = -1.0*(norm_Q[loc_fix] - 2*mu)
ENDIF

RETURN, norm_Q
END
