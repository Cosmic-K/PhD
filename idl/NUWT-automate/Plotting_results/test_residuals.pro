;+
;NAME: TEST_RESIDUALS
;
;PURPOSE:
;   Quantify the goodness-of-fit of a given data model by appliying two statistical
;   tests on the residuals. Tests for both a normal distribution, using the
;   Kolmogorov–Smirnov (KS) test, and autocovarience, using the Ljung–Box test.
;
;INPUTS:
;   residuals - values obtained by subtracting a model results from the data
;
;OPTIONAL INPUTS:
;   signif_level - significance level to use for determining the chi squared
;                  cut-off limit for the Ljung-Box test. Default is 0.95
;   fit_params - number of parameters fit by the model (or combination of models).
;                Used to apply a correction to the degrees of freedom in determining
;                the chi squared cut-off value. Default is 0 (no model)
;   max_lag - maximum autocorrelation lag to include in Ljung-Box test.
;             Default is num_residuals/2 (this is also the maximum value allowed)
;
;
;OUTPUTS:
;   test_stats - structure containing the test statistics as well as their
;                threshold values and probabilities
;
;
;HISTORY: Name---------Date---------Description
;         M Weberg  July, 2016 - Initial coding
;-

FUNCTION TEST_RESIDUALS, residuals, signif_level=signif_level, num_fit_params=num_fit_params, $
                         max_lag=max_lag, debug_hist=debug_hist

;Set default significance level
IF n_elements(signif_level) EQ 0 THEN signif_level = 0.95

; ;Performing the Kolmogorov–Smirnov two sample test to determine normality
; sigma = stddev(residuals)
; N = n_elements(residuals)
; standard_residuals = residuals/sigma
; resid_hist = HISTOGRAM(standard_residuals, LOCATIONS=resid_bins, min=-6, max=6)
; resid_hist = 1.0*resid_hist/N
;
; gauss_x_vals = 5*(findgen(1001) - 500)/500
; gauss_y_vals = 1/(sqrt(2*!pi))*EXP(-gauss_x_vals^2/2.0)
;
; ;KSTWO, resid_hist, gauss_y_vals, KS_stat, KS_prob
; ;KSTWO, standard_residuals, gauss_y_vals, KS_stat, KS_prob
; KSONE, standard_residuals, 'standard_normal_cdf', real_KS_stat, real_KS_prob

;Performing the Kolmogorov–Smirnov one sample test vs a gaussian CDF to determine normality
KS_stat = kolmogorov_smirnov(residuals, prob=ks_prob)

;Performing the Anderson Darling test for normality
; Disaabled for the time being since there some problem with running this test on macs
; will need to do more testing...
A_sqrd = -1.0
AD_crit_val = -1.0
; A_sqrd = anderson_darling(residuals, signif_level=signif_level, crit_val=AD_crit_val)

;Performing the Ljung-Box test to detect autocorrelation
Q = ljung_box(residuals, signif_level=signif_level, max_lag=max_lag, $
              num_fit_params=num_fit_params, crit_val=chisqr_cutoff)

;Initialize output array
test_stats = {signif_level:signif_level, $
              KS_stat:KS_stat, $
              KS_prob:KS_prob, $
              AD_stat:A_sqrd, $
              AD_crit:AD_crit_val, $
              LB_stat:Q, $
              LB_chisqrd:chisqr_cutoff}

; debug_hist = {residuals:residuals, $
;               norm_resid:standard_residuals, $
;               hist_vals:resid_hist, $
;               hist_bins:resid_bins}

RETURN, test_stats
END
