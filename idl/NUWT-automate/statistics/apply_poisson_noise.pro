;+
;NAME: APPLY_POISSON_NOISE.PRO
;
;PURPOSE:
;   Apply Poisson noise (also called 'shot noise') to an input image. Adapted from
;   'AddPoissonNoise.pro' available online at http://www.idlcoyote.com/code_tips/addpoisson.php
;   This version is more generalized and does not depend on any other custom functions.
;
;INPUTS:
;   image - image which to apply the poisson noise to
;
;OPTIONAL INPUTS:
;   fraction - fraction of noise to apply to the image. Default is 0.05 (i.e. 5%)
;                  cut-off limit for the Ljung-Box test. Default is 0.95
;   seed - seed for random number generator
;
;OUTPUTS:
;   noiseImage - image with the input data and the applied noise
;
;HISTORY: Name---------Date---------Description
;         M Weberg  Aug, 2016 - Initial modification
;-

FUNCTION APPLY_POISSON_NOISE, image, fraction=fraction, seed=seed
; Setting default values
IF n_elements(fraction) THEN fraction = 0.05

h = histogram(image, MIN=0, REVERSE_INDICES=ri)
noiseImage = image
FOR j=0,n_elements(h)-1 DO BEGIN
    currentMean = j > 0.5

    ; Find the indices of elements within each bin
    IF ri[j] NE ri[j+1] THEN BEGIN
        indices = ri[ri[j]:ri[j+1]-1]
        count = n_elements(indices)
    ENDIF ELSE BEGIN
        indices = -1
        count = 0
    ENDELSE

    IF count GT 0 THEN BEGIN
        noiseImage[indices] = RandomU(seed, count, POISSON=currentMean, /double)
        newMean = sqrt(currentMean) / fraction

        ; Adjust the standard deviation to the required level.
        noiseImage[indices] += newMean - currentMean

        ; Now adjust it back to starting the mean, but with noise added.
        noiseImage[indices] *= currentMean / newMean
    ENDIF
ENDFOR

RETURN, noiseImage
END
