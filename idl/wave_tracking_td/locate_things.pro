;FUNCTION: Locates peaks in time-distance plot with sub-pixel accuracy

;PROCEDURE OUTLINE: Takes an unsharp masked (and high-pass filtered)
;time-distance diagram and locates the peaks using a find_max crawling
;routine.The maximum then has to have a specific gradient (default
;>0.5) to be classed as a peak. Once a maximum is found a Gaussian fit
;is applied to the surrouding 5 pixels (two either side) to provide
;subpixel position for the maximum. 

;INPUTS: data - time-distance diagram
;        grad - limits on gradient (default = 0.5)
;
;OPTIONAL INPUTS: meas_size - sets number of pixels used in Gaussian fit, 
;                             default value is 5, odd numbers only
;                 errors - errors on intensity for each pixel, i.e., estimates for Photon 
;                          noise, array should be same size as data array. Supplied to 
;                          Gaussian fitting routine. If not set default is 0 errors            
;
;OUTPUTS: located - structure containing both the positions and errors of found peaks.
;                   Saved as COMMON variable so not a routine output as such! 
;
;TO DO: 
;
;HISTORY:
; Created- R. Morton Oct - 2012
; Edit - R. Morton Feb - 2013 - added variable slit size (meas_size) and errors
;        R Morton NOV 2014 - super update! Added structure format to remove all the arrays.
;                            Also added COMMON variables so re-used values/structures are passed automatically
;        R Morton MAR 2015 - fixed bug with definition of initial minimum value to set all located.peaks values to

pro locate_things,data=data,grad=grad,meas_size=meas_size,errors=errors

IF NOT KEYWORD_SET(meas_size) THEN meas_size=5.
meas_size=fix(meas_size)


COMMON located_dat,located, nx, nt

sz=size(data)
nx=sz(1) & nt=sz(2)
located={peaks:fltarr(nx,nt,2), errs:fltarr(nx,nt)}
located.peaks(0:nx-1,0:nt-1,0:1)=(min(data)-10.)<(-10) ;Should ensure no confusion
maxi={vals:fltarr(nx,2), errs:fltarr(nx,2)}

;Sets default gradient if non specified
IF n_elements(grad) EQ 0 THEN grad=0.5

;Begin search in each time-slice
FOR j=0, (nt-1) DO BEGIN
       
       h=0.
       image=data[0:nx-1,j,0]

       ;Search each along x-direction, excludes edges
       FOR i=6,(nx-6) DO BEGIN

          ;Finds maximum
          mx=max(image[i-5:i+5,0,0],loc)

          
          ;Wait till maximum is at centre of search bar
          IF loc EQ 5 THEN BEGIN

             ;Finds gradients either side of the maximum
             in=[-2,-1,0,1,2]
             res=poly_fit(in,image[i-4:i,0,0],2,yfit=fit)
             res2=poly_fit(in,image[i:i+4,0,0],2,yfit=fit2)

             ;If gradients greater than a certain value begin
             ;Gradient of quadratic evaluated at x=0
             IF (res[1] GT grad) AND (res2[1] LT (-1.)*grad) THEN BEGIN

                ;find gaussian fit to surrounding points

                in2=-(meas_size/2)+indgen(meas_size)
                len=meas_size/2
                
                estimates=[image[i,0,0],0.,2.,min(image((i-len):(i+len),0,0))]

                IF n_elements(errors) GT 0. THEN BEGIN
                   errrs=errors[(i-len):(i+len),j,0]
                   
                ENDIF ELSE BEGIN
                   ;errrs=findgen(meas_size)
                   ;errrs(*)=0.0000001
                ENDELSE

                res=gaussfit(in2,image[(i-len):(i+len),0,0],coeff,nterms=4,$
                             estimates=estimates,measure_errors=errrs,sigma=sigma)


                ;For Gaussian fit results to be used the coefficients have to
                ;be less than one pixel from maximum and with an error less than
                ;one pixel. Otherwise position of maximum is used with 0.5 pixel error.
                IF (abs(coeff[1]) LT 1.) AND (sigma[1] LT 1.) THEN BEGIN
                    maxi.vals(h,0)=i+coeff[1]
                    maxi.vals(h,1)=mx
                    maxi.errs(h,0)=sigma[1]
                    ;maxi_errs(h,1)
                ENDIF ELSE BEGIN
                    maxi.vals(h,0)=i
	            maxi.vals(h,1)=mx
	            maxi.errs(h,0)=0.5
                ENDELSE

                h=h+1

             ENDIF

          ENDIF

       ENDFOR


       ;Saves found values
       FOR k=0,h-1 DO BEGIN
           located.peaks(maxi.vals(k,0),j,0)=maxi.vals(k,0)
	   located.peaks(maxi.vals(k,0),j,1)=maxi.vals(k,1)
	   located.errs(maxi.vals(k,0),j)=maxi.errs(k,0) 
       ENDFOR
ENDFOR


END
