;PURPOSE - Fits around profile minimum to find Doppler Shift and profile minimum
;          intensity. Called by create_dv_map.
;
;INPUTS - cube - wavelength cube [x,y,lambda]
;       - wav  - array of wavelength values
;       - lambda - central wavelength
;       - method - method of fitting the line profile
;                  1) Centroid method
;                  2) Polynomial Fits
;                  3) Analytic fit to parabola - 3 points about min (very fast)
;                  4) 3 term Gaussfit fit to 3 points about min
;                  5) 3 term mpfit fit to 5 points about min
;
;OPTIONAL INPUTS - split - how many cores to split between, default=1. Generates
;                          IDL child processes on available cores. Will be slower
;                          if only splitting over a few cores (~<4). Only avilable for method 4.
;
;OUTPUTS - vel=vel,itc=itc,
;
;CALLING - Method 3 is very quick and gives comparable looking velocities to 4/5
;          Method 4 can be quick if using the split keyword and you have a lot of
;          cores available on your local machine!
;
;          DO NOT USE 1 AND 5 AT THE MOMENT - NOT FULLY FUNCTIONING
;
;
;HISTORY - Created by RJ Morton & K Mooroogen 05/2016
;
;TO DO - Complete method 1 & 5 by adding in intensity calculation
;        Add method to fit full profile to find width of the spectral line (maybe extension of 5)
;        Complete method 6 - COG method
;         


;Centroid method
FUNCTION COM, ivg,wav_diff,inten=inten

  common constants, c, lambda ,nx,ny,nw 
  vel=total(ivg*wav_diff,3)/total(ivg,3)*c/lambda/1.e3
  return,vel
END

;Polynomial fit
FUNCTION polyf, ivg,wav_diff,inten=inten,loc=loc

  common constants, c, lambda,nx,ny,nw 

  res=min(ivg,dim=3,loc)
  vel=fltarr(nx,ny)
  inten=fltarr(nx,ny)
  x=transpose(rebin([-1,0,1],3,nx,ny),[1,2,0])*nx*ny+rebin(loc,nx,ny,3)
  y=ivg[x]
  wa=wav_diff[x] 

  FOR j=0,ny-1 DO FOR i=0,nx-1 DO BEGIN
       xin=reform(wa[i,j,*])
       yin=reform(y[i,j,*])
       
       res=poly_fit(xin,yin,2)
       vel[i,j]=-res[1]/2./res[2]
       inten[i,j]=res[0]+vel[i,j]*res[1]+vel[i,j]^2*res[2]   ;minimum of parabola
  ENDFOR


END

;Analytic minimum of parabola
FUNCTION parabmin,ivg,wav_diff,inten=inten,loc=loc

  common constants, c, lamb,nx,ny,nw 

  res=min(ivg,dim=3,loc)
  x=transpose(rebin([-1,0,1],3,nx,ny),[1,2,0])*nx*ny+rebin(loc,nx,ny,3)
  y=ivg[x]
  wa=wav_diff[x]

  ;Analytic solution
  ana=wa[*,*,2]*(y[*,*,1]-y[*,*,0])+wa[*,*,1]*(y[*,*,0]-y[*,*,2])+wa[*,*,0]*(y[*,*,2]-y[*,*,1])
  anb=wa[*,*,2]^2*(y[*,*,0]-y[*,*,1])+wa[*,*,1]^2*(y[*,*,2]-y[*,*,0])+wa[*,*,0]^2*(y[*,*,1]-y[*,*,2])
  anc=wa[*,*,1]*wa[*,*,2]*(wa[*,*,1]-wa[*,*,2])*y[*,*,0]+wa[*,*,2]*wa[*,*,0]*(wa[*,*,2]-wa[*,*,0])*y[*,*,1]+$
      wa[*,*,0]*wa[*,*,1]*(wa[*,*,0]-wa[*,*,1])*y[*,*,2]
  denom=(wa[*,*,0]-wa[*,*,1])*(wa[*,*,0]-wa[*,*,2])*(wa[*,*,1]-wa[*,*,2])

  vel=-0.5*anb/ana
  inten=anc/denom-anb^2/4./ana/denom
 
   return,vel
END

;Gaussian fit
FUNCTION gfit, ivg,wav_diff,inten=inten,split=split,loc=loc

  common constants, c, lamb,nx,ny,nw 

  res=min(ivg,dim=3,loc)
  x=transpose(rebin([-2,-1,0,1,2],5,nx,ny),[1,2,0])*nx*ny+rebin(loc,nx,ny,5)
  y=ivg[x]
  wa=wav_diff[x]


  velc=fltarr(nx,ny)
  inten=fltarr(nx,ny)

  IF n_elements(split) LT 1 THEN split=1 ; default no-split

  ;Process that generates child processes
  IF split GT 1 THEN BEGIN
        SPLIT_FOR, 0,ny-1,commands=[$
        'FOR j=0,nx-1 DO BEGIN', $
        'xin=reform(wa[j,i,*]) & yin=reform(y[j,i,*]) &', $
        'z=max(yin)',$ 
        'yin-=max(yin) & estimates=[min(yin),xin[2],0.1] &',$
        'res=gaussfit(xin,yin,nterms=3,coeff,estimates=estimates) &',$
        'IF n_elements(vtemp) EQ 0 THEN vtemp=[j+nx*i,coeff[1]] ELSE vtemp=[[temporary(vtemp)],[j+nx*i,coeff[1]]]', $
        'IF n_elements(itemp) EQ 0 THEN itemp=z+coeff[0] ELSE itemp=[temporary(itemp),z+coeff[0]]', $
        'ENDFOR'], $
         varnames=['nx','wa','y'], $
         outvar=['vtemp','itemp'],nsplit=split      
            
            
        old_len=0
        FOR kk=0,split-1 DO BEGIN
             temp=scope_varfetch('vtemp'+strtrim(kk,2))
             len=max(floor(temp[0,*]/nx))+1
             dum=fltarr(nx,len-old_len)
             dum[*]=temp[1,*]
             velc[0:nx-1,old_len :len-1]=dum

             temp=scope_varfetch('itemp'+strtrim(kk,2))
             dum=fltarr(nx,len-old_len)
             dum[*]=temp[*]
             inten[0:nx-1,old_len :len-1]=dum
             old_len=len
             
        ENDFOR 
  ENDIF ELSE BEGIN
        ;Non-split version
        FOR i=0,ny-1 DO BEGIN 
            FOR j=0,nx-1 DO BEGIN
                xin=reform(wa[j,i,*]) & yin=reform(y[j,i,*])
                z=max(yin) 
                yin-=max(yin) & estimates=[min(yin),xin[2],0.1] 
                res=gaussfit(xin,yin,nterms=3,coeff,estimates=estimates) 
                velc[j,i]=coeff[1]
                inten[j,i]=z+coeff[0]
                counter,i*nx+j,nx*ny,/percent
            ENDFOR
        ENDFOR
  ENDELSE

  
  return,velc
END

;Gaussian fit
FUNCTION gfitm, ivg,wav_diff

  common constants, c, lamb,nx,ny,nw 

  res=min(ivg,dim=3,loc)
  x=transpose(rebin([-2,-1,0,1,2],5,nx,ny),[1,2,0])*nx*ny+rebin(loc,nx,ny,5)
  y=ivg[x]
  wa=wav_diff[x]

  vel=fltarr(nx,ny)
  err=fltarr(5)
  err[*]=1.

  FOR j=0,ny-1 DO FOR i=0,nx-1 DO BEGIN
      xin=reform(wa[i,j,*])
      yin=reform(y[i,j,*])
      yin-=max(yin)
      estimates=[min(yin),xin[2],0.1]
      weights=fltarr(n_elements(xin))
      weights[*]=1.       

      coeff=mpfitfun('mygaussred',xin,yin,weights,estimates,weights=weights,xtol=1e-3,/quiet)
      
      IF n_elements(coeff) gt 1 THEN vel[i,j]=coeff[1] 
      
      counter,j*nx+i,nx*ny,/percent
  ENDFOR 

  ;Will definitely be bad values
  in=where(loc/nx/ny gt nw-2)
  IF n_elements(in) gt 0 THEN vel[in]=wav_diff[loc[in]/nx/ny] 
  in=where(loc/nx/ny lt 1)
  IF n_elements(in) gt 0 THEN vel[in]=wav_diff[loc[in]/nx/ny]   
  in=where(vel gt max(wav_diff))
  vel[in]=max(wav_diff)
  in=where(vel lt min(wav_diff))
  vel[in]=min(wav_diff)
  
  return,vel
END

;COG method
;Isn't functioning at the moment!!!
FUNCTION cogfit, ivg,wav_diff

  common constants, c, lamb,nx,ny,nw 

  conto=smooth(ivg[*,*,0],9,/edge_truncate)
  contt=smooth(ivg[*,*,nw-1],9,/edge_truncate)
  icont=rebin((conto+contt)/2.,nx,ny,nw)
  wav_diff=wav_diff+lamb

  fint=wav_diff*(icont-ivg)
  sint=(icont-ivg)

  vel=fltarr(nx,ny)
  vel=total(fint,3)/total(sint,3)

  ;Will definitely be bad values
  in=where(vel gt max(wav_diff))
  vel[in]=max(wav_diff)
  in=where(vel lt min(wav_diff))
  vel[in]=min(wav_diff)
  
  return,vel
END

;voigt fit
FUNCTION vfit, ivg,wav_diff,inten=inten,dopwid=dopwid,split=split,loc=loc

  common constants, c, lamb,nx,ny,nw 
  help,ivg
  res=min(ivg,dim=3,loc)
  x=transpose(rebin([-3,-2,-1,0,1,2,3],7,nx,ny),[1,2,0])*nx*ny+rebin(loc,nx,ny,7)
  y=ivg[x]
  wa=wav_diff[x]

  dopwid=fltarr(nx,ny)
  velc=fltarr(nx,ny)
  inten=fltarr(nx,ny)

  IF n_elements(split) LT 1 THEN split=1 ; default no-split

  ;Process that generates child processes
  IF split GT 1 THEN BEGIN
        SPLIT_FOR, 0,ny-1,commands=[$
        'FOR j=0,nx-1 DO BEGIN', $
        'xin=reform(wa[j,i,*]) & yin=reform(y[j,i,*]) &', $
        'z=max(yin)',$ 
        'estimates=[max(yin),min(yin),2.84,0.13,xin[3]] &',$
        'fit=voigtfit(xin,yin,coeff,guess=estimates,/quiet) &',$
        'IF n_elements(vtemp) EQ 0 THEN vtemp=[j+nx*i,coeff[1]] ELSE vtemp=[[temporary(vtemp)],[j+nx*i,coeff[1]]]', $
        'IF n_elements(itemp) EQ 0 THEN itemp=z+coeff[0] ELSE itemp=[temporary(itemp),z+coeff[0]]', $
        'IF n_elements(wtemp) EQ 0 THEN wtemp=coeff[3] ELSE wtemp=[temporary(wtemp),coeff[3]]', $
        'ENDFOR'], $
         varnames=['nx','wa','y'], $
         outvar=['vtemp','itemp','wtemp'],nsplit=split      
            
            
        old_len=0
        FOR kk=0,split-1 DO BEGIN

             temp=scope_varfetch('vtemp'+strtrim(kk,2))
             len=max(floor(temp[0,*]/nx))+1
             dum=fltarr(nx,len-old_len)
             dum[*]=temp[1,*]
             velc[0:nx-1,old_len :len-1]=dum

             temp=scope_varfetch('wtemp'+strtrim(kk,2))
             dum=fltarr(nx,len-old_len)
             dum[*]=temp[*]
             dopwid[0:nx-1,old_len :len-1]=dum
            
             temp=scope_varfetch('itemp'+strtrim(kk,2))
             dum=fltarr(nx,len-old_len)
             dum[*]=temp[*]
             inten[0:nx-1,old_len :len-1]=dum
             old_len=len
             
        ENDFOR 
  ENDIF ELSE BEGIN
        ;Non-split version
        FOR i=0,ny-1 DO BEGIN 
            FOR j=0,nx-1 DO BEGIN
                xin=reform(wa[j,i,*]) & yin=reform(y[j,i,*])
                z=max(yin) 
		print,'no split'
                estimates=[max(yin),min(yin),2.84,0.13,xin[3]] 
                fit=voigtfit(xin,yin,coeff,guess=estimates,/quiet) 
		velc[j,i]=coeff[4]
                inten[j,i]=z+coeff[1]
                dopwid[j,i]=coeff[3]
                counter,i*nx+j,nx*ny,/percent
            ENDFOR
        ENDFOR
  ENDELSE

  
  return,velc
END

;Main routine
PRO fm_vel_spt_width,cube,wav,lambda=lambda,vel=vel,intc=intc,wid=wid,method=method,split=split

;on_error,2 

common constants, c, lamb ,nx,ny,nw 


;Parameter check
IF n_elements(lambda) EQ 0 THEN message,'Need central wavelength in A'

sz=size(cube)
IF sz(0) EQ 3 THEN BEGIN
   nx=sz(1) & ny=sz(2) & nw=sz(3)
ENDIF ELSE message, 'Input has does not have correct dimensions - [x,y,lambda] needed'

IF n_elements(method) EQ 0 THEN BEGIN
    method=3
    print,'No method specified using default method - 3'
ENDIF


c=299792458*1d  ;speed of light
lamb=lambda

;FOV average
avgprof=total(total(cube,1),1)/nx/ny
avgprof=transpose(rebin(temporary(avgprof),nw,nx,ny),[1,2,0])


wav_diff=wav ;(lambda-lambda_0)
wav_diff=transpose(rebin(temporary(wav_diff),nw,nx,ny),[1,2,0])

;Finds wavelength offset
CASE method OF 

    1: res=com(cube,wav_diff)
    2: res=polyf(cube,wav_diff,inten=intc,loc=loc)
    3: res=parabmin(cube,wav_diff,inten=intc,loc=loc)
    4: res=gfit(cube,wav_diff,split=split,inten=intc,loc=loc)
    5: res=cogfit(cube,wav_diff)
    6: res=gfitm(cube,wav_diff)
    7: res=vfit(cube,wav_diff,split=15,inten=intc,dopwid=wid,loc=loc)
   
    ELSE: message,'Not a method, choose 1-5'

ENDCASE

 ;Account for extreme values in output
  in=where(loc/nx/ny gt nw-2)
  IF n_elements(in) gt 0 THEN BEGIN
     res[in]=wav_diff[loc[in]/nx/ny]
     intc[in]=cube[loc[in]] 
  ENDIF
  in=where(loc/nx/ny lt 1)
  IF n_elements(in) gt 0 THEN BEGIN
      res[in]=wav_diff[loc[in]/nx/ny]
      intc[in]=cube[loc[in]] 
  ENDIF

  in=where(res gt max(wav_diff))
  IF n_elements(in) gt 0 THEN BEGIN
     res[in]=wav_diff[loc[in]/nx/ny]
     intc[in]=cube[loc[in]]   
  ENDIF

  in=where(res lt min(wav_diff))
  IF n_elements(in) gt 0 THEN BEGIN
     res[in]=wav_diff[loc[in]/nx/ny]
     intc[in]=cube[loc[in]]  
  ENDIF

;Calculates velocity
vel=res/lamb*299792458 


END
