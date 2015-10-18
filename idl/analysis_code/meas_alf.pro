;Krishna Mooroogen
;Northumbria University
;krishna.mooroogen@northumbria.ac.uk
;PhD supervisor RIchard Morton
;---------------------------------------------------------------------------
;NOTES
;Check separation distance befor estarting
;Check filename for saving before stating
;Check Errors for sesimology part
;Maybe chnage code so that it is always zeroed on the first time series
;Be aware of changing propagation direction
;Be aware of areas of interpolation and larger errors
;Unit handeling will need to be addressed when chnaging to velocity
;
;
;PURPOSE
;----------------------------------------------------------------------------
;Measure phase speeds propagation of times series along fibril axis using cross correlation
;
;
DESCRIPTION
;----------------------------------------------------------------------------
;Plots time series (separation not to scale for visual reasons can be chnaged)
;Picks time interval to begin CC
;Picks time series range
;First CC improves signal to noise of ref signal
;Second CC compares all ts to the ref signal and calculates lag times
;Lag times and the separatio ndistance between ts use to determine phase speeds
;Bootstarapping used to determine errors
;weighted mean used to get final phase speed
;
;
;INPUTS
;-----------------------------------------------------------------------------
;TEL - Time series array with multiple time series
;TER - Error on the time series
;Debug - optional shows plots


PRO meas_alf,tel=tel,ter=ter,debug=debug

;----------------------------------------------------------------------------
;CLOSING ANY OPEN FILES
close,1
close,2

;DEFINE CONSTANTS
;----------------------------------------------------------------------------
sz=size(tel)
last_time=sz(2)-1
Mm = (0.725/16.981891892)
tl=tel/Mm
tl_er=ter/Mm

;FOR PLOTTING

start=0
step=4
dx=1.*Mm*1000. ; may need to be chnaged if sep is diff
dt=1.343

;SETTING FILENAMES FOR OUPUTS
;----------------------------------------------------------------------------

file_nm='13283752_1930_2.txt';will need to be chnaged between runs do not forget.
file_nm_2='13283752_1930_2_phv_zero.txt'

file_ch=file_test(file_nm)
IF file_ch EQ 1 THEN BEGIN
f_ch=1
WHILE f_ch EQ 1 DO BEGIN
READ,file_nm,PROMPT='File exists, please enter new file name. '
f_ch=file_test(file_nm)
ENDWHILE
ENDIF

;INITIAL PLOTTING
;-----------------------------------------------------------------------------
;

len=findgen(n_elements(tl[*,last_time]))
in=where(tl[*,last_time] ne -1)
mn=min(tl[in,last_time])
mx=max(tl[*,last_time])

cgwindow,'cgplot',len[in]*dt,tl[in,last_time]*Mm+(step*last_time)*Mm,yrange=[0,mx+step*last_time]*Mm,$
xrange=[-17,n_elements(tl[*,last_time])+4]*dt,xst=1,xtitle='Time (s)',ytitle='Displacement (Mm)',charsize=1.5,charthick=1.9
FOR i=start,last_time DO BEGIN
in=where(tl[*,i] ne -1)
IF i EQ 0 THEN cgplot,len[in]*dt,tl[in,i]*Mm,/window,/overplot ELSE cgplot,len[in]*dt,tl[in,i]*Mm+step*i*Mm,/window,/overplot
IF i EQ 0 THEN cgerrplot,len[in]*dt,tl[in,i]*Mm+tl_er[in,i]*Mm,tl[in,i]*Mm-tl_er[in,i]*Mm,/addcmd,thick=0.001,width=0.005 ELSE cgerrplot,len[in]*dt,tl[in,i]*Mm+tl_er[in,i]*Mm+step*i*Mm,tl[in,i]*Mm-tl_er[in,i]*Mm+step*i*Mm,/addcmd,thick=0.001,width=0.005

IF i EQ 0 THEN cgtext,in[0]*dt-7*dt,[tl[in[0],i]]*Mm -1*Mm,strtrim(i,2),/window $
ELSE cgtext,in[0]*dt-7*dt,[tl[in[0],i]]*Mm+step*i*Mm -1*Mm,strtrim(i,2),/window

ENDFOR

;PICKING WHICH TIME RANGE TO USE
;------------------------------------------------------------------------------
;

pick,x,y,/window
pick,x1,y1,/window
x111=fix(x1+0.5)
x11=fix(x+0.5)
print,'x=',x11,' x1=',x111

x1=fix(x1/dt)
x=fix(x/dt)

print,x,x1
nt=x1-x+1

;PICKING WHICH SERIES TO USE
;------------------------------------------------------------------------------
;

print,'Pick which series to use (e.g., 1 8):'
th=''
read,th,prompt='Enter series: '

IF th EQ '' THEN BEGIN
	print,'Using all series'
	th1=start & th2=last_time
ENDIF ELSE BEGIN
	temp=fix(get_words(th))
        IF n_elements(temp) EQ 1 THEN BEGIN
                th1=temp[0] & th2=last_time 
        ENDIF ELSE BEGIN
	        th1=temp[0] & th2=temp[1]
        ENDELSE
	print,'Using series ',th1,' to ',th2
ENDELSE


;READ IN TS FILES TO USE
;------------------------------------------------------------------------------
;

FOR i=th1,th2 DO BEGIN
IF i eq th1 THEN dum=tl[x:x1,i]  ELSE dum=[[dum],[tl[x:x1,i]]]
IF i eq th1 THEN dum_er=tl_er[x:x1,i] ELSE dum_er=[[dum_er],[tl_er[x:x1,i]]]
ENDFOR
d_s=size(dum)


;INITIAL CROSS-CORRELATION 
;-----------------------------------------------------------------------------
;

last_time=th2-th1+1
IF last_time mod 2 eq 0 THEN last_time=last_time-1
all_lags=fltarr(last_time)
icent=last_time/2

FOR i=-last_time/2,last_time/2 DO BEGIN
	lag=ccpeak(dum[*,icent]-mean(dum[*,icent]),dum[*,icent+i]-mean(dum[*,icent+i]),5)
	all_lags[i+last_time/2]=lag
ENDFOR
c=[0.,median(deriv(all_lags))]

IF keyword_set(debug) THEN BEGIN
plot,all_lags
pause
ENDIF

;IMPROVE S/N FOR REF TIME-SERIES FOR 2ND CC
;-------------------------------------------------------------------------------


col=fltarr(nt,last_time)
FOR i=0,last_time-1 DO BEGIN
  shift=c(1)*(i-last_time/2)+c(0)
  col[*,i]=interpolate(dum(*,i),findgen(nt)+shift)
ENDFOR
col=median(col,dimension=2)

IF keyword_set(debug) THEN BEGIN
	plot,dum[*,0],yst=1,yrange=[min(dum[*,0])<min(dum[*,last_time-1]),max(dum[*,0])>max(dum[*,last_time-1])]
	oplot,dum[*,last_time-1]
	oplot,col,linestyle=2
	pause
ENDIF


col_sub=col-mean(col)

;ERRORS MONTE CARLO/BOOTSTRAP
;-------------------------------------------------------------------------------
;

lag_er=fltarr(last_time)
cc_peak=fltarr(last_time)
lags=fltarr(500)
peaks=fltarr(500)


test_noise=randomn(seed,d_s(1),500)
ref_noise=randomn(seed,d_s(1),500)

mat_col=rebin(col_sub,d_s(1),500)
;mat_sig_col=rebin(dum_er[*,icent],d_s(1),500)
mat_sig_col=rebin(sqrt(total((dum_er)^2.,2)/(x1-x)),d_s(1),500)
ref_ts_pls_noiz=(ref_noise*mat_sig_col)+mat_col


;SECOND ROUND OF CROSS CORRELATION
;-------------------------------------------------------------------------------
;


FOR i=-last_time/2,last_time/2 DO BEGIN

mat_test=rebin((dum[*,icent+i]-mean(dum[*,icent+i])),d_s(1),500)
mat_test_sig=rebin(dum_er[*,icent+i],d_s(1),500)
test_ts_pl_noiz=(test_noise*mat_test_sig)+mat_test

FOR j=0,499 DO BEGIN
lag=ccpeak(ref_ts_pls_noiz[*,j],test_ts_pl_noiz[*,j],5,c_cor=c_cor,ccf=cf)
lags[j]=lag
peaks[j]=c_cor
ENDFOR

constrain_lags=lags(where((peaks gt 0)))
constrain_peaks=peaks(where((peaks gt 0)))

plothist,constrain_lags,xhist,yhist,/autobin,/noplot
est=[max(yhist),median(constrain_lags),0.1]
fit=gaussfit(xhist,yhist,coeff,estimates=est,sigma=errors,chisq=chi,nterm=3)

;DEBUG PLOTS
;-------------------------------------------------------------------------------

IF keyword_set(debug) THEN BEGIN

plot,constrain_peaks,constrain_lags,psym=1
pause

plothist,constrain_lags,xhist,yhist,/autobin,/noplot
cghistoplot,constrain_lags,/window,xtitle='Lags'
cgplot,xhist,fit,/window,/overplot
vline,coeff(1)
vline,median(constrain_lags),color=100,linestyle=1,thick=2
pause

ENDIF

all_lags[i+last_time/2]=coeff(1)
lag_er[i+last_time/2]=coeff(2)

ENDFOR

IF windowavailable(0) THEN wdelete,0

;LINEAR FIT TO CC VALUES
;-------------------------------------------------------------------------------
;


np=findgen(last_time)-last_time/2

res=poly_fit(np,all_lags,1,yfit=fit,measure_errors=lag_er,sigma=sig)

param = mpfitfun('lin_func_fit', np, all_lags, lag_er,[res[0],res[1]], /quiet,perror=perr,bestnorm=chi)
i_yfit = param[0] + np*param[1]

;PLOTTING LAGS
;-------------------------------------------------------------------------------

cgwindow,'cgplot',np,all_lags,xmargin=[8,7],ymargin=[4,4],xstyle=4,ystyle=4
cgerrplot,np,all_lags+lag_er,all_lags-lag_er,/addcmd
cgplot,np,i_yfit,/overplot,/window,color='red',thick=1.8
cgAxis, YAxis=0,yrange=[-1+min(all_lags),1+max(all_lags)],title='Lag values',/window,ystyle=1
cgAxis, XAxis=0,xrange=[min(np)-0.05,max(np)+0.05],title='Time series number',/window,xstyle=1
cgAxis, YAxis=1, yrange=[-1+min(all_lags),1+max(all_lags)]*dt,/window,title='Time lag (s)',ystyle=1
cgAxis, XAxis=1,xrange=[min(np)-0.05,max(np)+0.05]*dx,/window,title='Distance (km)',xstyle=1

red_ch=float(chi)/(float(n_elements(np))-2-1.)
a=string(red_ch,Format='(D0.2)')
al_legend,'Reduced $\chi$!E2!N = '+strtrim(a,1),/window,bthick=2,charsize=1.2,charthick=1.7,background_color='rose'


d=0
de=0

pv=(1./all_lags)
pve=(1./all_lags)*(lag_er/all_lags)

;---------------------------------------------------------------------------------------------

;SECTION FOR CHNAGING THE ZERO POINT
;NOT CARRIED FORWARD JUST FOR SEISMOLOGY STUFF
;ARE THE ERRORS CORRECT?
;
;

shift_pv=shift(pv,1)
d_lag=pv[1:*]-shift_pv[1:*]
ph_val=dx*d_lag

spve=shift(pve,1)
d_lag_er=sqrt(pve[1:*]^2+spve[1:*]^2)
ph_val_er=ph_val*(d_lag_er/d_lag)

openw,2,file_nm_2
PRINTF,2,transpose([[ph_val],[ph_val_er]]),FORMAT='(2F)'
close,2

;---------------------------------------------------------------------------------------------

;SAVING PHASE SPEEDS
;-------------------------------------------------------------------------------
;


in=pv(where(abs(pv) LT 10))

phase_values=pv
phase_errors=pve

middle=n_elements(phase_values)/2
print,middle
print,n_elements(phase_values)

FOR j=0,n_elements(phase_values)-1 DO BEGIN
IF j lt middle THEN BEGIN
d=[temporary(d),phase_values(j)*(middle-j)*dx/dt]
de=[temporary(de),phase_errors(j)*(middle-j)*dx/dt]
ENDIF ELSE BEGIN
d=[temporary(d),phase_values(j)*(j-middle)*dx/dt]
de=[temporary(de),phase_errors(j)*(j-middle)*dx/dt]
ENDELSE
ENDFOR

d=d[1:*]
de=de[1:*]

d=d[in]
de=de[in]

dd=d[(where(abs(d) NE 0))]
dde=de[(where(abs(d) NE 0))]

weighted_ph=total(abs(dd)/(dde^2))/total(1/(dde^2))
w_error=sqrt(1./total(1/dde^2))

;NOW OUTPUTS PHASE VAUES AND WEIGHTED MEAN
;-----------------------------------------
openw,1,file_nm
PRINTF,1,transpose([[dd],[dde]]),FORMAT='(2F)'
PRINTF,1,transpose([['',weighted_ph],['',w_error]]),FORMAT='(2F)'
close,1

PRINT, weighted_ph,'+/-',w_error
END