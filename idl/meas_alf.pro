PRO meas_alf,tel=tel,ter=ter,debug=debug

;Unit handeling will need to be addressed when chnaging to velocity 

sz=size(tel)
last_time=sz(2)-1
Mm = (0.725/16.981891892)
tl=tel/Mm
tl_er=ter/Mm

start=0
step=4
dx=2.*Mm*1000.
dt=1.343

;##################################
;INITIAL PLOTTING

len=findgen(n_elements(tl[*,last_time]))
in=where(tl[*,last_time] ne -1)
mn=min(tl[in,last_time])
mx=max(tl[*,last_time])

plot,len[in],tl[in,last_time]+(step*last_time),yrange=[0,mx+step*last_time],$
xrange=[-12,n_elements(tl[*,last_time])]+4,xst=1

FOR i=start,last_time DO BEGIN
in=where(tl[*,i] ne -1)
IF i EQ 0 THEN oplot,len[in],tl[in,i] ELSE oplot,len[in],tl[in,i]+step*i
IF i EQ 0 THEN xyouts,in[0],[tl[in[0],i]],strtrim(i,2) $
ELSE xyouts,in[0]-5,[tl[in[0],i]]+step*i,strtrim(i,2)

ENDFOR


;##################################
;PICKING WHICH TIME RANGE TO USE
pick,x,y
pick,x1,y1
x1=fix(x1+0.5)
x=fix(x+0.5)
print,'x=',x,' x1=',x1

nt=x1-x+1


;##################################
;PICKING WHICH SERIES TO USE
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

;##################################
;READ IN FILES TO USE
FOR i=th1,th2 DO BEGIN
IF i eq th1 THEN dum=tl[x:x1,i]  ELSE dum=[[dum],[tl[x:x1,i]]]
IF i eq th1 THEN dum_er=tl_er[x:x1,i] ELSE dum_er=[[dum_er],[tl_er[x:x1,i]]]
ENDFOR
d_s=size(dum)

;##################################
;INITIAL CROSS-CORRELATION 
last_time=th2-th1+1
IF last_time mod 2 eq 0 THEN last_time=last_time-1
all=fltarr(last_time)
icent=last_time/2

FOR i=-last_time/2,last_time/2 DO BEGIN
	lag=ccpeak(dum[*,icent]-mean(dum[*,icent]),dum[*,icent+i]-mean(dum[*,icent+i]),5)
	all[i+last_time/2]=lag
ENDFOR
c=[0.,median(deriv(all))]

IF keyword_set(debug) THEN BEGIN
plot,all
pause
ENDIF

;##################################
;CREATE SUPER TIME-SERIES FOR 2ND CC
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

;##################################
;SECOND ROUND OF CROSS CORRELATION

col_sub=col-mean(col)

;##################################
;ERRORS MONTE CARLO
lag_er=fltarr(last_time)
cc_peak=fltarr(last_time)
lags=fltarr(500)
peaks=fltarr(500)


test_noise=randomn(seed,d_s(1),500)
ref_noise=randomn(seed,d_s(1),500)

mat_col=rebin(col_sub,d_s(1),500)
mat_sig_col=rebin(dum_er[*,icent],d_s(1),500)
ref_ts_pls_noiz=(ref_noise*mat_sig_col)+mat_col

FOR i=-last_time/2,last_time/2 DO BEGIN

mat_test=rebin((dum[*,icent+i]-mean(dum[*,icent+i])),d_s(1),500)
mat_test_sig=rebin(dum_er[*,icent+i],d_s(1),500)
test_ts_pl_noiz=(test_noise*mat_test_sig)+mat_test

window,1

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

IF keyword_set(debug) THEN BEGIN

plot,constrain_peaks,constrain_lags,psym=1
pause

plothist,constrain_lags,xhist,yhist,/autobin
oplot,xhist,fit
vline,coeff(1)
vline,median(constrain_lags),color=100,linestyle=1,thick=2
pause

ENDIF

all[i+last_time/2]=coeff(1)
lag_er[i+last_time/2]=coeff(2)

ENDFOR

;##################################
;LINEAR FIT TO CC VALUES

np=findgen(last_time)-last_time/2

res=poly_fit(np,all,1,yfit=fit,measure_errors=lag_er,sigma=sig)

param = mpfitfun('lin_func_fit', np, all, lag_er,[res[0],res[1]], /quiet,perror=perr)
i_yfit = param[0] + np*param[1]

plot,np,all,psym=2, yrange=[-1+min(all),1+max(all)]
oplot,np,res[1]*np+res[0],linestyle=2
oploterr,np,all,lag_er
oplot,np,i_yfit

alf=1./param[1]*dx/dt
print,'Phase speed ',alf,' km/s'
print,'error ',alf*(perr[1]/param[1])

t=findgen(x1-x)+x
wset,0
oplot,t,(t*(1./param[1]))

END