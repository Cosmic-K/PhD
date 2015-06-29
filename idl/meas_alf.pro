PRO meas_alf,tel=tel,ter=ter,debug=debug

;Unit handeling will need to be addressed when chnaging to velocity 

sz=size(tel)
last_time=sz(2)-1
Mm = (0.725/16.981891892)
tl=tel/Mm
tl_er=ter/Mm

nf=last_time
start=0
step=4
dx=2.;*Mm*1000
dt=1.343

;##################################
;INITIAL PLOTTING

len=findgen(n_elements(tl[*,last_time]))
in=where(tl[*,last_time] ne -1)
mn=min(tl[in,last_time])
mx=max(tl[*,last_time])

plot,len[in],tl[in,last_time]+(step*last_time),yrange=[0,mx+step*nf],$
xrange=[-12,n_elements(tl[*,last_time])]+4,xst=1

FOR i=start,nf DO BEGIN
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
	th1=start & th2=nf
ENDIF ELSE BEGIN
	temp=fix(get_words(th))
        IF n_elements(temp) EQ 1 THEN BEGIN
                th1=temp[0] & th2=nf 
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
nf=th2-th1+1
IF nf mod 2 eq 0 THEN nf=nf-1
all=fltarr(nf)
icent=nf/2

FOR i=-nf/2,nf/2 DO BEGIN
	lag=ccpeak(dum[*,icent]-mean(dum[*,icent]),dum[*,icent+i]-mean(dum[*,icent+i]),5)
	all[i+nf/2]=lag
ENDFOR
c=[0.,median(deriv(all))]

IF keyword_set(debug) THEN BEGIN
plot,all
pause
ENDIF

;##################################
;CREATE SUPER TIME-SERIES FOR 2ND CC
col=fltarr(nt,nf)
FOR i=0,nf-1 DO BEGIN
  shift=c(1)*(i-nf/2)+c(0)
  col[*,i]=interpolate(dum(*,i),findgen(nt)+shift)
ENDFOR
col=median(col,dimension=2)
;need some kind of errors

IF keyword_set(debug) THEN BEGIN
	plot,dum[*,0],yst=1,yrange=[min(dum[*,0])<min(dum[*,nf-1]),max(dum[*,0])>max(dum[*,nf-1])]
	oplot,dum[*,nf-1]
	oplot,col,linestyle=2
	pause
ENDIF

;##################################
;SECOND ROUND OF CROSS CORRELATION


;peak=fltarr(nf)
col_sub=col-mean(col)

;FOR i=-nf/2,nf/2 DO BEGIN
;    test_ser=dum[*,icent+i]-mean(dum[*,icent+i])
;	lag=ccpeak(col_sub,test_ser,5,c_cor=c_cor)
;	all[i+nf/2]=lag    ;Lag position
;    peak[i+nf/2]=c_cor ;value of CC
;ENDFOR

;##################################
;ERRORS MONTE CARLO
lag_er=fltarr(nf)
lags=fltarr(500)
peaks=fltarr(500)

test_noise=randomn(seed,d_s(1),500)
ref_noise=randomn(seed,d_s(1),500)
mat_col=rebin(col_sub,d_s(1),500)
mat_sig_col=rebin(dum_er[*,icent],d_s(1),500)
ref_ts_pls_noiz=(ref_noise*mat_sig_col)+mat_col

FOR i=-nf/2,nf/2 DO BEGIN

mat_test=rebin((dum[*,icent+i]-mean(dum[*,icent+i])),d_s(1),500)
mat_test_sig=rebin(dum_er[*,icent+i],d_s(1),500)
test_ts_pl_noiz=(test_noise*mat_test_sig)+mat_test

FOR j=0,499 DO BEGIN
lag=ccpeak(ref_ts_pls_noiz[*,j],test_ts_pl_noiz[*,j],5,c_cor=c_cor); can it handle arrays? otherwise double loop?
lags[j]=lag
peaks[j]=c_cor;do something with this see paper
ENDFOR

IF keyword_set(debug) THEN BEGIN
plothist,lags,xhist,yhist,/autobin,/noplot
est=[max(yhist,j),j,0.1]
fit=gaussfit(xhist,yhist,coeff,sigma=error,estimates=est,nterms=3,chisq=chi)

plothist,lags,xhist,yhist,/autobin;,/fill,fcolor='blue',color='blue'
loadct,2,/silent
oplot,xhist,fit,color=100,linestyle=1
loadct,0,/silent
pause
ENDIF

all[i+nf/2]=median(lags)
lag_er[i+nf/2]=stdev(lags)


ENDFOR



;##################################
;LINEAR FIT TO CC VALUES
;
np=findgen(nf)-nf/2

;SIMPLIFIED TEST OF CC:
;Null hypothesis:
;Correlation value has to be greater than that of random series. Cross correlation
;of random series should have sample correlations of mean 0 and variance 1/N
;N is sample number
;Using Normal CDF and two tailed test you obtain a cutoff value for cross correlation
;A cutoff of 1.0sigma is p=0.68 giving significance level of ~0.994/sqrt(N)
;A cutoff of 1.5sigma is p=0.866 giving significance level of ~1.49/sqrt(N)
;A cutoff of 2.0sigma is p=0.95 giving significance level of ~1.96/sqrt(N)
;A cutoff of 2.5sigma is p=0.987 giving significance level of ~2.58/sqrt(N)
;A cutoff of 3sigma is p=0.997 giving significance level of ~2.99997/sqrt(N)
;A cutoff of 4sigma is p=0.9999 giving significance level of ~3.99996/sqrt(N)
;A cutoff of 5 sigma is p=0.999 999 giving significance level of ~4.9999/sqrt(N)

good_p=where(peak gt 1.49/sqrt(nt)) ;Rejection of CC values

;Simple estimate of the error on the CC values
;Stand Dev of the sample population (Bevington & Robinson)
;Use only values with 'good' CC values 
;err=fltarr(nf)
err=lag_er[good_p]

res=poly_fit(np[good_p],all[good_p],1,yfit=fit,measure_errors=err)
;res=poly_fit(np[good_p],all[good_p],1,yfit=fit)

;err[good_p]=sqrt(total( (all[good_p]-fit)^2 )/(n_elements(good_p)-2))


;Constrained linear fit
;parinfo = replicate({value:0.D, fixed:0, limited:[0,0], limits:[0.D,0.D]}, 2)
;parinfo[1].limited[0] = 1 
;parinfo[1].limited[1] = 1 
;parinfo[0].fixed = 1

;if (res[1] lt 0) then begin
; parinfo[1].limits[0]  = -20D
; parinfo[1].limits[1]  = -0.001D
; parinfo[*].value      = [0D,-0.5D] 
;endif else begin
; parinfo[1].limits[0]  = 0.001D
; parinfo[1].limits[1]  = 20D
; parinfo[*].value      = [0D,0.5D] 
;endelse

param = mpfitfun('lin_func_fit', np(good_p), all(good_p), err,[res[0],res[1]], /quiet,perror=perr)
;param = mpfitfun('lin_func_fit', np(good_p), all(good_p), err[good_p],[res[0],res[1]], /quiet,perror=perr)
i_yfit = param[0] + np*param[1]

plot,np,all,psym=1, yrange=[-1+min(all),1+max(all)]
oplot,np,res[1]*np+res[0],linestyle=2
oploterr,np(good_p),all(good_p),err(good_p)
oplot,np,i_yfit

alf=1./param[1]*dx/dt
print,'Phase speed ',alf,' km/s',1./res[1]*dx/dt
print,'error ',alf*(perr[1]/param[1])
END