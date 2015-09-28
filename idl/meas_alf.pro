PRO meas_alf,tel=tel,ter=ter,debug=debug

;Unit handeling will need to be addressed when chnaging to velocity 
close,1
sz=size(tel)
last_time=sz(2)-1
Mm = (0.725/16.981891892)
tl=tel/Mm
tl_er=ter/Mm

file_nm='22452940_1952_4.txt';will need to be chnaged between runs do not forget.
file_ch=file_test(file_nm)
IF file_ch EQ 1 THEN BEGIN
f_ch=1
WHILE f_ch EQ 1 DO BEGIN
READ,file_nm,PROMPT='File exists, please enter new file name. '
f_ch=file_test(file_nm)
ENDWHILE
ENDIF


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

cgwindow,'cgplot',len[in]*dt,tl[in,last_time]*Mm+(step*last_time)*Mm,yrange=[0,mx+step*last_time]*Mm,$
xrange=[-17,n_elements(tl[*,last_time])+4]*dt,xst=1,xtitle='Time (s)',ytitle='Displacement (Mm)',charsize=1.5,charthick=1.9
FOR i=start,last_time DO BEGIN
in=where(tl[*,i] ne -1)
IF i EQ 0 THEN cgplot,len[in]*dt,tl[in,i]*Mm,/window,/overplot ELSE cgplot,len[in]*dt,tl[in,i]*Mm+step*i*Mm,/window,/overplot
IF i EQ 0 THEN cgerrplot,len[in]*dt,tl[in,i]*Mm+tl_er[in,i]*Mm,tl[in,i]*Mm-tl_er[in,i]*Mm,/addcmd,thick=0.001,width=0.005 ELSE cgerrplot,len[in]*dt,tl[in,i]*Mm+tl_er[in,i]*Mm+step*i*Mm,tl[in,i]*Mm-tl_er[in,i]*Mm+step*i*Mm,/addcmd,thick=0.001,width=0.005

IF i EQ 0 THEN cgtext,in[0]*dt-7*dt,[tl[in[0],i]]*Mm -1*Mm,strtrim(i,2),/window $
ELSE cgtext,in[0]*dt-7*dt,[tl[in[0],i]]*Mm+step*i*Mm -1*Mm,strtrim(i,2),/window

ENDFOR

;##################################
;PICKING WHICH TIME RANGE TO USE

pick,x,y,/window
pick,x1,y1,/window
x111=fix(x1+0.5)
x11=fix(x+0.5)
print,'x=',x11,' x1=',x111

x1=fix(x1/dt)
x=fix(x/dt)

print,x,x1
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
;mat_sig_col=rebin(dum_er[*,icent],d_s(1),500)
mat_sig_col=rebin(sqrt(total((dum_er)^2.,2)/(x1-x)),d_s(1),500)
ref_ts_pls_noiz=(ref_noise*mat_sig_col)+mat_col

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

IF keyword_set(debug) THEN BEGIN

plot,constrain_peaks,constrain_lags,psym=1
pause

plothist,constrain_lags,xhist,yhist,/autobin
oplot,xhist,fit
vline,coeff(1)
vline,median(constrain_lags),color=100,linestyle=1,thick=2
pause

ENDIF

all_lags[i+last_time/2]=coeff(1)
lag_er[i+last_time/2]=coeff(2)

ENDFOR

IF windowavailable(0) THEN wdelete,0

;##################################
;LINEAR FIT TO CC VALUES

np=findgen(last_time)-last_time/2

res=poly_fit(np,all_lags,1,yfit=fit,measure_errors=lag_er,sigma=sig)

param = mpfitfun('lin_func_fit', np, all_lags, lag_er,[res[0],res[1]], /quiet,perror=perr,bestnorm=chi)
i_yfit = param[0] + np*param[1]

cgwindow,'cgplot',np,all_lags,psym=1, yrange=[-1+min(all_lags),1+max(all_lags)],xrange=[min(np)-0.05,max(np)+0.05],xtitle='Time series number',ytitle='Lag values',xmargin=[8,7],ymargin=[4,4]
cgerrplot,np,all_lags+lag_er,all_lags-lag_er,/addcmd
cgplot,np,i_yfit,/overplot,/window,color='red',thick=1.8
cgAxis, YAxis=1, yrange=[-1+min(all_lags),1+max(all_lags)]*dt,/window,title='Time lag (s)'
cgAxis, XAxis=1,xrange=[0,n_elements(np)]*dx,/window,title='Distance (km)'
red_ch=float(chi)/(float(n_elements(np))-2-1.)
a=string(red_ch,Format='(D0.2)')
al_legend,'Reduced $\chi$!E2!N = '+strtrim(a,1),/window,bthick=2,charsize=1.2,charthick=1.7,background_color='rose'


;cgwindow,'cgplot',np,(1./all_lags)*dx/dt,psym=1,xrange=[min(np)-0.05,max(np)+0.05],yrange=[-1+min((1./all_lags)*dx/dt)-100,1+max((1./all_lags)*dx/dt)+100],xtitle='Time series number.',ytitle='Phase speed (km/s).'
;cgerrplot,np,(1./all_lags)*dx/dt+(1./all_lags)*dx/dt*(lag_er/all_lags),(1./all_lags)*dx/dt-(1./all_lags)*dx/dt*(lag_er/all_lags),/addcmd
;res1=poly_fit(np,(1./all_lags)*dx/dt,1,yfit=fit1,measure_errors=(1./all_lags)*dx/dt*(lag_er/all_lags),sigma=sig1,chisq=chi2)
;cgplot,np,fit,/overplot,/window,color='red',thick=1.8
;red_chi2=float(chi2)/(float(n_elements(np))-2-1.)
;a=string(red_chi2,Format='(D0.2)')
;al_legend,'Reduced $\chi$!E2!N = '+strtrim(a,1),/window,bthick=2,charsize=1.2,charthick=1.7,background_color='rose'

;print,'Phase speed ',res1(0),' +/-',sig1(0),' km/s'

alf=1./param[1]*dx/dt
print,'Phase speed ',alf,' +/-',alf*(perr[1]/param[1]),' km/s'

openw,1,file_nm
PRINTF,1,transpose([[(1./all_lags)*dx/dt],[(1./all_lags)*dx/dt*(lag_er/all_lags)]]),FORMAT='(2F)'
PRINTF,1,transpose([['',alf],['',alf*(perr[1]/param[1])]]),FORMAT='(2F)'
;printf,1,transpose([['',res1(0)],['',sig1(0)]]),FORMAT='(2F)'
close,1
END