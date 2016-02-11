pro phase_grad,x,debug=debug

Mm = (0.725/16.981891892)
dx=2.*Mm*1000.
dt=1.343
close,1
;(sqrt((var(6)^2)*redchi))

openr,lun,'lag_grad.txt',/get_lun
array = ''
line = ''
WHILE NOT EOF(lun) DO BEGIN & $
READF, lun, line & $
array = [array, line] & $
ENDWHILE
FREE_LUN, lun

;For some reason the array starts from index 1

FOR i=1,(n_elements(array)-1) DO BEGIN
;print,i
data=read_table(array[i])
sz=size(data)
;undoing incorrect maths in cc

lags=1/(data[0,0:sz(2)-3]/(dx/dt))

lag_er = lags*((data[1,0:sz(2)-3])/(data[0,0:sz(2)-3]))


np=findgen(n_elements(lags))-n_elements(lags)/2

res=poly_fit(np,lags,1,yfit=fit,measure_errors=lag_er,sigma=sig)

param = mpfitfun('lin_func_fit', np, lags, lag_er,[res[0],res[1]], /quiet,perror=perr,bestnorm=chi,dof=df)
i_yfit = param[0] + np*param[1]

grad=abs(param[1])
grad_er=abs(perr[1])

;print, grad

ph_grad=(1./grad)*dx/dt
ph_grad_er=ph_grad*(grad_er/grad)


IF keyword_set(debug) THEN BEGIN
;PLOTTING LAGS
;-------------------------------------------------------------------------------

cgwindow,'cgplot',np,lags,xmargin=[8,7],ymargin=[4,4],xstyle=4,ystyle=4,charthick=2,thick=1.5
cgerrplot,np,lags+lag_er,lags-lag_er,/addcmd
cgplot,np,i_yfit,/overplot,/window,color='red',thick=1.8
cgAxis, YAxis=0,yrange=[-1+min(lags),1+max(lags)],title='Lag values',/window,ystyle=1,ythick=1.5
cgAxis, XAxis=0,xrange=[min(np)-2,max(np)+2],title='Time series number',/window,xstyle=1,xthick=1.5
cgAxis, YAxis=1, yrange=[-1+min(lags),1+max(lags)]*dt,/window,title='Time lag (s)',ystyle=1,ythick=1.5
cgAxis, XAxis=1,xrange=[min(np)-2,max(np)+2]*dx,/window,title='Distance (km)',xstyle=1,xthick=1.5

red_ch=float(chi)/(float(n_elements(np))-2-1.)
a=string(red_ch,Format='(D0.2)')
al_legend,'Reduced $\chi$!E2!N = '+strtrim(a,1),/window,bthick=2,charsize=1.2,charthick=1.7,background_color='rose'
pause
ENDIF

openw,1,'/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/grad/grad_feat.txt',/append
PRINTF,1,transpose([[ph_grad],[ph_grad_er]]),FORMAT='(2F)'
close,1


ENDFOR



data1=read_table('/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/kinkwavemeasha/measured/10154058_36113_4/10154058_36113_4_new.txt')


openw,1,'/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/grad/grad_feat.txt',/append
PRINTF,1,transpose([[abs(data1[0,12])],[abs(data1[1,12])]]),FORMAT='(2F)'
close,1


data2=read_table('/Users/krishnamooroogen/Documents/PHYSICS/PhD/Data/kinkwavemeasha/measured/80967689_31109_0/80967689_31109_0_new.txt')

openw,1,'/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/grad/grad_feat.txt',/append
PRINTF,1,transpose([[abs(data2[0,16])],[abs(data2[1,16])]]),FORMAT='(2F)'
close,1


END