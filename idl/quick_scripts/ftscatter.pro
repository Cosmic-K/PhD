PRO ftscatter, list,f,avbinn,avbinn_er,vel=vel,pow=pow

openr,lun,list,/get_lun
array = ''
line = ''
WHILE NOT EOF(lun) DO BEGIN & $
READF, lun, line & $
array = [array, line] & $
ENDWHILE
FREE_LUN, lun

avbinn=fltarr(38)
avbinn_er=fltarr(38)

km = (0.725/16.981891892)*1000.
dt=1.343


FOR i=1, n_elements(array)-1,2 DO BEGIN
;Averaging over 2 time series
restore,array[i]
threads=scope_varfetch('THREADS_FIT_FG')
pos=threads.pos
pos=pos[*,1]

pos1=pos[where(pos ne -1)]

restore,array[i+1]
threads=scope_varfetch('THREADS_FIT_FG')
pos=threads.pos
pos=pos[*,1]

pos2=pos[where(pos ne -1)]

pos=(pos1+pos2)/2.
pos=pos*km
;vel power spec
IF keyword_set(vel) THEN BEGIN
time=findgen(n_elements(pos))*dt
time_fd=shift(time,1)
time_bd=shift(time,-1)

pos_fd=shift(pos,1)
pos_bd=shift(pos,-1)

;pos1=(pos_fd[1:*]-pos_bd[0:-2])/(time_fd[1:*]-time_bd[0:-2])
;pos3=(pos[0:-2]-pos_bd[0:-2])/(time[0:-2]-time_bd[0:-2])
;pos2=(pos_fd[1:*]-pos[1:*])/(time_fd[1:*]-time[1:*])

pos=deriv(time,pos)

ENDIF


N=n_elements(pos)
x=FINDGEN((N - 1)/2) + 1
ti=findgen(n)*dt

;making freq full freq now only need half for ft plotting
IF evod(n) EQ 0 THEN freq = [0.0, X, N/2.,-N/2 + X]/(N*dt) ELSE freq = [0.0, X,-(N/2 + 1) + X]/(N*dt)

signal=fft(pos-mean(pos),dimension=1)

nf=n_elements(freq)
signal=signal[1:floor(nf/2.)]
freq=freq[1:floor(nf/2.)]


IF n_elements(signals) eq 0 THEN signals=alog10((2*dt*n)*abs(signal)^2) ELSE signals=[temporary(signals),alog10((2*dt*n)*abs(signal)^2)]
IF n_elements(freqs) eq 0 THEN freqs=freq ELSE freqs=[temporary(freqs),freq]

p=plot(freq*1000.,(2*dt*n)*abs(signal)^2,/current,/over,/xlog,/ylog,linestyle=6,symbol='*',$
FONT_NAME='Helvetica',ytitle=' Spectral power density [$km^2$ $s^{-2}$ $Hz^{-1}$]',xtitle='Frequency [mHz]',yrange=[10e-8,10E5],xrange=[2,600])

;UNITS IN AXIS NEED CHANGING IF NOT USING VELOCITY per second or not

ENDFOR

av1=signals[where(freqs lt (0.01))]
av2=signals[where((freqs gt (0.01)) and (freqs LT (0.02)))] 
av3=signals[where((freqs gt (0.02)) and (freqs LT (0.03)))]
av4=signals[where((freqs gt (0.03)) and (freqs LT (0.04)))]
av5=signals[where((freqs gt (0.04)) and (freqs LT (0.05)))]
av6=signals[where((freqs gt (0.05)) and (freqs LT (0.06)))]
av7=signals[where((freqs gt (0.06)) and (freqs LT (0.07)))]
av8=signals[where((freqs gt (0.07)) and (freqs LT (0.08)))]
av9=signals[where((freqs gt (0.08)) and (freqs LT (0.09)))]
av10=signals[where((freqs gt (0.09)) and (freqs LT (0.10)))]
av11=signals[where((freqs gt (0.10)) and (freqs LT (0.11)))]
av12=signals[where((freqs gt (0.11)) and (freqs LT (0.12)))]
av13=signals[where((freqs gt (0.12)) and (freqs LT (0.13)))]
av14=signals[where((freqs gt (0.13)) and (freqs LT (0.14)))]
av15=signals[where((freqs gt (0.14)) and (freqs LT (0.15)))]
av16=signals[where((freqs gt (0.15)) and (freqs LT (0.16)))]
av17=signals[where((freqs gt (0.16)) and (freqs LT (0.17)))]
av18=signals[where((freqs gt (0.17)) and (freqs LT (0.18)))]
av19=signals[where((freqs gt (0.18)) and (freqs LT (0.19)))]
av20=signals[where((freqs gt (0.19)) and (freqs LT (0.20)))]
av21=signals[where((freqs gt (0.20)) and (freqs LT (0.21)))]
av22=signals[where((freqs gt (0.21)) and (freqs LT (0.22)))]
av23=signals[where((freqs gt (0.22)) and (freqs LT (0.23)))]
av24=signals[where((freqs gt (0.23)) and (freqs LT (0.24)))]
av25=signals[where((freqs gt (0.24)) and (freqs LT (0.25)))]
av26=signals[where((freqs gt (0.25)) and (freqs LT (0.26)))]
av27=signals[where((freqs gt (0.26)) and (freqs LT (0.27)))]
av28=signals[where((freqs gt (0.27)) and (freqs LT (0.28)))]
av29=signals[where((freqs gt (0.28)) and (freqs LT (0.29)))] 
av30=signals[where((freqs gt (0.29)) and (freqs LT (0.30)))]
av31=signals[where((freqs gt (0.30)) and (freqs LT (0.31)))]
av32=signals[where((freqs gt (0.31)) and (freqs LT (0.32)))]
av33=signals[where((freqs gt (0.32)) and (freqs LT (0.33)))]
av34=signals[where((freqs gt (0.33)) and (freqs LT (0.34)))]
av35=signals[where((freqs gt (0.34)) and (freqs LT (0.35)))]
av36=signals[where((freqs gt (0.35)) and (freqs LT (0.36)))]
av37=signals[where((freqs gt (0.36)) and (freqs LT (0.37)))]
av38=signals[where((freqs gt (0.37)) and (freqs LT (0.38)))]


FOR i=1,38 DO BEGIN 
av=scope_varfetch('av'+strtrim(i,1))
av=av[where(av ne 0)]
                 
nb=n_elements(av)

indx=floor(nb*randomu(seed,nb,500))

m=moment(mean(av[indx],dimension=1))

avbinn[i-1]=10^(mean(av))
avbinn_er[i-1]=2.303*(10^(mean(av)))*sqrt(m[1])


ENDFOR
stop

f=[0.005,((findgen(37)+1)*0.01)+0.005 ]

p7=plot(freqs[sort(freqs)]*1000.,gauss_smooth(10^(signals[sort(freqs)]),9,/edge_truncate),$
color='gold',/over,/curr,/xlog,/ylog,thick=2)

e=errorplot(f*1000.,avbinn,avbinn_er,linestyle=6,symbol='*',FONT_NAME='Helvetica',$
title='Chromospheric power spectrum',/curr,/over,color='red',errorbar_color='red',$
sym_thick=1.5,/xlog,/ylog)



IF keyword_set(vel) THEN BEGIN
restore,pow
v=scope_varfetch('V')
phot=scope_varfetch('MEAN_POWER_PHOTOSPHERE')
chromo=scope_varfetch('MEAN_POWER_CHROMOSPHERE')
erphoto=scope_varfetch('ERR_PHOTO')
erchromo=scope_varfetch('ERR_CHROMO')
;v=v/1000.
ph=errorplot(v,phot,erphoto,linestyle=6,symbol='tu',color='green',sym_thick=1.5,ERRORBAR_color='green',/curr,/over,/ylog,/xlog)
ch=errorplot(v,chromo,erchromo,linestyle=6,symbol='td',color='blue',sym_thick=1.5,ERRORBAR_color='blue',/curr,/over,/ylog,/xlog)
ENDIF

END