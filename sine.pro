sinewave = 3*SIN(2.0*FINDGEN(1000)*!PI/100)
sinewave2 = 2*SIN(2.0*FINDGEN(1000)*!PI/100)
sinewave3 = SIN(2.0*FINDGEN(1000)*!PI/100)

cgplot,sinewave,color='green',xtitle='Time in seconds', ytitle='Amplitude',title='Sine wave time series.',background='white'
cgplot, sinewave2,/Overplot,color='blue'
cgplot, sinewave3,/Overplot,color='red'
al_legend, ['Amplitude=3','Amplitude=2','Amplitude=1'],psym=[4,4,4],colors=['green','blue','red']

cgwindow,1

cgplot, abs(fft(sinewave))^2,color='green',xtitle='Time in seconds', ytitle='Amplitude',title='Sine wave time series.',background='white'
cgplot, abs(fft(sinewave2))^2,/Overplot,color='blue'
cgplot, abs(fft(sinewave3))^2,/Overplot,color='red'
al_legend, ['power spec amplitude=3','power spec amplitude=2','power spec amplitude=1'],psym=[4,4,4],colors=['green','blue','red']

;snapshot=TVRD(True=1)
;Write_JPEG, 's.jpeg',snapshot,True=1,Quality=75
;Write_EPS, 's.eps',snapshot,True=1,Quality=75
;cgcontrol,Output='s.png'