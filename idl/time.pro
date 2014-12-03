
set_plot,'ps'
device,/encapsul,/color,filename='pictutre.eps'

sinewave = 3*SIN(2.0*FINDGEN(1000)*!PI/100)
sinewave2 = 2*SIN(2.0*FINDGEN(1000)*!PI/100)
sinewave3 = SIN(2.0*FINDGEN(1000)*!PI/100)

cgplot,sinewave,color='green',xtitle='Time in seconds', ytitle='Amplitude',title='Sine wave time series.',background='white'
cgplot, sinewave2,/Overplot,color='blue'
cgplot, sinewave3,/Overplot,color='red'
al_legend, ['Amplitude=3','Amplitude=2','Amplitude=1'],psym=[4,4,4],colors=['green','blue','red']
device,/close

;window,1

device,/encapsul,/color,filename='pictutre2.eps'
cgplot, abs(fft(sinewave))^2,color='green',xtitle='Frequency Hz^-3', ytitle='Power',title='Sine wave power spectra.',background='255'
cgplot, abs(fft(sinewave2))^2,/Overplot,color='blue'
cgplot, abs(fft(sinewave3))^2,/Overplot,color='red'
al_legend, ['power spec amplitude=3','power spec amplitude=2','power spec amplitude=1'],psym=[4,4,4],colors=['green','blue','red']

device,/close

set_plot,'x'

