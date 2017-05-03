;FFT code for FFT of multiple time series 
;INPUTS 
;f: result for file search, search for files matching the location index 
;result will include text files will filter these out automatically
PRO ft1,files,errors=errors,ph_list=ph_list

pos1=files[1:190,0]
pos_er1=errors[1:190,0]

;inx=where(pos1 ne -1)
;pos1=pos1[inx]
;pos_er1=pos_er1[inx]

aln_er=make_array(n_elements(pos1),value=0.164214)
pos_er1=sqrt(pos_er1^2 + aln_er^2)
N=n_elements(pos1)
x=FINDGEN((N - 1)/2.) + 1
dt=1.343
ti=findgen(n)*dt

;making freq full freq now only need half for ft plotting
IF evod(n) EQ 0 THEN freq = [0.0, X, N/2.]/(N*dt) ELSE freq = [0.0, X]/(N*dt)

;bootstrap 1 for first signal 
pos_matrix=rebin(pos1-mean(pos1),n,500)
noise_mat1=randomn(seed,n,500)*rebin(sqrt((pos_er1^2.)/n),n,500)

pos_matrix_plsnoise=pos_matrix+noise_mat1

ftt=fft(pos_matrix_plsnoise,dimension=1)

signal1=mean(ftt,dimension=2)


tp=plot(freq[1:*],abs(signal1[1:*]),/ylog,/xlog,xstyle=2)

A_one=signal1[1]
A_two=signal1[2]
A_three=signal1[3]

col=['black','blue','green','orange','dark goldenrod','sky blue','pink','purple','rosy brown','indian red','aqua',$
'green yellow','sandy brown','firebrick','navy','plum','deep pink','aquamarine','salmon','powder blue','orchid','indigo','cornflower','peru','sea green']

;p=errorplot(ti,pos1,pos_er1,color='black',errorbar_color='black',xstyle=3,thick=0.5,errorbar_thick=0.5,$
;	xtitle='seconds',ytitle='pixels')

;rebuild=(2*abs(A_one)*cos(2*!pi*ti*freq[1]+atan(A_one,/phase))+2*abs(A_two)*cos(2*!pi*ti*freq[2]+atan(A_two,/phase))+2*abs(A_three)*cos(2*!pi*ti*freq[3]+atan(A_three,/phase)))+mean(pos1)


;p1=plot(ti,rebuild,color='red',xstyle=3,thick=0.5,linestyle=3,/over,/current)

;stop

FOR i=1,9 DO BEGIN
;looping over rest of series using the cut off from the first 

pos=files[1:190,i]
pos_er=errors[1:190,i]
;pos=pos[inx]
;pos_er=pos_er[inx] 

pos_er=sqrt(pos_er^2+aln_er^2)

;p=errorplot(findgen(n)*dt,pos+i*5.,pos_er,color=strtrim(col[i],1),errorbar_color=strtrim(col[i],1)$
;	,/current,/over,xstyle=3,thick=0.5,errorbar_thick=0.5,xtitle='seconds',ytitle='pixels')

;bootstrap 2 
pos_matrix=rebin(pos-mean(pos),n,500)
noise_mat=randomn(seed,n,500)*rebin(sqrt(pos_er^2./n),n,500)
pos_matrix_plsnoise=pos_matrix+noise_mat
ftt=fft(pos_matrix_plsnoise,dimension=1)
signal=mean(ftt,dimension=2)

;phase and cohrence 

csrb=signal1*conj(signal)

phase = atan(csrb,/phase)

csrbsm = smooth(signal1,5,/edge_truncate)*conj(smooth(signal,5,/edge_truncate))
csrr = smooth(signal1,5,/edge_truncate)*conj(smooth(signal1,5,/edge_truncate))
csbb = smooth(signal,5,/edge_truncate)*conj(smooth(signal,5,/edge_truncate))
coh = (csrbsm^2/(csrr*csbb))

;overplots

;p=plot(freq[1:*],abs(signal[1:*])/abs(signal1[1:*]),color=col[i],/overplot,linestyle=5,/current,xstyle=3,/ylog)
p=plot(freq[1:*],abs(signal[1:*]),color=col[i],/overplot,linestyle=5,/current,xstyle=3,/ylog,/xlog)
;p=plot(freq[1:*],1/3.*(signal+shift(abs(signal[1:*]),1)+shift(abs(signal[1:*]),2)),color=col[i],/overplot,linestyle=5,/current,xstyle=3,/ylog)

;p1=plot(freq[1:*],phase[1:*],color=col[i],/current,layout=[3,3,i])
;p1=plot(freq[1:*],1/3.*(phase[1:*]+shift(abs(phase[1:*]),1)+shift(abs(phase[1:*]),2)),color=col[i],/current,layout=[4,6,i])
;p2=plot(freq[1:*],1/3.*(coh(where(real_part(coh[1:*]) gt 0.7))+shift(abs(coh(where(real_part(coh[1:*]) gt 0.7))),1)+shift(abs(coh(where(real_part(coh[1:*]) gt 0.7))),2)),/current,color=col[i],layout=[4,4,i])

;p2=plot(freq[1:*],coh(where(real_part(coh[1:*] gt 0.7))),/current,color=col[i],layout=[3,3,i])
;pause

;twas cut at 40

IF n_elements(ph_list) eq 0 THEN ph_list=phase ELSE ph_list=[[temporary(ph_list)],[phase]]

ENDFOR 
;FOR i=1,n_elements(freq)-2,2 DO BEGIN
;IF i eq 1 THEN p=plot(findgen(9),mean(ph_list[i:i+2,*],dimension=1),/current,layout=[7,7,1],font_size=7) ELSE $
;p=plot(findgen(9),mean(ph_list[i:i+2,*],dimension=1),/current,layout=[7,7,((i-3)/2.0)+2],font_size=7)
;ENDFOR
;stop

END