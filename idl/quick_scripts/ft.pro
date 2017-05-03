;FFT code for FFT of multiple time series 
;INPUTS 
;f: result for file search, search for files matching the location index 
;result will include text files will filter these out automatically
PRO ft,f,f2=f2,ph_list=ph_list
files=f
;files=f[where(strmatch(f,'*.idl') eq 1)]
;intial restore and cropping to get rid of -1 values 
restore,files[0]
threads=scope_varfetch('THREADS_FIT_FG')
var=threads_fit_fg.FIT_RESULT_POS
var=var[*,1]
pos=threads.pos
pos_er=threads.err_pos
pos1=pos[*,1]
pos_er1=pos_er[*,1]

inx=where(pos1 ne -1)
pos1=pos1[inx]
pos_er1=pos_er1[inx]

;pos1=pos1[21:170]
;pos_er1=pos_er1[21:170]
;pos1=f[*,0]
;pos_er1=f2[*,0]

aln_er=make_array(n_elements(pos1),value=0.164214)
pos_er1=sqrt(pos_er1^2 + aln_er^2)
n1=n_elements(pos1[inx])
N=n_elements(pos1)
x=FINDGEN((N - 1)/2.) + 1
dt=1.343
ti=findgen(n)*dt


;making freq full freq now only need half for ft plotting
IF evod(n) EQ 0 THEN freq = [0.0, X, N/2,-N/2 + X]/(N*dt) ELSE freq = [0.0, X,-(N/2 + 1) + X]/(N*dt)


;bootstrap 1 for first signal 
pos_matrix=rebin(pos1-mean(pos1),n,500)
noise_mat1=randomn(seed,n,500)*rebin(sqrt((pos_er1^2.)/n),n,500)

pos_matrix_plsnoise=pos_matrix+noise_mat1


signal1=fft(pos_matrix_plsnoise,dimension=1)

;signal1=mean(ftt,dimension=2)

;signal1=fft(pos1-mean(pos1),dimension=1)
;freq=rebin(freq, 50)
;tp=plot(freq[1:*],abs(signal1[1:*]),/ylog,/xlog,xstyle=2)
dst=fltarr(500)
probs=fltarr(500)
rt=fltarr(500)

func=mysin(findgen(n1),var)
fit=func;[8:157]

;FOR i=0, 499 do BEGIN

;A_one=signal1[1,i]
;A_two=signal1[2,i]
;A_three=signal1[3,i]
;A4=signal1[4,i]

func_mat=rebin(func-mean(func),n,500)
pos_matrix_plsnoise=func_mat+noise_mat1
;signal1=fft(pos_matrix_plsnoise,dimension=1)
;tp=plot(freq[1:*],abs(signal1[1:*]),/ylog,/xlog,xstyle=2)

p=plot(pos_matrix_plsnoise[*,0]+mean(func))
p1=plot(pos1,/over,/curr)
stop
;rebuild=fltarr(192,500)


;rebuild=(2*abs(A_one)*cos(2*!pi*ti*freq[1]+atan(A_one,/phase))+2*abs(A_two)*cos(2*!pi*ti*freq[2]+atan(A_two,/phase))$
;;+2*abs(A_three)*cos(2*!pi*ti*freq[3]+atan(A_three,/phase))+2*abs(A4)*cos(2*!pi*ti*freq[4]+atan(A4,/phase)))+mean(pos1)


col=['black','blue','green','orange','dark goldenrod','sky blue','pink','purple','rosy brown','indian red','aqua',$
'green yellow','sandy brown','firebrick','navy','plum','deep pink','aquamarine','salmon','powder blue','orchid','indigo','cornflower','peru','sea green']

;p=errorplot(findgen(n),(pos1),(pos_er1),color='black',errorbar_color='black',xstyle=3,thick=0.5,errorbar_thick=0.5,$
;	xtitle='seconds',ytitle='pixels')


;p2=plot(findgen(n),fit,/over,/curr,linestyle=2,color='blue')
;p1=plot(findgen(n),rebuild,color='red',xstyle=3,thick=0.5,linestyle=3,/over,/current)
;stop

;res_build=(pos1-rebuild)/pos_er1
;res_fit=(pos1-fit)/pos_er1

;ksone,res_fit,'mynorm', Ds, prob,/plot,/window,/_extra, xtitle='X',ytitle='Probability',xthick=2,ythick=2,charthick=1,charsize=1.5,color='red',thick=1

;print,'ds: ',strtrim(ds,1),' prob: ',strtrim(prob,1)

;dst[i]=ds
;probs[i]=prob

;res_pos=where(res_fit GT 0)
;res_neg=where(res_fit LT 0)

;bin=fltarr(228)
;bin[res_pos]=1
;bin[res_neg]=0


;pr=r_test(bin, R = r, N0 = n0, N1 = n1)
;Rt[i]=pr[1]
;print,pr[1],n0,n1,r
;ENDFOR

;r=plot(rt,title='run')
;d=plot(dst,title='dd')
;p=plot(probs,title='prob')
;stop






FOR i=1,8 DO BEGIN
;looping over rest of series using the cut off from the first 

restore,files[i]
threads=scope_varfetch('THREADS_FIT_FG')
pos=threads.pos
pos_er=threads.err_pos

pos=pos[*,1]
pos_er=pos_er[*,1]
pos=pos[21:170]
pos_er=pos_er[21:170]

;pos=pos[inx]
;pos_er=pos_er[inx] ; check same lenght

pos_er=sqrt(pos_er^2+aln_er^2)

;p=errorplot(findgen(n)*dt,pos+i*5.,pos_er,color=strtrim(col[i],1),errorbar_color=strtrim(col[i],1)$
;	,/current,/over,xstyle=3,thick=0.5,errorbar_thick=0.5,xtitle='seconds',ytitle='pixels')

;bootstrap 2 
;pos_matrix=rebin(pos-mean(pos),n,500)
;noise_mat=randomn(seed,n,500)*rebin(sqrt(pos_er^2./n),n,500)
;pos_matrix_plsnoise=pos_matrix+noise_mat
;ftt=fft(pos_matrix_plsnoise,dimension=1)
;signal=mean(ftt,dimension=2)
signal=fft(rebin(pos-mean(pos),50),dimension=1)
;phase and cohrence 

csrb=signal1*conj(signal)
;csrbb=csrb[where(csrb gt max(abs(csrb))/100000)]
;help,csrb
;print,csrb
phase = atan(csrb,/phase)
;phase = atan(imaginary(csrb),real_part(csrb))

csrbsm = smooth(signal1,5,/edge_truncate)*conj(smooth(signal,5,/edge_truncate))
csrr = smooth(signal1,5,/edge_truncate)*conj(smooth(signal1,5,/edge_truncate))
csbb = smooth(signal,5,/edge_truncate)*conj(smooth(signal,5,/edge_truncate))
coh = (csrbsm^2/(csrr*csbb))

;overplots
help,signal
help,freq
;p=plot(freq[1:*],abs(signal[1:*])/abs(signal1[1:*]),color=col[i],/overplot,linestyle=5,/current,xstyle=3,/ylog)
p=plot(freq[1:*],abs(signal[1:*]),color=col[i],/overplot,linestyle=5,/current,xstyle=3,/ylog,/xlog)
;p=plot(freq[1:*],1/3.*(signal+shift(abs(signal[1:*]),1)+shift(abs(signal[1:*]),2)),color=col[i],/overplot,linestyle=5,/current,xstyle=3,/ylog)

;p1=plot(freq[1:*],phase[1:*],color=col[i],/current,layout=[3,3,i])
;p1=plot(freq[1:*],1/3.*(phase[1:*]+shift(abs(phase[1:*]),1)+shift(abs(phase[1:*]),2)),color=col[i],/current,layout=[3,3,i])
;p2=plot(freq[1:*],1/3.*(coh(where(real_part(coh[1:*]) gt 0.7))+shift(abs(coh(where(real_part(coh[1:*]) gt 0.7))),1)+shift(abs(coh(where(real_part(coh[1:*]) gt 0.7))),2)),/current,color=col[i],layout=[3,3,i])

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