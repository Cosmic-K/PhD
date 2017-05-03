PRO vel_rms,vel_core,rms_detrend_vel_core 

vel_core_0=vel_core[*,*,0:227]
vel_core_1=vel_core[*,*,227:455]
vel_core_2=vel_core[*,*,455:681]
vel_core_3=vel_core[*,*,681:908]
vel_core_4=vel_core[*,*,908:1116]

detrend_vel_core_0=fltarr(964,962,228)
detrend_vel_core_1=fltarr(964,962,228)
detrend_vel_core_2=fltarr(964,962,226)
detrend_vel_core_3=fltarr(964,962,227)
detrend_vel_core_4=fltarr(964,962,208)

det0=mean(vel_core[*,*,0:227],dimension=3)
det1=mean(vel_core[*,*,227:455],dimension=3)
det2=mean(vel_core[*,*,455:681],dimension=3)
det3=mean(vel_core[*,*,681:908],dimension=3)
det4=mean(vel_core[*,*,908:1116],dimension=3)

for i=0, 227 DO  detrend_vel_core_0[*,*,i]=vel_core_0[*,*,i]-det0  
for i=0, 227 DO  detrend_vel_core_1[*,*,i]=vel_core_1[*,*,i]-det1 
for i=0, 225 DO  detrend_vel_core_2[*,*,i]=vel_core_2[*,*,i]-det2 
for i=0, 226 DO  detrend_vel_core_3[*,*,i]=vel_core_3[*,*,i]-det3 
for i=0, 207 DO  detrend_vel_core_4[*,*,i]=vel_core_4[*,*,i]-det4 

rms_detrend_0=rms(detrend_vel_core_0,dimension=3)
rms_detrend_1=rms(detrend_vel_core_1,dimension=3)
rms_detrend_2=rms(detrend_vel_core_2,dimension=3)
rms_detrend_3=rms(detrend_vel_core_3,dimension=3)
rms_detrend_4=rms(detrend_vel_core_4,dimension=3) 

rms_detrend_vel_core=[[[rms_detrend_0]],[[rms_detrend_1]],[[rms_detrend_2]],[[rms_detrend_3]],[[rms_detrend_4]]]

END