PRO amp_ts, ts,amp
sz=size(ts)

amp=fltarr(1,sz(2))

FOR i=0, sz(2) -1 DO BEGIN

amp[*,i]=rms(abs(ts[*,i]-mean(ts[*,i]))*sqrt(2.)

ENDFOR

END