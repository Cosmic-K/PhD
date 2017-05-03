pro get_ts,files,amp,amp_er,pts,ptser,p,p_er

amp=fltarr(n_elements(files)-1)
amp_er=fltarr(n_elements(files)-1)
p=fltarr(n_elements(files)-1)
p_er=fltarr(n_elements(files)-1)
pts=fltarr(228,n_elements(files)-1)
ptser=fltarr(228,n_elements(files)-1)

FOR i=1, n_elements(files)-1 DO BEGIN

restore,files[i]

pos=threads_fit_fg.pos
pos_er=threads_fit_fg.err_pos

pos=pos[*,1:*]
pos_er=pos_er[*,1:*]
res=threads_fit_fg.FIT_RESULT_POS
res=res[*,1:*]

s=size(pos)
IF s(2) gt 1 THEN BEGIN
IF total(res[*,0]) EQ 0 THEN BEGIN 
pos=pos[*,1]
pos_er=pos_er[*,1]
res=res[*,1]
ENDIF ELSE BEGIN
pos=pos[*,0]
pos_er=pos_er[*,0]
res=res[*,0]
ENDELSE
ENDIF 

pts[*,i-1]=pos
ptser[*,i-1]=pos_er


IF n_elements(res) EQ 18 THEN BEGIN
dof=(res(12)-res(11))-5
chi=res(10)
redchi=float(chi)/float(dof)
ENDIF

IF n_elements(res) EQ 19 THEN BEGIN
DOF=res(11)
chi=res(10)
redchi=float(chi)/float(dof)
ENDIF
amp[i-1]=res(1)
amp_er[i-1]=(sqrt((res(6)^2)*redchi))
p[i-1]=res(2)
p_er[i-1]=(sqrt((res(7)^2)*redchi))

ENDFOR
END