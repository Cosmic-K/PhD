;+
;Aurthor: Krishna Mooroogen 
;Contact:kpmooroogen@gmail.com 
;Institution: Northumbria University 
;Date: 13/06/2016
:Description:
;
;Function to subtract time in hours/mins/seconds/millie seconds
;Format must be of hh:mm:ss.ms
;Either enter two time or enter array of times the the function will find the difference
;function 2 calculate time diff from first value in seconds
;FRMFST KEYWORD ONLY USED IN T1 USE
;
;Input: Times or time array 
;Output: time in seconds between segments or time from first time element if frmfst is used
;-

function t_sub,t1,t2=t2,frmfst=frmfst,silent=silent

IF N_ELEMENTS(t2) NE 0 THEN BEGIN 

	a=strsplit(t1,/extract,':')
	b=strsplit(t2,/extract,':')

	td=float(b)-float(a)

tdiff=(td(0))*3600-abs(td(1))*60-abs(td(2))

IF NOT KEYWORD_SET(silent) THEN BEGIN
PRINT, tdiff,' seconds'

PRINT,fix(tdiff)/3600,':',fix(tdiff)/60,':',fix(tdiff) mod 60,'.',tdiff-fix(tdiff)
ENDIF
ENDIF

IF N_ELEMENTS(t2) EQ 0 AND KEYWORD_SET(frmfst) THEN BEGIN

n=N_ELEMENTS(t1)

a=strsplit(t1,/extract,':')
a=a.ToArray(DIMENSION=1,type='float')
a=reform(a,1,3,n)
b=rebin(a[*,*,0],1,3,n)

td=a[*,*,*]-b

;WHY IS THIS ONLY 1 ONE ARRAY
h=abs((td(0,0,*)))*3600
m=abs(td(0,1,*))*60
s=abs(td(0,2,*))

n_size=N_ELEMENTS(h)
tdiff=fltarr(n_size)

FOR i=0,n_size-1 DO BEGIN
IF (h[i] eq 0) and (m[i] eq 0) THEN tdiff[i] = s[i]
IF (h[i] eq 0) and (m[i] ne 0) THEN tdiff[i] = m[i]-s[i]
IF (h[i] ne 0) and (m[i] ne 0) THEN tdiff[i] = h[i]-m[i]-s[i]
IF (h[i] ne 0) and (m[i] eq 0) THEN tdiff[i] = h[i]-s[i]
ENDFOR 

ENDIF

IF NOT KEYWORD_SET(frmfst) AND N_ELEMENTS(t2) EQ 0 THEN BEGIN

n=N_ELEMENTS(t1)

a=strsplit(t1,/extract,':')
a=a.ToArray(DIMENSION=1,type='float')
a=reform(a,1,3,n)
b=shift(a,3)
td=a[*,*,1:*]-b[*,*,1:*]
h=reform(abs((td(0,0,*)))*3600)
m=reform(abs(td(0,1,*))*60)
s=reform(abs(td(0,2,*)))

n_size=N_ELEMENTS(h)
tdiff=fltarr(n_size)
FOR i=0,n_size-1 DO BEGIN
IF (h[i] eq 0) and (m[i] eq 0) THEN tdiff[i] = s[i]
IF (h[i] eq 0) and (m[i] ne 0) THEN tdiff[i] = m[i]-s[i]
IF (h[i] ne 0) and (m[i] ne 0) THEN tdiff[i] = h[i]-m[i]-s[i]
IF (h[i] ne 0) and (m[i] eq 0) THEN tdiff[i] = h[i]-s[i]

ENDFOR 

ENDIF

return, tdiff

END