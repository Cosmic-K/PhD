;+
;NAME: POLAR SLIT
;
;PURPOSE:
;   Extracts a time-distance slice directly above the solar north (or south) 
;   pole in a series of images. Can operate in an interactive mode. Basic
;   code framework was adapted from diag_slit.pro
;
;INPUTS: 
;   in_data - 2D or 3D array containing image data
;
;OPTIONAL INPUTS:
;   y - central y-coordinate (in pixels) of the virtual slit
;   length - horizontal length of virtual slit (will be centered in the image)
;   width - number of vertical pixels to average over (improves signal/noise)
;   fn - frame number to plot 
;   xsize, ysize - size of graphics window (command for widgets)
;   win_id - set to value of open graphics window you want to use. If not set,
;            opens next free window.
;   /noopen - if set, suppresses the opening of plot windows
;
;OUTPUTS: 
;   out_data - output array where data along diagonal line is stored
;   outvec - array of [x1, x2, y, y] coordinates used for slit
;
;HISTORY: Name---------Date---------Description
;         M Weberg  8 JUNE, 2016  Initial coding
;
;TO DO: 
;   None at the current time.
;-

PRO POLAR_SLIT, in_data, out_data, y=y, length=length, width=width, fn=fn, outvec=outvec,$
                xsize=xsize, ysize=ysize, win_id=win_id, noopen=noopen

;Check dimensions of input data
sz = size(in_data)
IF sz[0] EQ 3 THEN BEGIN
    nx_data = sz[1]
    ny_data = sz[2]
    nt_data = sz[3]
ENDIF ELSE BEGIN
    IF sz[0] EQ 2 THEN BEGIN
        print, 'WARNING: input data is only 2D! Output array will be 1D.'
        nx_data = sz[1]
        ny_data = sz[2]
        nt_data = 1
        fn = 0
    ENDIF ELSE BEGIN
        MESSAGE, 'ERROR: please input a valid 2D or 3D array'
    ENDELSE
ENDELSE

IF NOT keyword_set(noopen) THEN BEGIN
    IF n_elements(fn) eq 0 THEN fn=0
    IF fn GT nt_data-1 THEN fn = nt_data-1
    ;Set plotting elements
    ;writing to buffer pixmap reduces on-screen jitter!
    IF n_elements(win_id) EQ 0 THEN BEGIN
        determine_window_size, xsize=xsize, ysize=ysize, openwin=openwin, /new_win 
        window, openwin[0], xs=xsize,ys=ysize
        window_var = openwin[0]
    ENDIF ELSE BEGIN 
        wset, win_id
        window_var = win_id
    ENDELSE
    window, xs=xsize, ys=ysize, /pixmap, /free
    pixID = !d.window
    tvim, in_data[*,*,fn]
    wset, window_var
    device, copy=[0,0,!d.x_size,!d.y_size,0,0,pixid]
ENDIF

;Ask for length, width and y-coord if not set
IF NOT keyword_set(length) THEN BEGIN
    READ, length, PROMPT='Please enter slit length: '
ENDIF

IF NOT keyword_set(width) THEN BEGIN
    READ, width, PROMPT='Please enter slit width (to average over): '
ENDIF

IF n_elements(y) EQ 0 THEN BEGIN
    IF NOT keyword_set(noopen) THEN BEGIN
        PRINT,'Select center y-coord with cursor (note: x-coord is ignored)'
        CURSOR, x, y, /Data,/Down
        print, 'y = '+strtrim(string(fix(round(y))),1)
    ENDIF ELSE BEGIN
        READ, y, PROMPT='Please enter y-coord: '
    ENDELSE
ENDIF

length = fix(round(length))
width = fix(round(width))
y = fix(round(y))

;Validating slit values and resetting if needed (prevents unexpected behavior)
IF length LT 1 THEN length = 1
IF length GT nx_data THEN length = nx_data
IF width LT 1 THEN width = 1
IF width GT ny_data THEN width = ny_data
IF y LT 0 THEN y = 0
IF y - floor(width/2.0) LT 0 THEN y = fix(floor(width/2.0))

;Figure out coordinates for the slit
x_margin = fix(floor((nx_data - length)/2.0))
x1 = x_margin
x2 = x1 + length - 1 ;the extra -1 is because IDL includes data at the end index

ybot = y - fix(floor(width/2.0))
ytop = ybot + width - 1 ;again, extra -1 to account for IDL indexing

;Plot the selected slit on the image
IF NOT keyword_set(noopen) THEN plots, [x1,x2], [y,y]

;Slicing the input data cube and then averaging over the width (if needed)
out_data = fltarr(length, nt_data)
slit = in_data[x1:x2, ybot:ytop, *]
IF width GT 1 THEN out_data[*,*] = mean(slit, dimension=2) ELSE out_data[*,*] = reform(slit)

;Outputting slit coordinates (might be used by other scripts?)
outvec = [x1,x2,y,y]

END


