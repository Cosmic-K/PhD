;Krishna Mooroogen
;Northumbria University
;krishna.mooroogen@northumbria.ac.uk
;PhD supervisor Richard Morton

;------------------------------------------------------
;PURPOSE
;Create reb-blue intensity difference mock doppler images form icube
;------------------------------------------------------
;NOTES
;------------------------------------------------------
;Not sure if this red-blue or blue-red
;Also note sure if should use (blue_wing_image-red_wing_image)*2 / (blue_wing_image+red_wing_image)

;------------------------------------------------------
;INPUTS
;DATA -- DATA ICUBE
;nt -- NUMBER OF TIME FRAMES
;nwst -- INDEX NUMBER OF STARTING WAVE FRAME
;nwen -- INDEX NUMBER OF ENDING WAVE FRAME
; i.e -5 to +5 from centre of spectral image


FUNCTION dopp,data,nt,nwst,nwen

;CONSTANTS
-------------------------------------------------------
sz=size(data)

;MAKE EMPTY DOPPLER ARRAY
;------------------------------------------------------
dopplergram=make_array(sz(1),sz(2),nt,/integer,value=0)

;FILL ARRAY WITH RED-BLUE INTENSITY DIFFERENCE
;------------------------------------------------------
FOR i=0, (nt-1) DO BEGIN

dopplergram[*,*,i]=data[*,*,nwst,i]-data[*,*,nwen,i]

ENDFOR
;------------------------------------------------------

RETURN, dopplergram

END