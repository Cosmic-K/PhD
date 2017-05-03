PRO diog_show,files,core,wing,bcog,veloc,dpwid=dpwid,average=average,props
km = (0.725/16.981891892)*1000.
 ;make this more general
restore, files[where(strmatch(files,'w_*',/fold_case) EQ 1)]
restore, files[where(strmatch(files,'b_*',/fold_case) EQ 1)]
restore, files[where(strmatch(files,'c_*',/fold_case) EQ 1)]
restore, files[where(strmatch(files,'v_*',/fold_case) EQ 1)]
restore, files[where(strmatch(files,'dpw_*',/fold_case) EQ 1)]

;core=scope_varfetch(strtrim(strsplit(files[where(strmatch(files,'c*',/fold_case) EQ 1)],'.idl',/extract),2))
;wing=scope_varfetch(strtrim(strsplit(files[where(strmatch(files,'w*',/fold_case) EQ 1)],'.idl',/extract),2))
;bcog=scope_varfetch(strtrim(strsplit(files[where(strmatch(files,'b*',/fold_case) EQ 1)],'.idl',/extract),2))
;dop=scope_varfetch(strtrim(strsplit(files[where(strmatch(files,'v*',/fold_case) EQ 1)],'.idl',/extract),2))

core=scope_varfetch('c')
wing=scope_varfetch('w')
bcog=scope_varfetch('b')
veloc=scope_varfetch('v')
dpwid=scope_varfetch('dopp_w4')<1.2 ; this will need to chnage because of my stupid formatting 
dpwid=dpwid[*,*,0:227]

props={axis_style:2,dimensions:[700,700],margin:[0.2,0.01,0.01,0.01]}

IF keyword_set(average) THEN BEGIN
myimage1 = image(total(core(*,*,*),3),layout=[3,2,1],_EXTRA=props,/current)
myimage2 = image(total(wing(*,*,*),3),layout=[3,2,2],_EXTRA=props,/current)
myimage3 = image(total(dpwid(*,*,*),3),layout=[3,2,3],_EXTRA=props,/current)
ENDIF ELSE BEGIN
myimage1 = image(core(*,*,0),layout=[3,2,1],_EXTRA=props,/current)
myimage2 = image(wing(*,*,0),layout=[3,2,2],_EXTRA=props,/current)
myimage3 = image(dpwid(*,*,0),layout=[3,2,3],_EXTRA=props,/current)
ENDELSE

myimage4 = image(veloc(*,*),layout=[3,2,4],_EXTRA=props,/current)
myimage5 = image(gauss_smooth(bcog(*,*),2,/edge_truncate)<30.>(-30.),layout=[3,2,5],_EXTRA=props,/current)


;IF dpwid NE 0 THEN print,'Retuned variables: core, wing, bcog, veloc, dpwid and props for graphics.' ELSE $ 
;print,'Retuned variables: core, wing, bcog, veloc and props for graphics.'

END