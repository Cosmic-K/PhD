PRO diog_show2,int,v,dp,core=core,wing=wing,veloc=veloc,dpwid=dpwid,average=average,props
km = (0.725/16.981891892)*1000.
 ;make this more general
 sz=size(dp)
 x1=0
 x2=400
 y1=0
 y2=400

 intt=int[100:880,200:960,*,*]
 v1=v[100:880,200:960,*]
 core=reform(intt[x1:x2,y1:y2,7,*])
 wing=reform(intt[x1:x2,y1:y2,0,*])
 veloc=v1[x1:x2,y1:y2,*]
 dpwid=dp<0.8

props={axis_style:2,dimensions:[700,700],margin:[0.2,0.01,0.01,0.01]}

IF keyword_set(average) THEN BEGIN
myimage1 = image(total(core,3),layout=[2,2,1],_EXTRA=props,/current,title='core')
myimage2 = image(total(wing,3),layout=[2,2,2],_EXTRA=props,/current,title='wing')
myimage3 = image(total(dpwid,3),layout=[2,2,3],_EXTRA=props,/current,title='width')
myimage4 = image(total(veloc[0:*,*,*],3)<350000,layout=[2,2,4],_EXTRA=props,/current,title='veloc',rgb_table=70)
ENDIF ELSE BEGIN
while n_elements(t) eq 0 do begin
READ,t,PROMPT='Where in time do you want to look?, there are '+strtrim(sz(3),1)+'frames'
;can make this more sophisticated with better error handling 
endwhile
myimage1 = image(core(*,*,t),layout=[2,2,1],_EXTRA=props,/current,title='core')
myimage2 = image(wing(*,*,t),layout=[2,2,2],_EXTRA=props,/current,title='wing')
myimage3 = image(dpwid(*,*,t),layout=[2,2,3],_EXTRA=props,/current,title='width')
myimage4 = image(veloc(*,*,t),layout=[2,2,4],_EXTRA=props,/current,title='veloc')

ENDELSE

END