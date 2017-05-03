PRO mk_icube, filename

nx= 982
ny= 994
nt= 4740
nlines= 15
nw=nt/4.

stokesI = intarr( nx, ny, nw )
h=0
FOR x=long(0),nt-1,60 DO BEGIN
FOR j=x,x+14 do begin
stokesI[*,*,h] = lp_get(filexx  name, j)
h+=1
ENDFOR
ENDFOR

extraheader=strarr(nlines)

for l=0,nlines-1. do begin
extraheader[l]=string(l)
endfor
help,stokesI
lp_write,stokesI,'stokesI.icube',extraheader=extraheader

mk_sp_from_im_cube, 'stokesI.icube',nlp=nlines


END