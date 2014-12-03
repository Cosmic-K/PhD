FUNCTION dopp,data,nt,nwst,nwen

sz=size(data)

dopplergram=make_arr(sz(1),sz(2),nt,/int,value=0)


FOR i=0, (nt-1) DO BEGIN

dopplergram[*,*,i]=data[*,*,nwst,i]-data[*,*,nwen,i]

ENDFOR

RETURN, dopplergram

END