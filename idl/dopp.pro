FUNCTION dopp,data,nt,nwst,nwen

sz=size(data)

dopplergram=make_array(sz(1),sz(2),nt,/integer,value=0)


FOR i=0, (nt-1) DO BEGIN

dopplergram[*,*,i]=data[*,*,nwst,i]-data[*,*,nwen,i]

ENDFOR

RETURN, dopplergram

END