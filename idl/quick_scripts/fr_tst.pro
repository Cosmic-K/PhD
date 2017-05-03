pro fr_tst,n
x=findgen(40)
For i=0, 39, 5 do begin
For j=i, i+4 do begin
print, x(j)
endfor
endfor
end