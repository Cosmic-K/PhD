function logno,x,param

a=param(0)
m=param(1)
s=param(2)

y=(a/float(s*(sqrt(2*!pi))*x))*exp(-1.0*(((alog(x>1)-m)^2)/(2.0*(s^2))))

return,y
end