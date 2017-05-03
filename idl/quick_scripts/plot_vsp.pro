pro plot_vsp,x


FOR i=1, 28 DO BEGIN 

amp=read_table('/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/amp_height/amp'+strtrim(i,1)+'.txt')
a=amp[0,*]
a_er=amp[1,*]

va=read_table('/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/va_height/va'+strtrim(i,1)+'.txt')
v=va[0,*]
v_er=va[1,*]

per=read_table('/Users/krishnamooroogen/Documents/PHYSICS/PhD/Images/p_height/p'+strtrim(i,1)+'.txt')
p=per[0,*]
p_er=per[1,*]

p1=ERRORPLOT(p,a,p_er,a_er,axis_style=2,xtitle='Period (s)',ytitle='Amplitude (km)')
p2=ERRORPLOT(p,v,p_er,v_er,axis_style=2,xtitle='Period (s)',ytitle='Velocity Amplitude (km/s)')
	

ENDFOR
END

