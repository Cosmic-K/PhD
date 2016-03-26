PRO PROCESS_TIMER, pass, totalpasses, t0

	timer_t = SYSTIME(/SECONDS)
	accumsectime = (timer_t-t0)
	totalsectime = (timer_t-t0)/FLOAT(pass)*FLOAT(totalpasses)
	ndig = FLOOR(ALOG10(totalpasses))+1
	IF (totalsectime GT 60) THEN BEGIN
		IF (totalsectime GT 3600) THEN BEGIN
			IF (totalsectime GT 86400) THEN BEGIN
				label = " days."
				fac = 86400.
			ENDIF ELSE BEGIN
				label = " hours."
				fac = 3600.
			ENDELSE
		ENDIF ELSE BEGIN
			label = " minutes."
			fac = 60.
		ENDELSE				
	ENDIF ELSE BEGIN
		label = " seconds."
		fac = 1.
	ENDELSE
	WRITEU, -1, STRING(FORMAT='(%"\rPass ",i'+STRTRIM(ndig,2)+',"/",i'+STRTRIM(ndig,2)+'," or ",f7.3,"%. Running",f6.2,"/",f6.2,a9)',$
		STRTRIM(pass,2),STRTRIM(LONG(totalpasses),2),STRTRIM(100.*pass/FLOAT(totalpasses),2),STRTRIM((accumsectime/fac),2),STRTRIM((totalsectime/fac),2),label)
	
END
