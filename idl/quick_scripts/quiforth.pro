pro quiforth,file,slit_located,slitnum=slitnum

RESTORE_NUWT_COMMON_DATA,file,located_out=located_out, threads_out=threads_out, fft_results_out=fft_results_out, fft_stats_out=fft_stats_out
COMMON all_nuwt_dat, nuwt_located, nuwt_threads, nuwt_fft_results, nuwt_fft_stats


IF n_elements(slitnum) EQ 0 THEN slitnum = 0

    IF slitnum GT n_elements(nuwt_located) THEN BEGIN
        last_slit = n_elements(nuwt_located)-1
        print, 'WARNING! slit number '+strtrim(slitnum, 2)+' does not exist!'
        print, '   slitnum set to '+strtrim(last_slit, 2)+' (last set of results)'
        slitnum = last_slit
    ENDIF

slit_located = nuwt_located[slitnum]

;all_pk_locs = where(slit_located.allpeaks[*,*] GT 0, /NULL)
;valid_pk_locs = where(slit_located.allpeaks[*,*]  GE 2, /NULL) ;includes both whole pixel (default) and sub-pixel (gaussin fit) peaks
;rejected_pk_locs = where(slit_located.allpeaks[*,*] EQ 1, /NULL)
;failed_gauss_locs = where(slit_located.allpeaks[*,*] EQ 2, /NULL)
;gauss_fit_locs = where(slit_located.allpeaks[*,*] EQ 3, /NULL)


;plt_pk = IMAGE(fltarr(nx,nt), axis_style=0, position=[0.125, 0.10, 0.995, 0.92], /current)
;valid_pk_image = fltarr(nx,nt,4) ;RGBA image (i.e. has an alpha channel)
;temp_peaks = fltarr(nx,nt)
; sub-pixel, gaussian fit peaks only
;temp_peaks[gauss_fit_locs] = 255
;valid_pk_image[*,*,0] = temp_peaks
;valid_pk_image[*,*,2] = temp_peaks
; both nearest pixel AND gaussian fit peaks
;temp_peaks[valid_pk_locs] = 255
;valid_pk_image[*,*,1] = temp_peaks
;valid_pk_image[*,*,3] = temp_peaks ;full opacity for ALL valid peaks
;plt_pk = IMAGE(valid_pk_image, axis_style=0, overplot=plt_pk, background_transparency=100)
;fail_gauss_fit_tag = text(0.05, 0.03, 'peaks defaulted to nearest whole pixel', color='green', font_size=10)

   


END
