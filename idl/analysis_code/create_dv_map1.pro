
;inpath - should be of the form /path/to/file/cripsex.wav.hr.min.sec
PRO create_dv_map1

inpath='IDL_data_files/ca/'
inpath2='/home/morton/SST/2014/'
nx=781
ny=761
nw=15
nt=81

;openr,lun,inpath+'stokesIicube',/get_lun
;dat=assoc(lun,intarr(nx,ny,nw,/nozer),512)
restore,inpath+'roi_wstoke.sav'
restore,inpath+'wav.idl'
dat=scope_varfetch('roi_stok')
;vel_map=fltarr(nx,ny)

;dum=intarr(nx,ny,nw,15)
;for i=0,14 do dum[*,*,*,i]=dat[*,*,*,i]

;split_for,0,4,commands=[$
;        'in=1.*reform(dum[*,*,*,i])',$
;        'fm_vel,in,wav,lambda=6563.,method=4,vel=a',$
;        'IF n_elements(gg) EQ 0 THEN gg=a ELSE gg=[[[gg]],[[a]]]'],$  
;        varnames=['wav','dum'], $
;        outvar='gg',$
;        nsplit=4                             

FOR i=0,nt-1 DO BEGIN
        print,i
	in=reform(dat[*,*,*,i])*1.
	fm_vel_spt_width,in,wav,lambda=8542.,method=7,vel=a,intc=b,wid=c,split=split
	;IF n_elements(vel_map) EQ 0 THEN vel_map=a ELSE vel_map=[[[temporary(vel_map)]],[[a]]]
        ;IF n_elements(prof_min) EQ 0 THEN prof_min=b ELSE prof_min=[[[temporary(prof_min)]],[[b]]]
	IF n_elements(dopp_w) EQ 0 THEN dopp_w=c ELSE dopp_w=[[[temporary(dopp_w)]],[[c]]]
	save,dopp_w,filename=inpath+'dpw.idl'
        
	;in1=reform(dat[0:400,0:400,*,i])*1.
        ;fm_vel_spt_width,in1,wav,lambda=8542.,method=7,vel=a,intc=b,wid=d,split=split
        ;IF n_elements(dopp_w1) EQ 0 THEN dopp_w1=d ELSE dopp_w1=[[[temporary(dopp_w1)]],[[d]]]

	;save,dopp_w1,filename=inpath+'dpw_040040.idl'
        
	;in2=reform(dat[400:*,0:400,*,i])*1.
        ;fm_vel_spt_width,in2,wav,lambda=6563.,method=7,vel=a,intc=b,wid=f,split=split
        ;IF n_elements(dopp_w2) EQ 0 THEN dopp_w2=f ELSE dopp_w2=[[[temporary(dopp_w2)]],[[f]]]
	;save,dopp_w2,filename=inpath+'dpw_40040.idl'

        ;in3=reform(dat[400:*,400:*,*,i])*1.
        ;fm_vel_spt_width,in3,wav,lambda=6563.,method=7,vel=a,intc=b,wid=g,split=split
        ;IF n_elements(dopp_w3) EQ 0 THEN dopp_w3=g ELSE dopp_w3=[[[temporary(dopp_w3)]],[[g]]]
	;save,dopp_w3,filename=inpath+'dpw_4040.idl'


ENDFOR



END
