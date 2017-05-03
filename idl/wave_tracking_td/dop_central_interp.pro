;PURPOSE - interpolates the velocity value to 0.1 pixel
;

PRO dop_central_interp,dpdat,in_pos,out_val

    
    ;POTENTIAL ISSUES WITH LOCATE_THINGS_FG AS ALLOWS \PM 1.5 
    res_int=interpolate(dpdat(round(in_pos)-1:round(in_pos)+1),findgen(20)/10.)
    new_pos=round((in_pos-round(in_pos))*10.)+10.
        out_val=res_int[new_pos]

    ;deubgging
    ;x=findgen(20)/10.
    ;plot,dpdat(round(in_pos)-1:round(in_pos)+1)
    ;oplot,x,res_int,psym=1
    ;plots,[1,1],[min(dpdat),max(dpdat)]
    ;plots,[x(new_pos),x(new_pos)],[min(dpdat),max(dpdat)]
    ;print,out_val,dpdat(round(in_pos))

END