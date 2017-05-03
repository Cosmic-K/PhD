;+
;NAME: FIX_GEN_KINK_WAVE_COORDS
;
;PURPOSE:
;   Partially fixes an error in certain older runs (before 30 Sept, 2016) of
;   'generate_kink_waves.pro' which resulted in the wrong start_dist and start_time
;   being recorded for the wave. The actual wave parameters and td diagram where
;   NOT affected.
;
;INPUTS:
;   maxint - maximum intensity for in DN for loop
;   width - standard deviation from Gaussian fit to loop cross-section
;   amp - amplitude of wave in pixels
;   period - period of wave in seconds
;   phase - phase of the wavefindge
;   res - resolution in arcseconds
;   cad - cadence in seconds
;   expo - exposure in seconds
;   stddev_noise - sigma value for noise (noise is randomly picked from normally distributed values)
;   mean_noise - mean value of noise
;   seed - sets a seed value normally distributed random numbers
;   alignerr - set sigma of alignment error (alignment error is randomly picked from normally distributed values)
;
;OPTIONAL INPUTS:
;   clean - set keyword to set noise to a low level
;   plot - set keyword to plot the generated array to screen
;   background - set keyword to allow prompt and setting of background function
;
;OUTPUTS:
;   karr - provides 2d array of kink wave
;   middle - returns central position of gaussian (20+middle)
;
;TO DO:
;   - None (hopefully ever)
;
;;HISTORY: Name---------Date---------Description
;         M Weberg  Sept, 2016 - Initial coding
;-

;#####################################################
;Generate a synthetic wave with a gaussian profile whose centeral position varies
;over time with a sinusoidal motion as well as a linear drift
;#####################################################
;"Complete" generation of a simulated wave (THIS IS THE FUNCTION USED BELOW)
FUNCTION fast_gen_kink, amp, period, phase, nx, nt, sim_dx, sim_dt, peak_int, width, slope, $
                        start_x_loc=start_x_loc, end_x_loc=end_x_loc
    kink = fltarr(nx,nt)
    x_vals = findgen(nx)*sim_dx ; distance in [arcsec]
    x_vals = rebin(x_vals, nx, nt) ; replicate over time
    t_vals = findgen(nt)*sim_dt ; time in [seconds]

    edge_offset = amp+3*width
    IF slope LT 0 THEN edge_offset = x_vals[-1] - edge_offset
    IF slope EQ 0 THEN edge_offset = x_vals[-1]/2.0
    center_line = slope*t_vals + edge_offset
    center_line = rebin(reform(center_line, 1, nt), nx, nt) ; replicate over space

    wave_locs = amp*cos(2.0*!PI*(t_vals)/period+phase) ; location in [arcsec]
    start_x_loc = wave_locs[0] ; Used for tracking wave locations
    end_x_loc = wave_locs[-1]
    wave_locs = rebin(reform(wave_locs, 1, nt), nx, nt) ; replicate over space

    kink = peak_int*EXP(-((x_vals-(wave_locs+center_line))/width)^2/2.0) ;should be a 2D array

    RETURN, kink
END

;#####################################################
;############### START OF MAIN PROGRAM ###############
;#####################################################
PRO FIX_GEN_KINK_WAVE_COORDS, gauss_width=gauss_width, res=res, cad=cad, expo=expo, quiet=quiet, $
                              ratio_x=ratio_x, ratio_t=ratio_t, sim_dx=sim_dx, sim_dt=sim_dt

;load Common block data with the bad values
COMMON sim_dat, sim_td, sim_waves

;#####################################################
;SETTING DEFAULT VALUES
;#####################################################
IF n_elements(gauss_width) EQ 0 THEN gauss_width = 0.6
IF n_elements(res) EQ 0 THEN res = 0.6 ; [arcsec] AIA 171 plate scale
IF n_elements(cad) EQ 0 THEN cad = 12.0 ; [sec] Typical AIA 171 cadence
IF n_elements(expo) EQ 0 THEN expo = 2.7 ; [sec] Typical AIA 171 exposure time

IF NOT KEYWORD_SET(ratio_x) THEN ratio_x = 3
IF NOT KEYWORD_SET(ratio_t) THEN ratio_t = 3
IF NOT KEYWORD_SET(sim_dx) THEN sim_dx = res/ratio_x ; defaults result in sim_dx = 0.2 [arcsec]
IF NOT KEYWORD_SET(sim_dt) THEN sim_dt = expo/ratio_t ; defaults result in sim_dt = 0.9 [sec]
print, 'sim_dx, sim_dt =', sim_dx, sim_dt
km_per_arcsec = 725.27

;#####################################################
;CHECKING / CALCULATING ARRAY SIZES
;#####################################################
num_waves = n_elements(sim_waves.amp)
td_sz = size(sim_td)
output_nx = td_sz[1]
output_nt = td_sz[2]

max_dist = output_nx*res
end_time = output_nt*cad
full_nx = FIX(round(max_dist/sim_dx))
full_nt = FIX(round(end_time/sim_dt))

;Attempting to figure out the original hi-res box coord_transform
full_sim = {box_coords:intarr(4, num_waves)}
full_sim.box_coords[0,*] = FIX(ceil(sim_waves.box_coords[0,*]/(sim_dx/res)))
full_sim.box_coords[1,*] = FIX(ceil(sim_waves.box_coords[1,*]/(sim_dt/cad)))
full_sim.box_coords[2,*] = FIX(ceil(sim_waves.box_coords[2,*]/(sim_dx/res)))
full_sim.box_coords[3,*] = FIX(ceil(sim_waves.box_coords[3,*]/(sim_dt/cad)))

;#####################################################
;TRANSFER DATA TO UPDATED OUTPUT STRUCTURE
;#####################################################
IF NOT KEYWORD_SET(quiet) THEN print, string([13B, 10B])+'Copying wave data to new structure...'
old_waves = sim_waves

sim_waves = {amp:fltarr(num_waves), period:fltarr(num_waves), phase:fltarr(num_waves), $
             slope:fltarr(num_waves), peak_int:fltarr(num_waves), $
             obs_cycles:fltarr(num_waves), box_coords:intarr(4, num_waves), $
             start_dist:fltarr(num_waves), start_time:fltarr(num_waves), $
             end_dist:fltarr(num_waves), end_time:fltarr(num_waves)}

sim_waves.amp = old_waves.amp
sim_waves.period = old_waves.period
sim_waves.phase = old_waves.phase
sim_waves.slope = old_waves.slope
sim_waves.peak_int = old_waves.peak_int
sim_waves.obs_cycles = old_waves.obs_cycles
sim_waves.box_coords = old_waves.box_coords

;#####################################################
;FIXING START_DIST AND START_TIME
;#####################################################
IF NOT KEYWORD_SET(quiet) THEN print, string([13B, 10B])+'Fixing coords...'

hi_res = fltarr(full_nx, full_nt)
FOR i=0L, (num_waves-1) DO BEGIN
    x_coord = full_sim.box_coords[0,i]
    t_coord = full_sim.box_coords[1,i]
    box_width = full_sim.box_coords[2,i] - x_coord + 1
    box_length = full_sim.box_coords[3,i] - t_coord + 1
    hi_res[x_coord, t_coord] = fast_gen_kink(sim_waves.amp[i], sim_waves.period[i], sim_waves.phase[i], $
                                             box_width, box_length, sim_dx, sim_dt, $
                                             sim_waves.peak_int[i], gauss_width, sim_waves.slope[i], $
                                             start_x_loc=first_loc_in_box, end_x_loc=last_loc_in_box)
    sim_waves.start_dist[i] = x_coord*sim_dx + first_loc_in_box ; [arcsec]
    sim_waves.end_dist[i] = x_coord*sim_dx + last_loc_in_box ; [arcsec]
    sim_waves.start_time[i] = t_coord*sim_dt ; [seconds]
    sim_waves.end_time[i] = (t_coord + box_length)*sim_dt ; [seconds]
ENDFOR

END
