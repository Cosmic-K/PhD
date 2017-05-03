;Krishna Mooroogen
;Northumbira University 
;krishna.mooroogen@northumbria.ac.uk
;proccedure to plot all results from nuwt using the common block and td 
;modifications will be needed for cubes of td's
;INPUTS
;file: file string of the saved data with stored common block 
;data: time distance diagram accosiated with output file
;currenly plots will be saved in the current folder, keywords are available to change that. 

PRO NUWT_plotall,data,tag,cadence=cadence,resol=resol,fftresults=fftresults
;data
s=size(data)
data2=data;[0:ceil(s(1)/2.),*]
alner=0.471906 ;change per data

a=3.15311e-08  
b=-0.000308478
c=3.

;a=8.2064e-7
;b=-0.00055
;c=0.087

er=sqrt(exp((a*data2^2)+(b*data2)+c)^2+(alner)^2)

RUN_NUWT_WITH_FFT,-1*data2,errors=er,/bootstrap,/gauss,/save_results,filetag='rad_'+tag,save_folder='./',pad_length=10

IF n_elements(cadence) eq 0 THEN cadence = 30.
IF n_elements(resol)   eq 0 THEN resol   = 0.0592

;restore_nuwt_common_data, file
;PLOT_NUWT_PEAKS_AND_THREADS,data2,res=resol,cad=cadence;,slitnum=70
;PLOT_NUWT_FFT_STATS,res=resol,cad=cadence;,slitnum=70

IF keyword_set(fftresults) THEN PLOT_NUWT_FFT_RESULTS,res=resol,cad=cadence;,slitnum=70 ; takes a long time 

;plot_nuwt_compare_runs, res=###, cad=###, /nuwt, ref_threads=REF_THREADS_ARRAY, ref_waves=REF_WAVES_ARRAY for debugging
quiforap,'nuwt_results_all_slits_rad_'+tag+'.sav',all_amps,all_periods,/single
save,all_amps,filename='amps.idl'
save,all_periods,filename='per.idl'
;quiforth,'nuwt_results_all_slits_rad_'+tag+'.sav',slit_located
;save,slit_located,filename='slitloc.idl'
;quifoft,'nuwt_results_all_slits_rad_'+tag+'.sav',slit_threads,slit_fft_results
;save,slit_threads,filename='threads.idl'
;save,slit_fft_results,filename='fft_res.idl'


END