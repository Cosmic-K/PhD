;+
;NAME: RESTORE_NUWT_COMMON_DATA
;
;PURPOSE:
;   Loading in results stored in a '.sav' file created by 'run_nuwt_with_fft.pro'
;   and placing it back in the 'all_nuwt_dat' master COMMON block
;
;INPUTS:
;   savfile - filename and path for the '.sav' file to be restored
;
;OPTIONAL OUTPUTS:
;   located_out - list containing the peaks located by NUWT. Each element in the
;                 list contains the stucture for a single dataslit.
;   threads_out - list containing the threads strung together. Each element
;                 in the list contains an array for the given slit. Each array
;                 contains multiple structures, one for each thread in the slit.
;   fft_results_out - list of the calculated fft spectra. Each element in the list
;                     is, itself, a list containing the results for a single slit.
;                     each sub_structure contains the spectra for a slingle thread.
;   fft_stats_out - list of the various calculated and selected values. Similar to
;                   "threads_out", each element is an array of structures. One
;                   array each slit & one structure for each thread in the slit

;
;HISTORY: Name---------Date---------Description
;         M Weberg  14 SEPT, 2016  Initial coding
;-

PRO RESTORE_NUWT_COMMON_DATA, savfile, $
                              located_out=located_out, threads_out=threads_out, $
                              fft_results_out=fft_results_out, fft_stats_out=fft_stats_out
COMMON all_nuwt_dat, nuwt_located, nuwt_threads, nuwt_fft_results, nuwt_fft_stats

RESTORE, savfile
print, 'NUWT results restored to the "all_nuwt_dat" COMMON block'

located_out = nuwt_located
threads_out = nuwt_threads
fft_results_out = nuwt_fft_results
fft_stats_out = nuwt_fft_stats

END
