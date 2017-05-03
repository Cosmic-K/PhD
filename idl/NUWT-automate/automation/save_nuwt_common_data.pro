;+
;NAME: SAVE_NUWT_COMMON_DATA
;
;PURPOSE:
;   Saving the master "all_nuwt_dat" COMMON block data to a '.sav' file
;
;INPUTS:
;   save_folder - folder in which to save the results. Defaults to the user's home folder.
;                 Will also append a '/' to the end if not included.
;   filename -
;   filetag - unique tag to append to the end of the output files
;
;OUTPUTS:
;   A '.sav' file containing the with the lists of structures currently stored
;   "all_nuwt_dat" COMMON block.
;
;HISTORY: Name---------Date---------Description
;         M Weberg  19 SEPT, 2016  Initial coding
;-

PRO SAVE_NUWT_COMMON_DATA, save_folder=save_folder, filename=filename, filetag=filetag

COMMON all_nuwt_dat, nuwt_located, nuwt_threads, nuwt_fft_results, nuwt_fft_stats

;###############################################
;SETTING DEFAULT VALUES
;###############################################
IF NOT KEYWORD_SET(filename) THEN filename = 'nuwt_results_all_slits'
IF NOT KEYWORD_SET(filetag) THEN filetag='' ELSE filetag = '_'+filetag
IF NOT KEYWORD_SET(save_folder) THEN save_folder = '' ;i.e. defaults to home folder
IF strlen(save_folder) GT 0 AND NOT save_folder.endswith('/') THEN save_folder = save_folder+'/'

save_filename = filename+filetag+'.sav'
print, 'Saving NUWT "all_nuwt_dat" COMMON block to file:'
print, '   Save folder: '+save_folder
print, '   filename: '+save_filename
SAVE, nuwt_located, nuwt_threads, nuwt_fft_results, nuwt_fft_stats, FILENAME=save_folder+save_filename

END
