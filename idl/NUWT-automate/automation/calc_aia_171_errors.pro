;+
;NAME: CALC_AIA_171_ERRORS
;
;PURPOSE:
;   Calculate the intensity errors (i.e. noise) for the SDO / AIA 171
;   channel as determined by Yuan & Nakariakov, 2012 (A&A).
;
;INPUTS:
;   input_data - array of unaltered intensity values from AIA 171
;
;OPTIONAL INPUTS
;   smooth_num - number of timesteps used in smoothing the td diagram. Used to
;                apply a normalization factor. Defualt is 1 (assumes no smoothing)
;
;OUTPUTS:
;   output_data - array of intensity errors. Will have the same size and shape
;                 as "input_data"
;
;HISTORY: Name---------Date---------Description
;         M Weberg  7 JUNE, 2016  Initial coding
;-

FUNCTION CALC_AIA_171_ERRORS, input_data, smooth_num=smooth_num

IF NOT keyword_set(smooth_num) THEN smooth_num = 1

output_data = SQRT(2.3 + 0.06*input_data) / SQRT(smooth_num)

RETURN, output_data

END
