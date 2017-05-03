; PURPOSE: This file contains control variables for running the Northumbria 
;          University Wave Tracking (NUWT) code in a semi-automated mode of 
;          operations. Typically this will be performed by by passing the name
;          of this config file as an argument to the automation script. Please
;          see the offical program documentation for more information.
;
; INPUTS: none
; OUTPUTS: structures containing control variables for each major step in the
;          NUWT code.
;
; HISTORY: Name---------Date---------Description
;          M Weberg  6-JUNE-2016  Initial file creation
;

; USAGE NOTE: if you wish to use the default value for an optional submodule 
;             input, simply comment out the line containing the variable. 
;
; *in the case where KEYWORD_SET() is used to check if a variable is used
;  or not, a value of 0 will also be considered as if the variable had not
;  been set.

FUNCTION TEST_KWARGS
kwargs = {$ ;this is the main kwarg structure
    grad : 0.001, $
    ;meas_size : 0, $
    num_search_bins : 5, $
    errors : 0, $
    ;check : 'why is it now working as I desire?', $
    ;cut_chisq : 0, $
    simp_grad : 1, $
    plot_results : 1, $
    test_var : 'yet another test' $
}
RETURN, kwargs
END
              


