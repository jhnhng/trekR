# trekR 0.0.1.0000

## Major Change 
* The added code to functions `between_emd` and `within_emd` to allow for the calculation of EMDs even when the complete number of intervals are not present. 

## Minor change 
* The `\` was changed to `function` in the `lapply` function present in the `between_emd` and `within_emd` functions to allow 
R versions <4.1 to install the package 

## Fixes in the works
* Errors arise from using `emspeeds` due to matrices with different dimensions. Current solution to issue is to remove all individuals that have matrices with different dimensions. Example of this is shown in "Setting Up the Data"
