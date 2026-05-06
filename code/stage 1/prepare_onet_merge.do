// Author: Po-Jui Huang
// Date: 19th Nov. 2025
// Goal: prepare necesary dta file for future computation and merge using ONET data
/*====================*/
cd "my path"

import excel using ".\data\rawdata\onet\Abilities.xlsx", firstrow clear
save ".\data\rawdata\onet\Abilities.dta", replace

import excel using ".\data\rawdata\onet\Work Activities.xlsx", firstrow clear
save ".\data\rawdata\onet\Work Activities.dta", replace

import excel using ".\data\rawdata\onet\Work Context.xlsx", firstrow clear
save ".\data\rawdata\onet\Work Context.dta", replace

import delimited ".\data\rawdata\onet_2010_to_2019_Crosswalk.csv", varnames(1) clear
replace onetsoc2010code = trim(onetsoc2010code) 
replace onetsoc2019code = trim(onetsoc2019code)
save ".\data\rawdata\onet\onet_2010_to_2019_Crosswalk.dta", replace
