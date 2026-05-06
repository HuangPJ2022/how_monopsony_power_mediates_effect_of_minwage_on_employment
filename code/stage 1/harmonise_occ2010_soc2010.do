// Author: Po-Jui Huang
// Date: 10th Nov. 2025
// Goal: harmonise occ2010 to soc 2010
/*====================*/
cd "my path"

// adjust occ code 
import excel "./rawdata/occ2010_soc2010.xlsx", sheet("Sheet1") firstrow clear
destring CensusCode, replace
rename CensusCode occ2010
rename SOCCode soc2010
replace soc2010 = trim(soc2010)
save "./rawdata/occ2010_soc2010.dta", replace

rename occ2010 occ10ly
rename soc2010 soc101y
replace soc101y = trim(soc101y)
save "./rawdata/occ10ly_soc10ly.dta", replace

// merge
use "./rawdata/cps_00006.dta", clear


preserve
forvalues start = 1980(10)2020 {
    local end = `start' + 10
	keep if (year >= `start') & (year < `end')
	dis "(year >= `start') & (year < `end')" _N
	save "./rawdata/cps_`start'_`end'.dta", replace
	
	merge m:m occ2010 using "./rawdata/occ2010_soc2010.dta"
	drop _merge

	merge m:m occ10ly using "./rawdata/occ10ly_soc10ly.dta"
	drop _merge
	
	replace soc2010 = "niu" if occ2010 == 9999
	replace soc101y = "niu" if occ10ly == 9999
	replace soc2010 = trim(soc2010)
	replace soc101y = trim(soc101y)
	label var soc2010 "2010 SOC Code"
	label var soc101y "2010 SOC Code last year"

	// check merging results(both causing 0 real changes)
	gen merge_soc2010_fail = 0
	gen merge_soc10ly_fail = 0
	replace merge_soc2010_fail = 1 if (soc2010 == "") & (occ2010 != .)
	replace merge_soc10ly_fail = 1 if (soc101y == "") & (occ10ly != .)

	drop merge_soc*

	save "./temp/cps_soc2010_merged_`start'_`end'.dta", replace
	
	
	restore
	
	if `start' < 2020{
		preserve
	}
}