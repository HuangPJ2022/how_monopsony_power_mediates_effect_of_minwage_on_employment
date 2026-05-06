// Author: Po-Jui Huang (code with Claude together)
// Date: 24th March. 2026
// Goal: construct a crosswalk for CPS and my skill measures dataset. This dataset consists of one on one map from an occupation code in CPS to an occupation code in my skill measures dataset. 
/*====================*/

cd "my path"

/*Find code can be matched directly*/
use "./cps_soc2010_merged_all.dta", clear
merge m:1 soc2010 using ".\temp\matched_codes.dta"
keep soc2010 _merge
duplicates drop soc2010, force
save ".\temp\matched_soc_crosswalk.dta", replace

keep if _merge != 2 // drop occupations which only exist in using data
gen matched = 0 // just help us check
replace matched = 1 if (_merge == 3) 

rename soc2010 soc2010_original
gen soc2010_to_match = "" // generate a final matched occu
replace soc2010_to_match = soc2010_original if _merge == 3 

/*looking for 6 digits*/
gen soc2010 = substr(soc2010_original, 1, 6) + "0" if matched == 0
merge m:1 soc2010 using ".\temp\matched_codes.dta", gen(_merge_6)
drop if _merge_6 == 2
replace soc2010_to_match = soc2010 if (_merge_6 == 3) & (matched == 0)
replace matched = 1 if (_merge_6 == 3) | (_merge == 3)
drop soc2010

/*looking for 5 digits*/
gen soc2010 = substr(soc2010_original, 1, 5) + "00" if matched == 0
merge m:1 soc2010 using ".\temp\matched_codes.dta", gen(_merge_5)
drop if _merge_5 == 2
replace soc2010_to_match = soc2010 if (_merge_5 == 3) & (matched == 0)
replace matched = 1 if (_merge_5 == 3) | (_merge_6 == 3) | (_merge == 3)
drop soc2010

/*looking for 4 digits*/
gen soc2010 = substr(soc2010_original, 1, 4) + "000" if matched == 0
merge m:1 soc2010 using ".\temp\matched_codes.dta", gen(_merge_4)
drop if _merge_4 == 2
replace soc2010_to_match = soc2010 if (_merge_4 == 3) & (matched == 0)
replace matched = 1 if (_merge_4 == 3) | (_merge_5 == 3) | (_merge_6 == 3) | (_merge == 3)
drop soc2010

/*looking for 3 digits*/
gen soc2010 = substr(soc2010_original, 1, 3) + "0000" if matched == 0
merge m:1 soc2010 using ".\temp\matched_codes.dta", gen(_merge_3)
drop if _merge_3 == 2
replace soc2010_to_match = soc2010 if (_merge_3 == 3) & (matched == 0)
replace matched = 1 if (_merge_3 == 3) |  (_merge_4 == 3) | (_merge_5 == 3) | (_merge_6 == 3) | (_merge == 3)

// only armed forces cannot be matched. Of course, "", niu, and none are all unmatched.
drop _merge* soc2010 match
rename soc2010_original soc2010

save ".\temp\matched_soc_crosswalk.dta", replace // save a crosswalk

/*merge back the crosswalk. finally merge skill measure*/
use "./cps_soc2010_merged_all.dta", clear
merge m:1 soc2010 using ".\temp\matched_soc_crosswalk.dta"
drop if _merge == 2
merge m:1 soc2010_to_match using ".\temp\onet_soc2010_merged_final.dta", gen(_merge_skill)

tab soc2010_to_match if _merge_skill == 1 // ensure I did all I can. Yes!
drop _merge*

save "./cps_soc2010_merged_onet_all.dta", replace
