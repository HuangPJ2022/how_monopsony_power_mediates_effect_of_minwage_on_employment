// Author: Po-Jui Huang 
// Date: 21st Apr. 2026 
// Goal:  Computes average minimum wage coverage (share of affected workers) separately for events with ≥5% and ≥10% wage growth rates.
/*====================*/

cd "my path"

use "./temp/cps_soc2010_merged_clean_earn_1980_1990.dta", clear
forvalues start = 1990(10)2020 {
    local end = `start' + 10
	dis "now `start' to `end'"
    append using "./temp/cps_soc2010_merged_clean_earn_`start'_`end'.dta"
}

merge m:1 statefips quarterly_date using "./rawdata\mw_state_stata\mw_state_quarterly.dta"
drop if _merge == 2
drop _merge


preserve

keep if paidhour == 2
keep if qhourwag == 0

gen is_covered_wm = (hourwage <= next_min_mw) 

collapse (mean) mw_coverage = is_covered_wm (mean) min_mw [pweight=earnwt], by(statefips quarterly_date)



merge m:1 statefips quarterly_date using "./rawdata\mw_state_stata\mw_state_quarterly.dta"


drop if _merge == 2
drop _merge

keep if policy_state == 1

gen gr_mw = diff_lag_mw / lag_min_mw

gen year = yofd(dofq(quarterly_date))
drop if year > 2016
drop if year < 1990

keep stateabb statefips statename year quarterly_date gr_mw mw_coverage

gen gr_5 = (gr_mw >= 0.05)
gen gr_10 = (gr_mw >= 0.1)

bys gr_5: egen avg_cover_5 = mean(mw_coverage)
bys gr_10: egen avg_cover_10 = mean(mw_coverage)

sum avg_cover_5
sum avg_cover_10

restore