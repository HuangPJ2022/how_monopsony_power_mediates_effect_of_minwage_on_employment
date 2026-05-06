// Author: Po-Jui Huang
// Date: 21st Feb., 3rd, Apr. 2026
// Goal: Processes historical state minimum wage data and identifies all state-level minimum wage change events by quarter.
/*====================*/

cd "my path"

use "./rawdata\mw_state_stata\mw_state_quarterly.dta", clear

xtset statefips quarterly_date

gen lag_min_fed_mw = l.min_fed_mw
gen lag_min_mw = l.min_mw

gen next_min_fed_mw = f.min_fed_mw
gen next_min_mw = f.min_mw

gen diff_lag_fed_mw = min_fed_mw - lag_min_fed_mw
gen diff_lag_mw = min_mw - lag_min_mw

gen policy_fed   = (diff_lag_fed_mw > 0.00001) & (diff_lag_fed_mw !=.)
gen policy_state = (diff_lag_mw > 0.00001)     & (diff_lag_mw !=.) & (policy_fed == 0)

save "./rawdata\mw_state_stata\mw_state_quarterly.dta", replace

keep if policy_state == 1
keep if quarterly_date >= 104

save "./temp/policy_state_event.dta", replace

/*260403 add gr*/
use "./temp/policy_state_event.dta", clear

gen gr_mw = diff_lag_mw / lag_min_mw

sort statefips quarterly_date

save "./temp/policy_state_event.dta", replace