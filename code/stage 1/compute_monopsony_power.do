// Author: Po-Jui Huang 
// Date: 31st Mar., 2nd Apr., 3rd Apr., 21st Apr. 2026
// Goal: construct a measurement of monopsony power: the fraction of new recruitment from the non-employed
/*====================*/

cd "my path"

use "./temp/cps_soc2010_merged_1980_1990.dta", clear

forvalues start = 1990(10)2020 {
    local end = `start' + 10
    append using "./temp/cps_soc2010_merged_`start'_`end'.dta"
}

drop if year > 2021

rename statefip statefips
gen quarter = ceil(month / 3)
gen quarterly_date = yq(year, quarter)
format quarterly_date %tq

drop if asecflag == 1
drop if missing(cpsidp)

sort cpsidp mish
xtset cpsidp mish

gen lag_occ = l.occ2010
drop if missing(lag_occ)

gen same_occ = lag_occ == occ2010
drop if same_occ

gen newRecruit = (lag_occ == 9999)
gen newRecruit_wgt = newRecruit * wtfinl

bysort statefips quarterly_date ind1990: gen cell_size = _N

/* Collapse: year-quarter-industry */
collapse (sum) wtfinl newRecruit_wgt (mean) cell_size, by(ind1990 quarterly_date statefips)
drop if inlist(ind1990, 0, 952)

gen newRecruit_rate = newRecruit_wgt / wtfinl

merge m:1 ind1990 using ".\rawdata\ind1990_details.dta", keepusing(sec1990 cat1990) nogen
drop if ind1990 >= 940

save "./temp/newRecruit_rate_yqt.dta", replace

/* Collapse: year-quarter-category */
preserve
collapse (sum) wtfinl newRecruit_wgt cell_size, by(statefips quarterly_date cat1990)
gen newRecruit_rate_cat = newRecruit_wgt / wtfinl
replace newRecruit_rate_cat = 0 if missing(newRecruit_rate_cat)
save "./temp/newRecruit_rate_yqt_cat.dta", replace
restore

merge m:1 statefips quarterly_date cat1990 using "./temp/newRecruit_rate_yqt_cat.dta", nogen

gegen nq4_monopsony = xtile(newRecruit_rate), nquantiles(4) by(quarterly_date statefips)
gegen nq_monopsony  = xtile(newRecruit_rate), nquantiles(10) by(quarterly_date statefips)
gegen rank_monopsony = rank(newRecruit_rate), by(quarterly_date statefips)

bys quarterly_date statefips: gen pct_monopsony = (rank_monopsony / _N) * 100

save "./temp/newRecruit_rate_yqt.dta", replace

/* Aggregate into year level*/
gen year = yofd(dofq(quarterly_date))

collapse (sum) cell_size_yr = cell_size ///
         (mean) newRecruit_rate_yr = newRecruit_rate [aw = wtfinl], ///
         by(year statefips ind1990)

gegen nq4_monopsony_yr = xtile(newRecruit_rate_yr), nquantiles(4) by(year statefips)
gegen nq_monopsony_yr  = xtile(newRecruit_rate_yr), nquantiles(10) by(year statefips)
gegen rank_monopsony_yr = rank(newRecruit_rate_yr), by(year statefips)

bys year statefips: gen pct_monopsony_yr = (rank_monopsony_yr / _N) * 100

merge m:1 ind1990 using ".\rawdata\ind1990_details.dta", keepusing(sec1990 cat1990)
drop if _merge == 2
drop _merge

keep statefips ind1990 sec1990 cat1990 year ///
     cell_size_yr newRecruit_rate_yr ///
     nq4_monopsony_yr nq_monopsony_yr rank_monopsony_yr pct_monopsony_yr

save "./temp/newRecruit_rate_yr.dta", replace
