// Author: Po-Jui Huang 
// Date: 6th May 2026 
// Goal:  Estimates Model 1 — correlation between the monopsony power and wage compression
/*====================*/

cd "my path"
global dir "260506_wageCompression"
cap mkdir "./results"
cap mkdir "./results/$dir"


use "./temp/cps_soc2010_merged_clean_earn_1980_1990.dta", clear
forvalues start = 1990(10)2020 {
    local end = `start' + 10
	dis "now `start' to `end'"
    append using "./temp/cps_soc2010_merged_clean_earn_`start'_`end'.dta"
}

merge m:1 statefips quarterly_date using "./rawdata\mw_state_stata\mw_state_quarterly.dta"
drop if _merge == 2
drop _merge

bysort statefips quarterly_date ind1990: gen cell_size_syqi = _N


/*========================================*/
*        1. Summary Stats for all obs
/*========================================*/

estpost summarize hourwage if (paidhour == 2) & (qhourwag == 0), listwise detail
estimates store sum3
esttab sum3 using "./results/$dir/monopsony_power_wage_compression_sum_stats.tex", replace   ///
    cells("mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3)) p10(fmt(3)) p25(fmt(3)) p50(fmt(3)) p75(fmt(3)) p90(fmt(3)) count") ///
    label title("Summary Stats: Actual Hourwage") nonumber nomtitle

estimates clear

/*========================================*/
*       2 Measure 1: Kaitz index
*               Actual Wage        
/*========================================*/
preserve

keep if paidhour == 2
keep if qhourwag == 0

collapse (p50) me_wage_syqi = hourwage (mean) min_fed_mw min_mw cell_size_syqi [pweight=earnwt], by(statefips quarterly_date ind1990)

drop if cell_size_syqi < 20

gen kaitz_syqi = min_mw / me_wage_syqi

rename quarterly_date current_qt
gen quarterly_date = current_qt - 40
format quarterly_date %tq

merge 1:1 statefips ind1990 quarterly_date using "./temp/newRecruit_rate_yqt.dta"
replace newRecruit_rate = 0 if _merge == 1
drop if _merge == 2
drop _merge

drop quarterly_date
rename current_qt quarterly_date
	
/*=========== Summary, Reg, Plot ===========*/
estpost summarize newRecruit_rate kaitz_syqi, listwise detail
estimates store sum_kaitz_act
esttab sum_kaitz_act using "./results/$dir/monopsony_power_wage_compression_sum_stats.tex", append ///
    cells("mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3)) p10(fmt(3)) p25(fmt(3)) p50(fmt(3)) p75(fmt(3)) p90(fmt(3))") ///
    label title("Summary Stats: Monopsony Power and Kaitz Index") ///
    nonumber nomtitle
estimates clear

binscatter newRecruit_rate kaitz_syqi, ///
    ytitle("The Fraction of New Recruits from Non-employment") ///
    xtitle("Kaitz Index (state-qrt-ind)") ///
    title("Monopsony Power and Kaitz Index") ///
    line(lfit) ///
    yscale(range(0 1)) ///
    ylabel(0(0.2)1) ///
    scheme(s1mono)
graph export "./results/$dir/Monopsony_Kaitz_Plot.png", as(png) width(3000) replace

gen newRecruit_rate_2 = newRecruit_rate^2

estimates clear
qui eststo: reg kaitz_syqi newRecruit_rate statefips ind1990 quarterly_date, r
qui eststo: reg kaitz_syqi newRecruit_rate newRecruit_rate_2 statefips ind1990 quarterly_date, r
qui eststo: reghdfe kaitz_syqi newRecruit_rate, absorb(statefips ind1990 statefips#quarterly_date ind1990#quarterly_date)
esttab
esttab using "./results/$dir/monopsony_power_wage_compression_reg_tab.tex", replace ///
    title("Monopsony Power and Kaitz Index: Actual Wage") ///
    mtitles("no FE" "no FE + square term" "FE") ///
    coeflabels(newRecruit_rate "Monopsony Power" newRecruit_rate_2 "Squared Term") ///
    addnotes("Monopsony power is measured by the fraction of new recruits from non-employment by industry." "The final columns is a fixed-effects model.") ///
    keep(newRecruit_rate newRecruit_rate_2) ///
    ar2 b(3) se star(* 0.10 ** 0.05 *** 0.01)
estimates clear

restore


/*========================================*/
*       3 Measure 2: Coverage
*               Actual Wage        
/*========================================*/
preserve

keep if paidhour == 2
keep if qhourwag == 0

gen is_covered_wm = (hourwage <= next_min_mw) 

collapse (mean) mw_coverage = is_covered_wm (mean) min_fed_mw min_mw cell_size_syqi [pweight=earnwt], by(statefips quarterly_date ind1990)
drop if cell_size_syqi < 20

rename quarterly_date current_qt
gen quarterly_date = current_qt - 40
format quarterly_date %tq

merge 1:1 statefips ind1990 quarterly_date using "./temp/newRecruit_rate_yqt.dta"
replace newRecruit_rate = 0 if _merge == 1
drop if _merge == 2
drop _merge

drop quarterly_date
rename current_qt quarterly_date
	
/*=========== Summary, Reg, Plot ===========*/
estpost summarize newRecruit_rate mw_coverage, listwise detail
estimates store sum_cov_act
esttab sum_cov_act using "./results/$dir/monopsony_power_wage_compression_sum_stats.tex", append ///
    cells("mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3)) p10(fmt(3)) p25(fmt(3)) p50(fmt(3)) p75(fmt(3)) p90(fmt(3))") ///
    label title("Summary Stats: Monopsony Power and Coverage") ///
    nonumber nomtitle
estimates clear

binscatter newRecruit_rate mw_coverage, ///
    ytitle("The Fraction of New Recruits from Non-employment") ///
    xtitle("MW Coverage (state-qrt-ind)") ///
    title("Monopsony Power and MW Coverage") ///
    line(lfit) ///
    yscale(range(0 1)) ///
    ylabel(0(0.2)1) ///
    scheme(s1mono)
graph export "./results/$dir/Monopsony_Coverage_Plot.png", as(png) width(3000) replace

gen newRecruit_rate_2 = newRecruit_rate^2

estimates clear
qui eststo: reg mw_coverage newRecruit_rate statefips ind1990 quarterly_date, r
qui eststo: reg mw_coverage newRecruit_rate newRecruit_rate_2 statefips ind1990 quarterly_date, r
qui eststo: reghdfe mw_coverage newRecruit_rate, absorb(statefips ind1990 statefips#quarterly_date ind1990#quarterly_date)
esttab
esttab using "./results/$dir/monopsony_power_wage_compression_reg_tab.tex", append ///
    title("Monopsony Power and Coverage: Actual Wage") ///
    mtitles("no FE" "no FE + square term" "FE") ///
    coeflabels(newRecruit_rate "Monopsony Power" newRecruit_rate_2 "Squared Term") ///
    addnotes("Monopsony power is measured by the fraction of new recruits from non-employment by industry."  "The final columns is a fixed-effect model.") ///
    keep(newRecruit_rate newRecruit_rate_2) ///
    ar2 b(3) se star(* 0.10 ** 0.05 *** 0.01)
estimates clear

restore