// Author: Po-Jui Huang 
// Date: 3rd Apr. 2026 
// Goal:  Computes mean wage datasets in three versions: imputed wages, actual (non-imputed) wages, and actual wages with gaps filled by imputed values.
/*====================*/

cd "my path"
use "./temp/cps_soc2010_merged_clean_earn_all.dta", clear

/* ======================================== */
* 1. mean imputed wage
/* ======================================== */
global dir "imputed_hourwage"
cap mkdir "./temp"
cap mkdir "./temp/$dir"
winsor2 imputed_hourwage, cuts(1 99) replace

preserve
bysort ind1990 statefips year: gen hwage_cell_size_ist = _N
collapse (mean) imputed_hourwage_ist = imputed_hourwage ///
         (first) hwage_cell_size_ist [aw = wtfinl], ///
         by(ind1990 statefips year)
gegen nq4_imputed_hwage_ist = xtile(imputed_hourwage_ist), nquantiles(4) by(statefips year)
gegen nq_imputed_hwage_ist  = xtile(imputed_hourwage_ist), nquantiles(10) by(statefips year)
save "./temp/$dir/average_imputed_hwage_ist.dta", replace
restore

preserve
bysort ind1990 year: gen hwage_cell_size_it = _N
collapse (mean) imputed_hourwage_it = imputed_hourwage ///
         (first) hwage_cell_size_it [aw = wtfinl], ///
         by(ind1990 year)
gegen nq4_imputed_hwage_it = xtile(imputed_hourwage_it), nquantiles(4) by(year)
gegen nq_imputed_hwage_it  = xtile(imputed_hourwage_it), nquantiles(10) by(year)
save "./temp/$dir/average_imputed_hwage_it.dta", replace
restore

/* ======================================== */
* 2. mean actual wage
/* ======================================== */
global dir "actual_hourwage"
cap mkdir "./temp/$dir"

keep if paidhour == 2
keep if qhourwag == 0

preserve
bysort ind1990 statefips year: gen hwage_cell_size_ist = _N
collapse (mean) actual_hourwage_ist = hourwage ///
         (first) hwage_cell_size_ist [aw = wtfinl], ///
         by(ind1990 statefips year)
gegen nq4_actual_hwage_ist = xtile(actual_hourwage_ist), nquantiles(4) by(statefips year)
gegen nq_actual_hwage_ist  = xtile(actual_hourwage_ist), nquantiles(10) by(statefips year)
save "./temp/$dir/average_actual_hwage_ist.dta", replace
restore

preserve
bysort ind1990 year: gen hwage_cell_size_it = _N
collapse (mean) actual_hourwage_it = hourwage ///
         (first) hwage_cell_size_it [aw = wtfinl], ///
         by(ind1990 year)
gegen nq4_actual_hwage_it = xtile(actual_hourwage_it), nquantiles(4) by(year)
gegen nq_actual_hwage_it  = xtile(actual_hourwage_it), nquantiles(10) by(year)
save "./temp/$dir/average_actual_hwage_it.dta", replace
restore

/* ======================================== */
* 3. combined wage: actual + imputed fill
*  uses actual when available, imputed for gap years
/* ======================================== */
global dir "combined_hourwage"
cap mkdir "./temp/$dir"

* --- ind1990 × statefips × year ---
use "./temp/actual_hourwage/average_actual_hwage_ist.dta", clear
append using "./temp/imputed_hourwage/average_imputed_hwage_ist.dta"

* Keep actual when both exist, imputed only for gap years
gen has_actual = !missing(actual_hourwage_ist)
bysort ind1990 statefips year (has_actual): keep if _n == _N
drop has_actual

* Create combined variable
gen combined_hourwage_ist = cond(!missing(actual_hourwage_ist), ///
                                 actual_hourwage_ist, ///
                                 imputed_hourwage_ist)
gen wage_source_ist = cond(!missing(actual_hourwage_ist), "actual", "imputed")

* Compute quantiles
drop nq4_actual_hwage_ist nq_actual_hwage_ist nq4_imputed_hwage_ist nq_imputed_hwage_ist
gegen nq4_combined_hwage_ist = xtile(combined_hourwage_ist), nquantiles(4) by(statefips year)
gegen nq_combined_hwage_ist  = xtile(combined_hourwage_ist), nquantiles(10) by(statefips year)

gen hwage_cell_size_combined_ist = cond(!missing(actual_hourwage_ist), ///
                                        hwage_cell_size_ist, ///
                                        hwage_cell_size_ist)

save "./temp/$dir/average_combined_hwage_ist.dta", replace

* --- ind1990 × year ---
use "./temp/actual_hourwage/average_actual_hwage_it.dta", clear
append using "./temp/imputed_hourwage/average_imputed_hwage_it.dta"

gen has_actual = !missing(actual_hourwage_it)
bysort ind1990 year (has_actual): keep if _n == _N
drop has_actual

gen combined_hourwage_it = cond(!missing(actual_hourwage_it), ///
                                actual_hourwage_it, ///
                                imputed_hourwage_it)
gen wage_source_it = cond(!missing(actual_hourwage_it), "actual", "imputed")

drop nq4_actual_hwage_it nq_actual_hwage_it nq4_imputed_hwage_it nq_imputed_hwage_it
gegen nq4_combined_hwage_it = xtile(combined_hourwage_it), nquantiles(4) by(year)
gegen nq_combined_hwage_it  = xtile(combined_hourwage_it), nquantiles(10) by(year)

save "./temp/$dir/average_combined_hwage_it.dta", replace