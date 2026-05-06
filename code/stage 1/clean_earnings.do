// Author: Po-Jui Huang
// Date: 21st Feb. 2026 
// Goal: drop observations whose earnings can not be imputed.
/*====================*/
cd "my path"

forvalues start = 1980(10)2020 {
    local end = `start' + 10
	dis "now `start' to `end'"
    use "./temp/cps_soc2010_merged_`start'_`end'.dta", clear
	
	if `start' == 2020{
		drop if year > 2021
		dis "I dropped year after `start' + 1"
	}
	
	//drop if asecflag == 1
	drop if cpsidp == 0| cpsidp == .
	
	/*formatting data*/
	rename statefip statefips
	gen quarter = ceil(month / 3)
	gen quarterly_date = yq(year, quarter)
	format quarterly_date %tq

	/*drop data based on accessibility*/
	drop if (earnweek == .) & (paidhour == .) // cannot observe
	drop if (earnweek == 0) & (hourwage == 0) // no earnings data
	drop if (earnweek == 0) & (paidhour == 1) // not paidhour and zero earnings in a week
	drop if (eligorg == 0)  & (paidhour == 0)

	/*drop data based on imputability*/
	drop if (ahrsworkt == 999) & (uhrswork1 > 99) & (uhrsworkorg > 99) & (hourwage > 999) // this group of sample can not impute hour wage
	drop if  (paidhour == 0) & (hourwage > 999) & (earnweek > 999) // same
	drop if (paidhour == 1) & (qearnwee == 0)
	drop  if (uhrswork1 == 0) & (paidhour == 1) & (uhrsworkorg == .) & (uhrsworkt == .) & (ahrsworkt == .)

	/*check hourwage and earnweek*/
	sum hourwage, d
	sum earnweek, d 
	
	/*impute hourwage*/
	gen hrwage_hrsw1   = earnweek / uhrswork1   
	gen hrwage_hrsworg = earnweek / uhrsworkorg 
	gen hrwage_hrswkt  = earnweek / uhrsworkt   
	gen hrwage_ahrswkt = earnweek / ahrsworkt   
	gen imputed_hourwage = max(hrwage_ahrswkt, hrwage_hrsw1, hrwage_hrswkt, hrwage_hrsworg)
	gen missing_imputed = imputed_hourwage == .
	replace imputed_hourwage = hourwage if missing_imputed == 1 & paidhour == 2 // for those still cannot be imputed but receive hourwage. we use the original hourwage data
	
	/*just to check*/
	drop missing_imputed
	gen missing_imputed = imputed_hourwage == .
	tab missing_imputed

	save "./temp/cps_soc2010_merged_clean_earn_`start'_`end'.dta", replace
}

/*===== collect all individual data =====*/
use  "./temp/cps_soc2010_merged_clean_earn_1980_1990.dta", clear
forvalues start = 1990(10)2020 {
	local end = `start' + 10
	dis "now `start' to `end'"
	append using "./temp/cps_soc2010_merged_clean_earn_`start'_`end'.dta"
} 

save "./temp/cps_soc2010_merged_clean_earn_all.dta", replace

