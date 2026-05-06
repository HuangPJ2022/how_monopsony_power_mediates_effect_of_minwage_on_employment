// Author: Po-Jui Huang (code with Claude together)
// Date: 19th Nov. 2025
// Goal:
// 1. Append ONET statistics
// 2. Keep questions related to interested skill set and transform ONET data into skill score (defined by Acemoglu, Daron & Autor, David, 2011. "Skills, Tasks and Technologies: Implications for Employment and Earnings," Handbook of Labor Economics, in: O. Ashenfelter & D. Card (ed.), Handbook of Labor Economics, edition 1, volume 4, chapter 12, pages 1043-1171, Elsevier.)  
// 3. Merge ONETSOC 2010 and ONETSOC 2019 crosswalk
// 4. Collapse by SOC 2010
/*====================*/
cd "my path"
use ".\rawdata\onet\Abilities.dta", clear
append using ".\rawdata\onet\Work Activities.dta", force
append using ".\rawdata\onet\Work Context.dta", force
replace ONETSOCCode = trim(ONETSOCCode)

keep if ScaleID == "IM" | ScaleID == "CX" // keep the ONET aggregated value

/*===== keep questions related to our interested skill set and transform ONET data into skill score  =====*/
// drop irrelevant questions
gen keepme = 0
foreach v in ///
    4.A.2.a.4 4.A.2.b.2 4.A.4.a.1 4.A.4.a.4 4.A.4.b.4 4.A.4.b.5 ///
    4.C.3.b.7 4.C.3.b.4 4.C.3.b.8 4.C.3.d.3 4.A.3.a.3 4.C.2.d.1.i ///
    4.A.3.a.4 4.C.2.d.1.g 1.A.2.a.2 1.A.1.f.1 ///
    4.C.1.a.2.l 4.A.4.a.5 4.A.4.a.8 4.A.1.b.2 ///
    4.A.3.a.2 4.A.3.b.4 4.A.3.b.5 ///
{
    replace keepme = 1 if ElementID == "`v'"
}

keep if keepme == 1
drop keepme

// Define the lists for each skill set
gen byte skill_cat = .
local g1_codes "4.A.2.a.4 4.A.2.b.2 4.A.4.a.1"
local g2_codes "4.A.4.a.4 4.A.4.b.4 4.A.4.b.5"
local g3_codes "4.C.3.b.7 4.C.3.b.4 4.C.3.b.8"
local g4_codes "4.C.3.d.3 4.A.3.a.3 4.C.2.d.1.i"
local g5_codes "4.A.3.a.4 4.C.2.d.1.g 1.A.2.a.2 1.A.1.f.1"
local g6_codes "4.C.1.a.2.l 4.A.4.a.5 4.A.4.a.8 4.A.1.b.2 4.A.3.a.2 4.A.3.b.4 4.A.3.b.5"

// Loop through each list and assign the group number
//// Group 1: Non-routine cognitive: Analytical
foreach code of local g1_codes {
    replace skill_cat = 1 if ElementID == "`code'"
}

//// Group 2: Non-routine cognitive: Interpersonal
foreach code of local g2_codes {
    replace skill_cat = 2 if ElementID == "`code'"
}

//// Group 3: Routine cognitive
foreach code of local g3_codes {
    replace skill_cat = 3 if ElementID == "`code'"
}

//// Group 4: Routine manual
foreach code of local g4_codes {
    replace skill_cat = 4 if ElementID == "`code'"
}

//// Group 5: Non-routine manual physical
foreach code of local g5_codes {
    replace skill_cat = 5 if ElementID == "`code'"
}

//// Group 6: Offshorability
foreach code of local g6_codes {
    replace skill_cat = 6 if ElementID == "`code'"
}

// Keep only the rows that were assigned a group
keep if skill_cat != .

// label value
label define skill_lbl 1 "Non-routine cognitive: Analytical" ///
                       2 "Non-routine cognitive: Interpersonal" ///
                       3 "Routine Cognitive" ///
                       4 "Routine Manual" ///
                       5 "Non-routine manual physical" ///
                       6 "Offshorability"
label values skill_cat skill_lbl

// check(1-4 the same N, 5 6 greater proportion N)
tab skill_cat

// reverse value(we use reversed value in some questions.)
foreach v in 4.C.3.b.8 4.C.1.a.2.l 4.A.4.a.5 4.A.4.a.8 4.A.1.b.2 4.A.3.a.2 4.A.3.b.4 4.A.3.b.5{
	replace DataValue = 5 - DataValue if ElementID == "`v'"
}

bysort ONETSOCCode: egen max_N = max(N)

// compute skill score on this each skill set
collapse (mean) skill_score = DataValue, by(ONETSOCCode skill_cat max_N)


/*==========Merge ONETSOC 2010 and ONETSOC 2019 crosswalk==========*/
rename ONETSOCCode onetsoc2019code // rename by the var name in the using data
merge m:m onetsoc2019code using ".\rawdata\onet\onet_2010_to_2019_Crosswalk.dta" // applying m:m is to keep observations as many as possible
save ".\temp\onet_merged.dta", replace

/*==========Collapse by SOC 2010==========*/
// generate soc2010
gen soc2010 = substr(onetsoc2010code, 1, 7)

// collapse unweighted mean by SOC 2010 and skill set
collapse (mean) skill_score (max) max_N, by(soc2010 skill_cat)

// standardise skill score(faciliate comparison late)
bys skill_cat: egen z_skill = std(skill_score)

// deal with missing data
drop if soc2010 == "" 
drop if skill_cat == .

// temp save(should modify this later)
save ".\temp\temp_onet_soc2010_merged.dta", replace

// generate a filled dataset(N_occ * N_categories)
levelsof soc2010, local(occ)
local nocc : word count `occ'

clear

// Total obs = number of occupations * 6
set obs `=`nocc'*6'

// fill soc2010 variable
gen soc2010 = ""
local i = 1
foreach o of local occ {
    forvalues j = 1/6 {
        replace soc2010 = "`o'" in `i'
        local i = `i' + 1
    }
}

// fill skill_cat (1–6 repeated for each occupation)
gen skill_cat = .
forvalues i = 1/`=_N' {
    replace skill_cat = mod(`i'-1,6)+1 in `i'
}

// label categories value
label define skill_lbl 1 "Non-routine cognitive: Analytical" ///
                       2 "Non-routine cognitive: Interpersonal" ///
                       3 "Routine Cognitive" ///
                       4 "Routine Manual" ///
                       5 "Non-routine manual physical" ///
                       6 "Offshorability"
label values skill_cat skill_lbl

// check
bysort soc2010: egen n_cat = total(!missing(skill_cat))
tab n_cat
drop n_cat
save ".\temp\temp_onet_fill.dta", replace

// Merge with original data to fill in missing values
merge 1:1 soc2010 skill_cat using ".\temp\temp_onet_soc2010_merged.dta"
drop if _merge == 2
drop _merge

// check
bysort soc2010: egen n_cat = total(!missing(skill_cat))
tab n_cat
drop n_cat

// reshape can help us merge CPS and ONET later
reshape wide skill_score z_skill max_N, i(soc2010) j(skill_cat)
drop max_N2 max_N3 max_N4 max_N5 max_N6
rename max_N1 max_N

// label to help us identify
label variable skill_score1 "Non-routine cognitive: Analytical"
label variable skill_score2 "Non-routine cognitive: Interpersonal"
label variable skill_score3 "Routine Cognitive"
label variable skill_score4 "Routine Manual"
label variable skill_score5 "Non-routine manual physical"
label variable skill_score6 "Offshorability"
label variable z_skill1 "Non-routine cognitive: Analytical (z-score)"
label variable z_skill2 "Non-routine cognitive: Interpersonal (z-score)"
label variable z_skill3 "Routine Cognitive (z-score)"
label variable z_skill4 "Routine Manual (z-score)"
label variable z_skill5 "Non-routine manual physical (z-score)"
label variable z_skill6 "Offshorability (z-score)"

/*========== 7 digit: base save ==========*/
save ".\temp\onet_soc2010_merged.dta", replace

* Initialise matched codes tracker
keep soc2010
save ".\temp\matched_codes.dta", replace

/*========== 6 digits ==========*/
use ".\temp\onet_soc2010_merged.dta", clear
preserve
replace soc2010 = substr(soc2010, 1, 6) + "0"         // pad to 7 characters
drop if missing(soc2010)
merge m:1 soc2010 using ".\temp\matched_codes.dta", gen(dup_merge)
drop if dup_merge == 3
drop dup_merge
collapse (mean) skill_score* z_skill* [aw = max_N], by(soc2010)
save ".\temp\onet_soc2010_6digit.dta", replace

* Update matched codes
keep soc2010
append using ".\temp\matched_codes.dta"
duplicates drop
save ".\temp\matched_codes.dta", replace
restore

/*========== 5 digits ==========*/
preserve
replace soc2010 = substr(soc2010, 1, 5) + "00"        // pad to 7 characters
drop if missing(soc2010)
merge m:1 soc2010 using ".\temp\matched_codes.dta", gen(dup_merge)
drop if dup_merge == 3
drop dup_merge
collapse (mean) skill_score* z_skill* [aw = max_N], by(soc2010)
save ".\temp\onet_soc2010_5digit.dta", replace

* Update matched codes
keep soc2010
append using ".\temp\matched_codes.dta"
duplicates drop
save ".\temp\matched_codes.dta", replace
restore

/*========== 4 digits ==========*/
preserve
replace soc2010 = substr(soc2010, 1, 4) + "000"       // pad to 7 characters
drop if missing(soc2010)
merge m:1 soc2010 using ".\temp\matched_codes.dta", gen(dup_merge)
drop if dup_merge == 3
drop dup_merge
collapse (mean) skill_score* z_skill* [aw = max_N], by(soc2010)
save ".\temp\onet_soc2010_4digit.dta", replace

* Update matched codes
keep soc2010
append using ".\temp\matched_codes.dta"
duplicates drop
save ".\temp\matched_codes.dta", replace
restore

/*========== 3 digits ==========*/
preserve
replace soc2010 = substr(soc2010, 1, 3) + "0000"      // pad to 7 characters
drop if missing(soc2010)
merge m:1 soc2010 using ".\temp\matched_codes.dta", gen(dup_merge)
drop if dup_merge == 3
drop dup_merge
collapse (mean) skill_score* z_skill* [aw = max_N], by(soc2010)
save ".\temp\onet_soc2010_3digit.dta", replace

* Update matched codes
keep soc2010
append using ".\temp\matched_codes.dta"
duplicates drop
save ".\temp\matched_codes.dta", replace
restore

/*========== Append all ==========*/
use ".\temp\onet_soc2010_merged.dta", clear
drop max_N
append using ".\temp\onet_soc2010_6digit.dta"
append using ".\temp\onet_soc2010_5digit.dta"
append using ".\temp\onet_soc2010_4digit.dta"
append using ".\temp\onet_soc2010_3digit.dta"

rename soc2010 soc2010_to_match
save ".\temp\onet_soc2010_merged_final.dta", replace

