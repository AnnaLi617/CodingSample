* Coding Sample
* Xinmo Li
* ====================================================
* Note: This coding sample aim to conduct DID analysis on the effect of scholarship program on the enrollment in 2-year community college.

clear all
cap log close 

cd ".../DataTask_XinmoLi" //Please change the directory

log using "DataTask_XinmoLi.smcl", replace

* Converting format
local myfilelist : dir . files "*.csv"
foreach file of local myfilelist{
	local filename=subinstr("`file'",".csv","",.)
	import delimited "`file'", clear
	save "`filename'.dta", replace
}

* A. Assembling the Data * ====================================================

* add year variable
forvalues year=2010/2015 {
	use "hd`year'.dta", clear
	gen year=`year'
	save "hd`year'_wyear", replace
}

* Combining Data
use "hd2010_wyear.dta", clear
forvalues year = 2011/2015 {
	append using "hd`year'_wyear.dta"
}

merge m:1 unitid using "sfa1015.dta"
keep if _merge == 3
drop _merge

cap gen ID_IPEDS = unitid
drop unitid // renaming to ID_IPEDS

foreach var in scugrad scugffn scugffp fgrnt_p fgrnt_a sgrnt_p sgrnt_a {
	cap gen `var' = .
	forvalues year = 2010/2015{
		replace `var' = `var'`year' if year == `year'
	}
	drop if `var' == .
}

// check if all variables are matched by year
foreach var in scugrad scugffn scugffp fgrnt_p fgrnt_a sgrnt_p sgrnt_a {
	cap gen `var' = .
	forvalues year = 2010/2015{
		tab `var'`year' if `var' != `var'`year' & year == `year'
	}
}

// drop variables with years
foreach var in scugrad scugffn scugffp fgrnt_p fgrnt_a sgrnt_p sgrnt_a {
	cap gen `var' = .
	forvalues year = 2010/2015{
		drop `var'`year'
	}
}


* Matching zip code
replace zip = trim(zip)
replace zip = "." if missing(zip)
drop if zip == "."
cap gen zip_5digits = substr(zip, 1, 5)

save "CleanedData.dta", replace

use "zip_to_stabbr_xwalk.dta", clear
tostring zip, replace
cap gen zip_5digits = substr("00000", 1, 5 - length(zip)) + zip if length(zip) < 5
replace zip_5digits = zip if length(zip) == 5

save "5digit_zip_to_stabbr_xwalk.dta", replace

use "CleanedData.dta", clear 
merge m:1 zip_5digits using "5digit_zip_to_stabbr_xwalk.dta"
keep if _merge == 3
drop _merge
drop zip_5digits

* Generating variables and Balancing data
keep if stabbr == "TN"

tab ugoffer
cap gen degree_bach = 0
replace degree_bach = 1 if ugoffer == 1 & deggrant == 1
tab degree_bach

tab groffer
cap gen degree_masters = 0
replace degree_masters = 1 if groffer == 1 & deggrant == 1
tab degree_masters

drop if degree_bach == 0 & degree_masters == 1

tab control
drop if control == -3
cap gen public = 0
replace public = 1 if control == 1
tab public

cap gen enroll_ftug = scugffn
cap gen grant_state_students = enroll_ftug * (sgrnt_p * 0.01)
cap gen grant_state = grant_state_students * sgrnt_a
sum grant_state

cap gen grant_federal_students = enroll_ftug * (fgrnt_p * 0.01)
cap gen grant_federal = grant_federal_students * fgrnt_a
sum grant_federal

bysort ID_IPEDS: gen years_present = _N
keep if years_present == 6


label var ID_IPEDS "Institution Identifier"
label var year "Year"
label var degree_bach "Bachelor's Degree-Granting Institutions DV"
label var public "Public Institution DV"
label var enroll_ftug "Number of FTFT Undergraduates"
label var grant_state "Total amount of state and local grant aid"
label var grant_federal "Total amount of federal grant aid"

* Variables for DID regressions
// DID Regression 1
cap gen Post = 0
replace Post = 1 if year >= 2015

cap gen Treated = 0 
replace Treated = 1 if public == 1 & degree_bach == 0

cap gen Post_Treated = Post * Treated

// DID Regression 2 & 3
cap gen college_type = .
replace college_type = 1 if public == 1 & degree_bach == 1
replace college_type = 2 if public == 1 & degree_bach == 0
replace college_type = 3 if public == 0 & degree_bach == 1
replace college_type = 4 if public == 0 & degree_bach == 0



save "CleanedData.dta", replace


* B. Analysis * ===============================================================

* 1) 

use "CleanedData.dta", replace
cap gen degree_type = "Two-Year College"
replace degree_type = "Four-Year College" if degree_bach == 1
cap gen institution_type = "Private"
replace institution_type = "Public" if public == 1
label var degree_type "Types of College"
label var institution_type "Types of Institution"

collect clear
local vlist enroll_ftug grant_state grant_federal
collect: table ( degree_type institution_type var ) ( result year ) (), ///
    totals(year) ///
	statistic(mean `vlist') ///
    nformat(%9.0f)
collect export "sum_stats", as(tex) tableonly replace

* 2/a)
cap gen grant_total = grant_state + grant_federal
cap egen avg_grant_total = mean(grant_total), by(year degree_bach public)
label var avg_grant_total "Average State and Federal Grants"
twoway (connect avg_grant_total year if degree_bach == 1 & public == 0, sort lcolor(dkblue) mcolor(dkblue) msymbol(circle)) ///
	   (connect avg_grant_total year if degree_bach == 1 & public == 1, sort lcolor(dkgreen) mcolor(dkgreen) msymbol(square)) ///
	   (connect avg_grant_total year if degree_bach == 0 & public == 0, sort lcolor(dkorange) mcolor(dkorange) msymbol(diamond)) ///
	   (connect avg_grant_total year if degree_bach == 0 & public == 1, sort lcolor(red) mcolor(red) msymbol(triangle)), ///
	   ylabel(#7, labsize(small)) ///
	   xlabel(#6) ///
	   title(`"Average State and Federal Grant Aids"') ///
	   legend(on label(1 "Private 4 year") ///
		label(2 "Public 4 year") ///
		label(3 "Private 2 year") ///
		label(4 "Public 2 year"))
graph export "Average State and Federal Grants.png", replace as(png) name("Graph")


* 2/b)
cap egen avg_entroll_ftug = mean(enroll_ftug), by(year degree_bach public)
label var avg_entroll_ftug "Average FTFT Undergraduates Enrolled"
twoway (connect avg_entroll_ftug year if degree_bach == 1 & public == 0, sort lcolor(dkblue) mcolor(dkblue) msymbol(circle)) ///
	   (connect avg_entroll_ftug year if degree_bach == 1 & public == 1, sort lcolor(dkgreen) mcolor(dkgreen) msymbol(square)) ///
	   (connect avg_entroll_ftug year if degree_bach == 0 & public == 0, sort lcolor(dkorange) mcolor(dkorange) msymbol(diamond)) ///
	   (connect avg_entroll_ftug year if degree_bach == 0 & public == 1, sort lcolor(red) mcolor(red) msymbol(triangle)), ///
	   ylabel(#7) xlabel(#6) title(`"Average School Level FTFT Undergraduates Enrolled"') ///
	   legend(on label(1 "Private 4 year") ///
		label(2 "Public 4 year") ///
		label(3 "Private 2 year") ///
		label(4 "Public 2 year"))
graph export "Average School Level FTFT UG.png", replace as(png) name("Graph")

* 3) Rregression Analysis: DID

xtset ID_IPEDS year, yearly

// DID Regression 1
eststo MainDID : reghdfe enroll_ftug Post Treated Post_Treated grant_federal grant_state, absorb (ID_IPEDS year) vce(cluster ID_IPEDS)

// Parallel Trend Assumption 
gen event = year - 2015 if Treated == 1
tab event, gen(eventt)
forvalues i = 1/6 {
	replace eventt`i' = 0 if eventt`i' == .
}
xtreg enroll_ftug eventt* grant_federal grant_state i.year, r fe
// year 2015 is not presented in the graph because it was omitted in the regression due to colinearity
coefplot, keep (eventt1 eventt2 eventt3 eventt4 eventt5 eventt6) ///
	coeflabels(eventt1 = "2010" ///
		eventt2 = "2011" ///
		eventt3 = "2012" ///
		eventt4 = "2013" ///
		eventt5 = "2014" ///
		eventt6 = "2015") ///
	vertical ///
	yline(0) ///
	ytitle("Coefficent") ///
	xtitle("Year") ///
	addplot(line @b @at) ///
	ciopts(recast(rcap) lpattern(dash) lwidth(medium))
graph export "Parallel Assumption Test.png", replace as(png) name("Graph")


// check robustness: run the fixed effect model regression with standard error clustered on the types of colleges
preserve
by ID_IPEDS: gen college_type_inconsistent = college_type[1] != college_type[_N]
list ID_IPEDS year college_type if college_type_inconsistent
drop if college_type_inconsistent // dropped 1 observation for this regression: because it changed from type 4 to type 3 in the middle of 2011 to 2015

// DID Regression 2
eststo RobustCheckDID1 : reghdfe enroll_ftug Post Treated Post_Treated grant_federal grant_state, absorb (ID_IPEDS year) vce(cluster college_type)

// DID Regression 3
eststo RobustCheckDID2 : reghdfe enroll_ftug Post Treated Post_Treated grant_federal grant_state, absorb (ID_IPEDS year) vce(cluster ID_IPEDS)
restore

// Export regression result to Latex
esttab MainDID RobustCheckDID1 RobustCheckDID2 using "did_result.tex", ///
	replace style(tex) ///
	label compress title("DID Fixed Effect Regression Result") ///
	varlabels(enroll_ftug "FTFT Students Enrolled" ///
		grant_federal "Total Federal Aids" ///
		grant_state "Total State and Local Aids") ///
	se star(* 0.1 ** 0.05 *** 0.01) ///
    addnote("* p<0.1; ** p<0.05; *** p<0.01" ///
	"Column (1) shows the result of the Fixed Effect regression with standard error clustered on the instituiton level." ///
	"Column (2) shows the result of the regression with standard error clustered on types of college level." ///
	"Note that one institution's 6 year data was dropped because it switched group." ///
	"Column (3) presents the result of regression clustered on the types of college level without the institution that switched group.")
	
cap log close
