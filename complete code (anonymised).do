*Infant mortality re-analysis
*Written 21/10/2019 by Sean Harrison

/*
Notes: 
Stillbirth = death before birth
Perinatal = stillbirths & deaths at ages under 7 days
Neonatal = deaths at ages under 28 days
Postnatal = deaths at ages 28 days and over, but under 1 year
Infant = deaths at ages under 1 year, excluding perinatal

Death data from: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsregisteredbyareaofusualresidenceenglandandwales
Birth data from: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/datasets/birthsbyareaofusualresidenceofmotheruk

Stata 15 required for transparency in Stata figures

Reference for the SE of incidence rates: http://www.medicine.mcgill.ca/epidemiology/hanley/bios601/vanBelle/ch15.pdf

*/

global cd_data ""
global cd_raw_data "$cd_data\raw data"
global cd_graphs "$cd_data\graphs"
global cd_tables "$cd_data\tables"

*Part 0: Packages
{
/*
ssc install metan
ssc install bspline
*/
}


*Part I: Data prep
{
*Deaths
*Read in from Excel
forvalues i = 10/17 {
	if `i' == 10 {
		import excel "$cd_raw_data\deaths_20`i'.xls", sheet("Table 1a") cellrange(A11:T1246) clear
	}
	if `i' == 11 {
		import excel "$cd_raw_data\deaths_20`i'.xls", sheet("Table 1a") cellrange(A12:Q544) clear
	}
	if `i' == 12 {
		import excel "$cd_raw_data\deaths_20`i'.xls", sheet("Table 1a") cellrange(A11:Q527) clear
	}
	if `i' == 13 {
		import excel "$cd_raw_data\deaths_20`i'.xls", sheet("Table 1a") cellrange(A11:Q528) clear
	}
	if `i' == 14 {
		import excel "$cd_raw_data\deaths_20`i'.xls", sheet("Table 1a") cellrange(A11:Q528) clear
	}
	if `i' == 15 {
		import excel "$cd_raw_data\deaths_20`i'.xls", sheet("Table 1a") cellrange(A11:Q513) clear
	}
	if `i' == 16 {
		import excel "$cd_raw_data\deaths_20`i'.xls", sheet("Table 1a") cellrange(A11:Q513) clear
	}
	if `i' == 17 {
		import excel "$cd_raw_data\deaths_20`i'.xls", sheet("Table 1a") cellrange(A11:Q513) clear
	}
	qui drop if A == ""
	capture qui drop if H == ""
	capture qui drop if H == .
	qui capture drop R S T
	qui replace A = subinstr(A,"  ","",.)
	qui replace A = strtrim(A)
	forvalues j = 0/9 {
		qui replace A = subinstr(A,"`j'","",.)
	}
	rename A area
	rename O rate_infant
	rename P rate_neonatal
	rename Q rate_perinatal
	rename H deaths_infant
	rename I deaths_neonatal
	rename J deaths_perinatal
	qui drop B C D E F G K L M N
	foreach var of varlist deaths* rate* {
		capture qui destring `var', replace force
	}
	gen year = 2000+`i'
	compress
	save "$cd_raw_data\deaths_20`i'.dta", replace
}

*Births
*Read in from Excel
forvalues i = 10/16 {
	if `i' == 10 {
		import excel "$cd_raw_data\births_20`i'.xls", sheet("Table 1a") cellrange(B12:T584) clear
	}
	if `i' == 11 {
		import excel "$cd_raw_data\births_20`i'.xls", sheet("Table 1a") cellrange(B12:T530) clear
	}
	if `i' == 12 {
		import excel "$cd_raw_data\births_20`i'.xls", sheet("Table 1a") cellrange(B12:T530) clear
	}
	if `i' == 13 {
		import excel "$cd_raw_data\births_20`i'.xls", sheet("Table 1") cellrange(B12:T539) clear
	}
	if `i' == 14 {
		import excel "$cd_raw_data\births_20`i'.xls", sheet("Table 1") cellrange(B12:T539) clear
	}
	if `i' == 15 {
		import excel "$cd_raw_data\births_20`i'.xls", sheet("Table 1") cellrange(B12:T539) clear
	}
	if `i' == 16 {
		import excel "$cd_raw_data\births_20`i'.xls", sheet("Table 1") cellrange(B13:T525) clear
	}
	qui drop if B == ""
	qui replace B = subinstr(B,"  ","",.)
	qui replace B = strtrim(B)
	forvalues j = 0/9 {
		qui replace B = subinstr(B,"`j'","",.)
	}
	rename B area
	rename F births_live
	keep area births_live
	foreach var of varlist births* {
		capture qui destring `var', replace force
	}
	gen year = 2000+`i'
	compress
	save "$cd_raw_data\births_20`i'.dta", replace
}

*Area codes
*Read in from Excel
import excel "$cd_raw_data\births_2015.xls", sheet("Table 1") cellrange(A12:B539) clear
qui drop if B == ""
qui replace B = subinstr(B,"  ","",.)
qui replace B = strtrim(B)
forvalues j = 0/9 {
	qui replace B = subinstr(B,"`j'","",.)
}
rename B area
rename A area_code
keep area area_code

*Format areas
qui replace area = subinstr(area," & ", " and ",.)
gen l = length(area)
qui replace area = substr(area,1,l-1) if substr(area,-1,1) == "P"
qui replace area = substr(area,1,l-3) if substr(area,-2,2) == "UA"
qui replace area = subinstr(area," UA "," ",.)
qui replace area = subinstr(area," UA"," ",.)
qui replace area = "Edinburgh, City of" if area == "Edinburgh City of"
qui replace area = "NORTHERN IRELAND" if area == "NORTHERN IRELAND,"
qui replace area = "SCOTLAND" if area == "SCOTLAND ," | area == "SCOTLAND,"
qui replace area = "Somerset County" if area == "Somerset"
qui replace area = "Southend-on-Sea" if area == "Southend on Sea"
qui replace area = "St. Helens" if area == "St Helens"
qui replace area = proper(area)
qui replace area = subinstr(area,"‚",",",.)
qui replace area = subinstr(area,"’","'",.)
qui replace area = subinstr(area,"'","'",.)
qui replace area = "Rhondda Cynon Taff" if area == "Rhondda, Cynon, Taff"
qui replace area = "Rhondda Cynon Taff" if area == "Rhondda, Cynon, Taf"
qui replace area = "The Vale Of Glamorgan" if area == "Vale Of Glamorgan"
drop l

compress
save "$cd_raw_data\area_codes.dta", replace

*Standardise names and append
*Deaths
*Append all years together
use "$cd_raw_data\deaths_2010.dta", clear
forvalues i = 11/17 {
	replace area = strtrim(area)
	append using "$cd_raw_data\deaths_20`i'.dta"
}

*Format areas
qui replace area = subinstr(area," & ", " and ",.)
gen l = length(area)
qui replace area = substr(area,1,l-1) if substr(area,-1,1) == "P"
qui replace area = substr(area,1,l-3) if substr(area,-2,2) == "UA"
qui replace area = subinstr(area," UA "," ",.)
qui replace area = "Edinburgh, City of" if area == "Edinburgh City of"
qui replace area = "NORTHERN IRELAND" if area == "NORTHERN IRELAND,"
qui replace area = "SCOTLAND" if area == "SCOTLAND ," | area == "SCOTLAND,"
qui replace area = "Somerset County" if area == "Somerset"
qui replace area = "Southend-on-Sea" if area == "Southend on Sea"
qui replace area = "St. Helens" if area == "St Helens"
qui replace area = proper(area)
qui replace area = subinstr(area,"‚",",",.)
qui replace area = subinstr(area,"’","'",.)
qui replace area = subinstr(area,"'","'",.)
qui replace area = "Rhondda Cynon Taff" if area == "Rhondda, Cynon, Taff"
qui replace area = "Rhondda Cynon Taff" if area == "Rhondda, Cynon, Taf"
qui replace area = "The Vale Of Glamorgan" if area == "Vale Of Glamorgan"
drop l

*Remove areas with only 1 year of data
duplicates tag area, gen(x)
drop if x == 0
drop x

*Remove duplicate entries
duplicates drop

save "$cd_raw_data\deaths.dta", replace

*Births
*Append all years together
use "$cd_raw_data\births_2010.dta", clear
forvalues i = 11/17 {
	replace area = strtrim(area)
	drop if births_live == .
	append using "$cd_raw_data\births_20`i'.dta"
}

*Format areas
qui replace area = subinstr(area," & ", " and ",.)
gen l = length(area)
qui replace area = substr(area,1,l-1) if substr(area,-1,1) == "P"
qui replace area = substr(area,1,l-3) if substr(area,-2,2) == "UA"
qui replace area = subinstr(area," UA "," ",.)
qui replace area = subinstr(area," UA"," ",.)
qui replace area = "Edinburgh, City of" if area == "Edinburgh City of"
qui replace area = "NORTHERN IRELAND" if area == "NORTHERN IRELAND,"
qui replace area = "SCOTLAND" if area == "SCOTLAND ," | area == "SCOTLAND,"
qui replace area = "Somerset County" if area == "Somerset"
qui replace area = "Southend-on-Sea" if area == "Southend on Sea"
qui replace area = "St. Helens" if area == "St Helens"
qui replace area = proper(area)
qui replace area = subinstr(area,"‚",",",.)
qui replace area = subinstr(area,"’","'",.)
qui replace area = subinstr(area,"'","'",.)
qui replace area = "Rhondda Cynon Taff" if area == "Rhondda, Cynon, Taff"
qui replace area = "Rhondda Cynon Taff" if area == "Rhondda, Cynon, Taf"
qui replace area = "The Vale Of Glamorgan" if area == "Vale Of Glamorgan"
drop l

*Remove areas with only 1 year of data
duplicates tag area, gen(x)
drop if x == 0
drop x

*Remove duplicate entries
duplicates drop

*Merge with area codes
merge m:1 area using "$cd_raw_data\area_codes.dta", nogen keep(1 3)

*Cornwall and the Isles of Scilly are basically just Cornwall
replace area_code = "E06000052" if area == "Cornwall And Isles Of Scilly"

order area_code, a(area)

save "$cd_raw_data\births.dta", replace

*Merge births, deaths & IMD

use "$cd_raw_data\births.dta", clear
merge 1:1 area year using "$cd_raw_data\deaths.dta", nogen keep(2 3)
order year, a(area_code)

*Add in missing area codes
replace area_code = "E09000012" if area == "Hackney"

merge m:1 area_code using "$cd_raw_data\imd.dta", nogen keep(1 3)

*Fill in missing rates
qui replace rate_infant = deaths_infant*1000/births_live if deaths_infant == .
qui replace rate_neonatal = deaths_neonatal*1000/births_live if deaths_neonatal == .

*Estimate births for areas without birth data
replace births_live = int(((1000*deaths_infant/rate_infant)+(1000*deaths_neonatal/rate_neonatal))/2) if births_live == .
replace births_live = int(1000*deaths_infant/rate_infant) if births_live == .
replace births_live = int(1000*deaths_neonatal/rate_neonatal) if births_live == .

*Drop places with too much missing data
drop if rate_infant == . & rate_neonatal == . & rate_perinatal == .

*The rest of the missing area codes are for NI & Scotland, so remove
drop if imd_2015 == .

drop area_reference

sort area year

*Data is too imprecise to estimate the number of stillbirths
*so take perinatal rate N to be births too (won't be too far out)
*Rate variance = cases/N^2
gen rate_infant_se = 1000*sqrt((deaths_infant/births_live^2)*(1-deaths_infant/births_live)) if rate_infant != .
gen rate_neonatal_se = 1000*sqrt((deaths_neonatal/births_live^2)*(1-deaths_neonatal/births_live)) if rate_neonatal != .
gen rate_perinatal_se = 1000*sqrt((deaths_perinatal/births_live^2)*(1-deaths_perinatal/births_live)) if rate_perinatal != .

*Make area numerical
encode area, gen(area_numeric)
order area_numeric, first
drop area area_code
rename area_numeric area

order imd, a(area)

*Add in postnatal mortality
gen deaths_postnatal = deaths_infant-deaths_neonatal, a(deaths_perinatal)
gen rate_postnatal = deaths_postnatal*1000/births_live if deaths_postnatal != 0, a(rate_perinatal)
gen rate_postnatal_se = 1000*sqrt((deaths_postnatal/births_live^2)*(1-deaths_postnatal/births_live)) if rate_postnatal != .

save "$cd_data\analysis.dta", replace
}


*Part II: Analysis
{
*I want to find out: 
*i) how the average mortality rates are changing over time, including inflection point
*ii) whether the *trend* for each area's mortality rates change depending on the IMD, including inflection point
*iii) within IMD groups, how the mortality rates change over time, including inflection point

*i) average mortality
{
use "$cd_data\analysis.dta", clear
local n = _N
local obs = `n' + 8
set obs `obs'

local k = `n'+1
forvalues i = 2010/2017 {
	qui replace year = `i' in `k'
	
	foreach var of varlist rate_infant-rate_postnatal {
		qui metan `var' `var'_se if year == `i', random nograph
		qui replace `var' = r(ES) in `k'
		qui replace `var'_se = r(seES) in `k'
	}
	
	local k = `k'+1
}

foreach var of varlist rate_infant-rate_postnatal {
	gen `var'_l95 = `var'-1.96*`var'_se
	gen `var'_u95 = `var'+1.96*`var'_se
}

foreach var of varlist rate_infant-rate_postnatal {
	twoway line `var' year if area == . || rcap `var'_u95 `var'_l95 year if area == ., name(`var', replace) legend(order(1))
	local x = subinstr("`var'","rate_","",.)
	graph export "$cd_graphs\Average mortality (`x').png", as(png) width(1200) replace
}

graph combine rate_infant rate_neonatal rate_perinatal rate_postnatal
graph export "$cd_graphs\Average mortality.png", as(png) width(1200) replace

*Does the trend line change between 2010-2013 versus 2013-2017?

local n = _N
local obs = `n' + 3
set obs `obs'
local k1 = `n'+1
local k2 = `n'+2
local k3 = `n'+3

foreach var of varlist rate_infant-rate_postnatal {
	vwls `var' year if area == ., sd(`var'_se)
	qui replace `var' = _b[year] in `k1'
	qui replace `var'_se = _se[year] in `k1'
	
	vwls `var' year if area == . & year <= 2013, sd(`var'_se)
	local b1 = _b[year]
	local se1 = _se[year]
	vwls `var' year if area == . & year >= 2013, sd(`var'_se)
	local b2 = _b[year]
	local se2 = _se[year]
	
	vwls `var' year if area == . & year <= 2014, sd(`var'_se)
	local b1 = _b[year]
	local se1 = _se[year]
	vwls `var' year if area == . & year >= 2014, sd(`var'_se)
	local b2 = _b[year]
	local se2 = _se[year]
	qui replace `var' = `b1' in `k2'
	qui replace `var'_se = `se1' in `k2'
	qui replace `var' = `b2' in `k3'
	qui replace `var'_se = `se2' in `k3'
}

*Table 1
keep if area == .
tostring year, replace
replace year = "Trend: 2010-2017" in 9
replace year = "Trend: 2010-2014" in 10
replace year = "Trend: 2014-2017" in 11

foreach var of varlist rate_infant-rate_postnatal {
	replace `var'_l95 = `var'-1.96*`var'_se if `var'_l95 == .
	replace `var'_u95 = `var'+1.96*`var'_se if `var'_u95 == .
	tostring `var', replace force
	tostring `var'_l95, replace force
	tostring `var'_u95, replace force
}

foreach var of varlist rate_infant-rate_postnatal *95 {
	replace `var' = subinstr(`var',"-.","-0.",.)
	replace `var' = subinstr(`var',".","0.",.) if substr(`var',1,1) == "."
	replace `var' = substr(`var',1,5) if substr(`var',1,1) != "-"
	replace `var' = substr(`var',1,6) if substr(`var',1,1) == "-"
	replace `var' = substr(`var',1,4) in 1/8
}

gen a = " ("
gen b = " to "
gen c = ")"

foreach var of varlist rate_infant-rate_postnatal {
	egen `var'_x = concat(`var' a `var'_l95 b `var'_u95 c)	
}

keep year *_x

foreach var of varlist *_x {
	local a = subinstr("`var'","_x","",.)
	rename `var' `a'
}


}
*Results: 
*It looks like infant & neonatal mortality rates were lowest in 2013/2014, then rose again
*The rate has continuously dropped for perinatal and postnatal mortality rates, however


*ii) trend of mortality by IMD
{
use "$cd_data\analysis.dta", clear

*Estimate the trend for all areas, then see if that trend differs by IMD

foreach var of varlist rate_infant-rate_postnatal {
	gen `var'_vwls = .
	gen `var'_vwls_se = .
	gen `var'_vwls_2010 = .
	gen `var'_vwls_2010_se = .
	gen `var'_vwls_2014 = .
	gen `var'_vwls_2014_se = .
	forvalues i = 1/324 {
		qui su `var' if area == `i'
		if r(N) >= 2 {
			qui vwls `var' year if area == `i', sd(`var'_se)
			qui replace `var'_vwls = _b[year] if area == `i'
			qui replace `var'_vwls_se = _se[year] if area == `i'
		}
		qui su `var' if area == `i' & year <= 2014
		if r(N) >= 2 {
			qui vwls `var' year if area == `i' & year <= 2014, sd(`var'_se)
			qui replace `var'_vwls_2010 = _b[year] if area == `i'
			qui replace `var'_vwls_2010_se = _se[year] if area == `i'
		}
		qui su `var' if area == `i' & year >= 2014
		if r(N) >= 2 {
			qui vwls `var' year if area == `i' & year >= 2014, sd(`var'_se)
			qui replace `var'_vwls_2014 = _b[year] if area == `i'
			qui replace `var'_vwls_2014_se = _se[year] if area == `i'
		}
	}
}

duplicates drop area, force

local n = _N
local obs = `n' + 3
set obs `obs'
local k1 = `n'+1
local k2 = `n'+2
local k3 = `n'+3
tostring year, replace
replace year = "IMD association with trend (per 10 units of IMD): 2010-2017" in 325
replace year = "IMD association with trend (per 10 units of IMD): 2010-2014" in 326
replace year = "IMD association with trend (per 10 units of IMD): 2014-2017" in 327

*Overall trends
foreach var of varlist rate_infant_vwls rate_neonatal_vwls rate_perinatal_vwls rate_postnatal_vwls {
	vwls `var' imd, sd(`var'_se)
	replace `var' = _b[imd] in `k1'
	replace `var'_se = _se[imd] in `k1'
}

*2010-2014 and 2014-2017 trends
foreach var of varlist rate_infant_vwls_2010 rate_neonatal_vwls_2010 rate_perinatal_vwls_2010 rate_postnatal_vwls_2010 rate_infant_vwls_2014 rate_neonatal_vwls_2014 rate_perinatal_vwls_2014 rate_postnatal_vwls_2014 {
	vwls `var' imd, sd(`var'_se)
	replace `var' = _b[imd] in `k2'/`k3'
	replace `var'_se = _se[imd] in `k2'/`k3'
}

foreach var of varlist rate_infant_vwls rate_neonatal_vwls rate_perinatal_vwls rate_postnatal_vwls {
	replace `var' = `var'_2010 in `k2'
	replace `var'_se = `var'_2010_se in `k2'
	replace `var' = `var'_2014 in `k3'
	replace `var'_se = `var'_2014_se in `k3'
}

*Table 2
keep in 325/327

keep year rate_infant_vwls rate_neonatal_vwls rate_perinatal_vwls rate_postnatal_vwls *_vwls_se

foreach var of varlist rate_infant_vwls rate_neonatal_vwls rate_perinatal_vwls rate_postnatal_vwls {
	replace `var' = `var'*10
	replace `var'_se = `var'_se*10
	gen `var'_l95 = `var'-1.96*`var'_se 
	gen `var'_u95 = `var'+1.96*`var'_se 
	tostring `var', replace force
	tostring `var'_l95, replace force
	tostring `var'_u95, replace force
}

foreach var of varlist rate_infant_vwls rate_neonatal_vwls rate_perinatal_vwls rate_postnatal_vwls *95 {
	replace `var' = subinstr(`var',"-.","-0.",.)
	replace `var' = subinstr(`var',".","0.",.) if substr(`var',1,1) == "."
	replace `var' = substr(`var',1,5) if substr(`var',1,1) != "-"
	replace `var' = substr(`var',1,6) if substr(`var',1,1) == "-"
}

gen a = " ("
gen b = " to "
gen c = ")"

foreach var of varlist rate_infant_vwls rate_neonatal_vwls rate_perinatal_vwls rate_postnatal_vwls {
	egen `var'_x = concat(`var' a `var'_l95 b `var'_u95 c)	
}

keep year *_x

foreach var of varlist *_x {
	local a = subinstr("`var'","_vwls_x","",.)
	rename `var' `a'
}


}
*Results:
*No consistent evidence of an association between the linear trend of infant, neonatal or perinatal mortality and IMD
*Absolutely no evidence of IMD driving trends up in 2014-2017 or 2015-2017
*The trend across IMD is always to make the trend more negative, except neonatal 2015-2017, where it is positive, but P = 0.934


*iii) average mortality by IMD group
{
use "$cd_data\analysis.dta", clear

*Group into quintiles of IMD
xtile imd_cat = imd, nq(5)

*Do analysis i) within each IMD quintile
local n = _N
local obs = `n' + 40
set obs `obs'

local k = `n'+1
forvalues i = 2010/2017 {
	forvalues imd = 1/5 {
		qui replace year = `i' in `k'
		qui replace imd_cat = `imd' in `k'
		
		foreach var of varlist rate_infant-rate_postnatal {
			qui metan `var' `var'_se if year == `i' & imd_cat == `imd', random nograph
			qui replace `var' = r(ES) in `k'
			qui replace `var'_se = r(seES) in `k'
		}
	
		local k = `k'+1
	}
}

keep if area == .

foreach var of varlist rate_infant-rate_postnatal {
	gen `var'_l95 = `var'-1.96*`var'_se
	gen `var'_u95 = `var'+1.96*`var'_se
}

*Table 3.1
preserve

keep year imd_cat rate_infant-rate_postnatal *95 
sort imd_cat year
order imd_cat, first

foreach var of varlist rate_infant-rate_postnatal *95 {
	tostring `var', replace force
	replace `var' = subinstr(`var',"-.","-0.",.)
	replace `var' = subinstr(`var',".","0.",.) if substr(`var',1,1) == "."
	replace `var' = substr(`var',1,4)
}

gen a = " ("
gen b = " to "
gen c = ")"

foreach var of varlist rate_infant-rate_postnatal {
	egen `var'_x = concat(`var' a `var'_l95 b `var'_u95 c)	
}

keep imd_cat year *_x

foreach var of varlist *_x {
	local a = subinstr("`var'","_x","",.)
	rename `var' `a'
}

tostring year, replace

save "$cd_tables\table 3_1.dta", replace

restore


/* ONLY RUN ON STATA 15
foreach var of varlist rate_infant-rate_postnatal {
	twoway line `var' year if imd_cat == 1 || ///
	line `var' year if imd_cat == 2 || ///
	line `var' year if imd_cat == 3 || ///
	line `var' year if imd_cat == 4 || ///
	line `var' year if imd_cat == 5 || ///
	rarea `var'_l95 `var'_u95 year if imd_cat == 1, color(%20) || ///
	rarea `var'_l95 `var'_u95 year if imd_cat == 2, color(%20) || ///
	rarea `var'_l95 `var'_u95 year if imd_cat == 3, color(%20) || ///
	rarea `var'_l95 `var'_u95 year if imd_cat == 4, color(%20) || ///
	rarea `var'_l95 `var'_u95 year if imd_cat == 5, color(%20) || ///
	, name(`var', replace) legend(label(1 "IMD group 1") ///
	label(2 "IMD group 2") /// 
	label(3 "IMD group 3") /// 
	label(4 "IMD group 4") ///
	label(5 "IMD group 5")) ///
	legend(order(1 2 3 4 5))
	local x = subinstr("`var'","rate_","",.)
	graph export "$cd_graphs\IMD-specific mortality (`x').png", as(png) width(1200) replace
}
*/

foreach var of varlist rate_infant-rate_postnatal {
	gen `var'_vwls = .
	gen `var'_vwls_se = .
	gen `var'_vwls_2010 = .
	gen `var'_vwls_2010_se = .
	gen `var'_vwls_2014 = .
	gen `var'_vwls_2014_se = .
	forvalues imd = 1/5 {
		vwls `var' year if imd_cat == `imd', sd(`var'_se)
		qui replace `var'_vwls = _b[year] if imd_cat == `imd'
		qui replace `var'_vwls_se = _se[year] if imd_cat == `imd'
		
		qui vwls `var' year if imd_cat == `imd' & year <=2014, sd(`var'_se)
		qui replace `var'_vwls_2010 = _b[year] if imd_cat == `imd'
		qui replace `var'_vwls_2010_se = _se[year] if imd_cat == `imd'
		
		qui vwls `var' year if imd_cat == `imd' & year >=2014, sd(`var'_se)
		qui replace `var'_vwls_2014 = _b[year] if imd_cat == `imd'
		qui replace `var'_vwls_2014_se = _se[year] if imd_cat == `imd'
	}
}

keep *vwls* imd_cat
duplicates drop

gen year = "Trend: 2010-2017"
set obs 15
local i = 6

forvalues imd = 1/5 {
	replace imd_cat = `imd' in `i'
	foreach var of varlist rate_infant_vwls rate_neonatal_vwls rate_perinatal_vwls rate_postnatal_vwls {
		local j = `i'
		replace `var' = `var'_2010[`imd'] in `j'
		replace `var'_se = `var'_2010_se[`imd'] in `j'
		replace year = "Trend: 2010-2014" in `j'
		
		local j = `j'+1
		replace `var' = `var'_2014[`imd'] in `j'
		replace `var'_se = `var'_2014_se[`imd'] in `j'
		replace year = "Trend: 2014-2017" in `j'
	}
	local i = `i'+1
	replace imd_cat = `imd' in `i'
	local i = `i'+1
}

order year, first
gen x = 1 if year == "Trend: 2010-2017"
replace x = 2 if year == "Trend: 2010-2014"
replace x = 3 if x == .
sort imd_cat x year		
drop *201*

foreach var of varlist *_vwls* {
	local a = subinstr("`var'","_vwls","",.)
	rename `var' `a'
}

*Table 3.2
preserve

foreach var of varlist rate_infant rate_neonatal rate_perinatal rate_postnatal {
	gen `var'_l95 = `var'-1.96*`var'_se
	gen `var'_u95 = `var'+1.96*`var'_se
}

foreach var of varlist rate_infant rate_neonatal rate_perinatal rate_postnatal *95 {
	tostring `var', replace force
	replace `var' = subinstr(`var',"-.","-0.",.)
	replace `var' = subinstr(`var',".","0.",.) if substr(`var',1,1) == "."
	replace `var' = substr(`var',1,5) if substr(`var',1,1) != "-"
	replace `var' = substr(`var',1,6) if substr(`var',1,1) == "-"
}

gen a = " ("
gen b = " to "
gen c = ")"

foreach var of varlist rate_infant rate_neonatal rate_perinatal rate_postnatal {
	egen `var'_x = concat(`var' a `var'_l95 b `var'_u95 c)	
}

keep imd_cat year *_x

foreach var of varlist *_x {
	local a = subinstr("`var'","_x","",.)
	rename `var' `a'
}

append using "$cd_tables\table 3_1.dta"

set obs 60
replace year = "IMD quintile = 1" in 56
replace year = "IMD quintile = 2" in 57
replace year = "IMD quintile = 3" in 58
replace year = "IMD quintile = 4" in 59
replace year = "IMD quintile = 5" in 60
replace imd_cat = 1 in 56
replace imd_cat = 2 in 57
replace imd_cat = 3 in 58
replace imd_cat = 4 in 59
replace imd_cat = 5 in 60

gen x = 1 if year == "Trend: 2010-2017"
replace x = 2 if year == "Trend: 2010-2014"
replace x = 3 if year == "Trend: 2014-2017"
replace x = -1 if x == .
replace x = -2 if strpos(year,"IMD") > 0
sort imd_cat x year	
drop x

drop imd_cat

save "$cd_tables\table 3.dta", replace

restore

foreach var of varlist rate_infant rate_neonatal rate_perinatal rate_postnatal {
	forvalues i = 1/3 {
		qui vwls `var' i.imd_cat if x == `i', sd(`var'_se)
		matrix a = e(b)
		matrix b = e(V)
		forvalues k = 1/5 {
			qui replace `var' = a[1,`k'] if x == `i' & imd_cat == `k'
			qui replace `var'_se = sqrt(b[`k',`k']) if x == `i' & imd_cat == `k'
		}
	}
}

sort x imd_cat
drop x

foreach var of varlist rate_infant rate_neonatal rate_perinatal rate_postnatal {
	gen `var'_l95 = `var'-1.96*`var'_se
	gen `var'_u95 = `var'+1.96*`var'_se
}

foreach var of varlist rate_infant rate_neonatal rate_perinatal rate_postnatal *95 {
	tostring `var', replace force
	replace `var' = subinstr(`var',"-.","-0.",.)
	replace `var' = subinstr(`var',".","0.",.) if substr(`var',1,1) == "."
	replace `var' = substr(`var',1,5) if substr(`var',1,1) != "-"
	replace `var' = substr(`var',1,6) if substr(`var',1,1) == "-"
}

gen a = " ("
gen b = " to "
gen c = ")"

foreach var of varlist rate_infant rate_neonatal rate_perinatal rate_postnatal {
	egen `var'_x = concat(`var' a `var'_l95 b `var'_u95 c)	
}

keep imd_cat year *_x

foreach var of varlist *_x {
	replace `var' = "Reference" if `var' == "0 (0 to 0)"
	local a = subinstr("`var'","_x","",.)
	rename `var' `a'
}

set obs 18
replace year = "Trend: 2010-2017" in 16
replace year = "Trend: 2010-2014" in 17
replace year = "Trend: 2014-2017" in 18
replace imd_cat = 0 in 16/18

gen x = 1 if year == "Trend: 2010-2017"
replace x = 2 if year == "Trend: 2010-2014"
replace x = 3 if year == "Trend: 2014-2017"

sort x imd_cat	
drop x
tostring imd_cat, replace
replace imd_cat = year if imd_cat == "0"
drop year

save "$cd_tables\table 4.dta", replace 

}

*Results:
*Clear stratification by IMD for perinatal and infant mortality rates (deprived = bad)
*Neonatal mortality worst for top and bottom IMD groups, for some reason
*Difficult to see any consistent increase in any type of mortality in any particular IMD group
*It's actually the least deprived groups that seem to be increasing in mortality from 2014, not the least deprived

*Again, I don't see anything to suggest IMD is driving an increase in any mortality
*The most deprived quintile makes the trend lines more negative than others in 2014-2017
*The same is broadly true for 2013-2017
*Importantly, the P values are all well above 0.1

*Conclusion: higher IMD does not predict a rising rate of infant, neonatal or perinatal mortality between 2013-2017, compared with lower IMD
*Higher IMD does predict a higher rate of all mortalities, 

}


*Part III: Summary (no area)
{

use "$cd_raw_data\summary_no_area.dta", clear

gen deaths_stillbirth_1 = deaths_stillbirth + deaths_0_1
gen deaths_1_360 = deaths_0_360-deaths_0_1

foreach var of varlist death* {
	local a = subinstr("`var'","deaths_","",.)
	
	if "`var'" == "deaths_stillbirth" | "`var'" == "deaths_all" | "`var'" == "deaths_stillbirth_1" {
		qui gen rate_`a' = `var'*1000/births_total
		qui gen rate_`a'_se = 1000*sqrt((`var'/births_total^2)*(1-`var'/births_total))
	}
	else {
		qui gen rate_`a' = `var'*1000/births_live
		qui gen rate_`a'_se = 1000*sqrt((`var'/births_live^2)*(1-`var'/births_live))
	}
}

order *_se, last

foreach var of varlist rate_stillbirth-rate_1_360 {
	gen `var'_l95 = `var'-1.96*`var'_se
	gen `var'_u95 = `var'+1.96*`var'_se
}

foreach var of varlist rate_stillbirth-rate_1_360 {
	twoway line `var' year || rcap `var'_u95 `var'_l95 year, name(`var', replace) legend(order(1))
	local x = subinstr("`var'","rate_","",.)
	graph export "$cd_graphs\Average mortality (extra analysis for `x').png", as(png) width(1200) replace
}

*Look to see if the change in stillbirth rate is associated with the change in <1 day mortality rate
corr rate_0_1 rate_stillbirth if year <= 2014 
corr rate_0_1 rate_stillbirth if year >= 2014 

vwls rate_0_1 rate_stillbirth if year <= 2014, sd(rate_0_1_se)
vwls rate_0_1 rate_stillbirth if year >= 2014, sd(rate_0_1_se)

*Can I do this in a graphical way?
*Plot something against year
*Difference in rate seems like the obvious choice

gen rate_diff_stillbirth_0_1 = rate_stillbirth - rate_0_1

*Difference in two estimates, use Fisher's formula
gen rate_diff_stillbirth_0_1_se = sqrt(rate_stillbirth_se^2 + rate_0_1_se^2)
gen rate_diff_stillbirth_0_1_l95 = rate_diff_stillbirth_0_1 - 1.96*rate_diff_stillbirth_0_1_se
gen rate_diff_stillbirth_0_1_u95 = rate_diff_stillbirth_0_1 + 1.96*rate_diff_stillbirth_0_1_se

twoway line rate_diff_stillbirth_0_1 year || ///
rcap rate_diff_stillbirth_0_1_l95 rate_diff_stillbirth_0_1_u95 year, ///
name(rate_diff_stillbirth_0_1_add, replace) ///
legend(order(1))
graph export "$cd_graphs\Stillbirth versus mortality under 1day (additive).png", as(png) width(1200) replace

*Also try relative proportions
gen rate_prop_stillbirth_0_1 = rate_stillbirth / rate_0_1

*Difference in two estimates, use Fisher's formula
gen rate_prop_stillbirth_0_1_se = sqrt(rate_stillbirth_se^2 + rate_0_1_se^2)
gen rate_prop_stillbirth_0_1_l95 = rate_prop_stillbirth_0_1 - 1.96*rate_prop_stillbirth_0_1_se
gen rate_prop_stillbirth_0_1_u95 = rate_prop_stillbirth_0_1 + 1.96*rate_prop_stillbirth_0_1_se

twoway line rate_prop_stillbirth_0_1 year || /// 
rcap rate_prop_stillbirth_0_1_l95 rate_prop_stillbirth_0_1_u95 year, /// 
name(rate_prop_stillbirth_0_1, replace) ///
legend(order(1))
graph export "$cd_graphs\Stillbirth versus mortality under 1day (multiplicative).png", as(png) width(1200) replace

}




















