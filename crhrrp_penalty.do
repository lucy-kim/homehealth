*crhrrp_penalty.do
* create readmissions penalty data for each hospital, by condition & overall

loc path /home/hcmg/kunhee/Labor/Bayada_data/
cd `path'

*first, import the raw hospital-level overall readmission penalty rate data into Stata
insheet using HRRP/readmissions-year-3.csv, comma names clear
gen str6 prvdr_num = string(providerid, "%06.0f")
drop providerid
count
*3383 hospitals in total


* recode "not assessed" values in the penalty rate to missing value before destringing
tab fy2013readmis if regexm(fy2013readmis, "[^0-9 .]")
foreach v of varlist fy201?readmis* {
  replace `v' = "" if `v'=="Not Assessed"
}
destring fy201?readmis* , replace ignore("%")

* drop hospitals not subject to penalty, i.e. penalty = missing value
forval y=2013/2015 {
  *drop if fy`y'readmis==.
  rename fy`y'readmis penalty`y'
}
drop address-county

*reshape long
reshape long penalty , i(prvdr_num name) j(fyear)

tempfile penalty
save `penalty'

*-----------------
* process the raw hospital-level condition-specific readmission penalty rate from Atul

foreach d in "ami" "chf" "pneum" {
  insheet using HRRP/hrrp_penalty_`d'.csv, comma names clear
  keep penaltyrate2012 v16
  drop if _n < 2
  rename penaltyrate2012 prvdr_num
  rename v16 penalty2012_`d'
  destring penalty2012_`d', replace
  compress
  tempfile penalty2012_`d'
  save `penalty2012_`d''
}

use `penalty2012_ami', clear
foreach d in "chf" "pneum" {
  merge 1:1 prvdr_num using `penalty2012_`d'', nogen
}
egen totpenalty2012 = rowtotal(penalty2012_*)

merge 1:m prvdr_num using `penalty', keep(2 3) nogen

compress
save hrrp_penalty, replace
