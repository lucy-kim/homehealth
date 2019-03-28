* to interpret the magnitude of our estimates, want to compare the effort levels on the healthiest and sickest people at baseline (2012); get 2012 effort values

loc path /home/hcmg/kunhee/hrrp-home/data/
loc reg /home/hcmg/kunhee/hrrp-home/output
cd `path'

use epilvl_rehosp_smpl, clear

keep if fy==2012

*restrict to targeted patients
keep if hrrpcond==1
drop if stroke | copd
assert stroke==0 & copd==0

*get the sickest & healthiest by looking at the quintile of the sum of risk of hospitalization categories at baseline
capture drop riskhosp
egen riskhosp = rowtotal(riskhosp_* hrfactor_* priorcond_*)
tab riskhosp

sum riskhosp, de
loc p25 = `r(p25)'
loc p75 = `r(p75)'
gen sickest = riskhosp >= `p75'
gen healthiest = riskhosp <= `p25'

keep if sickest==1 | healthiest==1
tab sickest

loc riskhosp riskhosp_fall riskhosp_manyhos riskhosp_mental riskhosp_ge5
loc pat_sickness age female white `riskhosp' hashosp30 hashosp30_1stwk1
loc out1 lov freq_tnv vtc_tr_pay
loc out2 lov_1stwk1 freq_tnv_1stwk1 vtc_tr_pay_1stwk1
loc out3 lov_1stwk0 freq_tnv_1stwk0 vtc_tr_pay_1stwk0

lab var age "Age"
lab var female "Female"
lab var white "White"
lab var riskhosp_fall "Risk for hospitalization: History of 2+ falls"
lab var riskhosp_manyhosp "Risk for hospitalization: 2+ hospitalizations"
lab var riskhosp_mental "Risk for hospitalization: Recent decline in Mental"
lab var riskhosp_ge5med "Risk for hospitalization: Take 5+ medications"
lab var riskhosp_oth "Risk for hospitalization: Other"
lab var hashosp30_1stwk1 "Readmission in the first week"

loc l_epilength "Episode length (days)"
loc l_lov "Visit length (min)"
loc l_lovsn "Nurse visit length (min)"
loc l_freq_tnv "Frequency of visits"
loc l_freq_tnvsn "Frequency of nurse visits"
loc l_startHH_1day "Start HH within 1 day from discharge"
loc l_vtc_tr_pay "Total cost index ($)"
loc l_hashosp30 "P(Readmission) in 30 days"

foreach v of varlist `out1' {
  loc l_`v'_1stwk1 "`l_`v''"
  loc l_`v'_1stwk0 "`l_`v''"
}

forval x=1/3 {
  foreach v of varlist lov freq_tnv vtc_tr_pay {
    capture lab var `v' "`l_`v''"
  }
}

preserve
keep `pat_sickness' sickest `out1' `out2' `out3'
order sickest `pat_sickness' `out1' `out2' `out3'
bys sickest: sum `pat_sickness' `out1' `out2' `out3'
bys sickest: outreg2 using `reg'/effort_compare.xls, replace sum(log) eqkeep(N mean) label
restore
