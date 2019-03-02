* create episode-level data starting for fy 2013-2015 where fy is a year ending June

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults

cd `path'

*construct effort measures using the visit-level data
use HHeffort_visit, clear

*instead of dropping, recode to missing if visit length < p1 or > p99
drop if lov==0
replace lov = lov *60
sum lov, de
loc p1 = `r(p1)'
loc p99 = `r(p99)'
gen x = lov < `p1' | lov > `p99'
bys epiid: egen outlierLOV = max(x)
drop if outlierLOV==1
drop x outlierLOV

*drop visits if visittime is missing
drop if visittime==.

duplicates tag epiid visitdate discipline, gen(dup)
tab dup
drop dup

*merge with summary index of costs
capture drop visit_travel_cost visit_tot_cost payrate
merge m:1 epiid visitdate discipline using resource_index, keep(1 3) nogen

*total # visits during episode , visit length
gen tnv = 1
gen tnvsn = discipline=="SN"
gen lovsn = lov if discipline=="SN"

*during the entire episode
preserve
collapse (sum) tnv tnvsn vtc_tr_pay visit_travel_cost visit_tot_cost payrate (mean) lov lovsn, by(epiid epilength)
assert vtc_tr_pay >= (visit_travel_cost+ visit_tot_cost+ payrate-0.00001) & vtc_tr_pay <= (visit_travel_cost+ visit_tot_cost+ payrate+0.00001)

tempfile effort1
save `effort1'
restore

*by whether the episode is first week or the rest
preserve
collapse (sum) tnv tnvsn vtc_tr_pay visit_travel_cost visit_tot_cost payrate (mean) lov lovsn, by(epiid firstwk)
foreach v of varlist tnv tnvsn vtc_tr_pay visit_travel_cost visit_tot_cost payrate lov lovsn {
  renam `v' `v'_1stwk
}
reshape wide *_1stwk, i(epiid) j(firstwk)
tempfile effort2
save `effort2'
restore

*--------------------------
*restrict to episodes without a visit on the day of readmission (already done in crHHeffort_visit.do, so skip here), who had a hospital stay before HH, who didn't have AIDS or blooad anemia, < age 65
use HHeffort_visit, clear

*restrict to patients who had a hospital stay before HH
keep if facility=="Hosp"

*drop AIDS & blood anemia
drop if ynel16==1 | ynel25==1

drop if age < 65

*5-year age bins
egen age5yr = cut(age), at(65,70,75,80,85,90,95)
replace age5yr = 95 if age > 94
assert age5yr!=.

loc riskhosp riskhosp_fall riskhosp_manyhos riskhosp_mental riskhosp_ge5 riskhosp_oth
loc priorcond priorcond_dis priorcond_impd priorcond_cath priorcond_pain priorcond_mem priorcond_inc
loc hrfactor hrfactor_alco hrfactor_drug hrfactor_smoke hrfactor_obese
loc overallst overallst_vbad overallst_bad overallst_tem
loc ins ma_visit ma_epi dual
loc ages age5yr
loc demog `ages' female white noassist livealone `ins'
loc comorbid ynch* `overallst' `hrfactor' `priorcond'

loc chars `comorbid' `riskhosp' `demog'
loc vars epiid provider_hosp socdate2 offid_nu clientid fy inpat_dcd ami hf pn copd stroke cardioresp hashosp* days2hosp firsthospdate age
keep `chars' `vars'
duplicates drop

* create a severity measure: sum of risk of hospitalization categories at baseline
capture drop riskhosp
egen riskhosp = rowtotal(riskhosp_* hrfactor_* priorcond_*)
tab riskhosp

*create indicator for whether episode had 4 HRRP conditions
gen hrrpcond = ami==1 | hf==1 | pn==1

*Are there patients having multiple conditions? ~1000 pats have 2-4 conditions
egen ss = rowtotal(ami hf pn copd stroke)
tab ss
drop ss

*Indicator for whether the HH episode started within 1 day from hospital discharge
gen time2hh = socdate2 - inpat_dcdate_e
drop if time2hh < 0 | time2hh > 30
tab time2hh
*all the episodes started HH within 14 days from hospital discharge
gen startHH_1day = time2hh <=1 if time2hh!=.

*merge with efforts data at the episode level
merge 1:1 epiid using `effort1', keep(3) nogen
merge 1:1 epiid using `effort2', keep(3) nogen

*create frequency of visits = # visits / episode length
foreach v of varlist tnv tnvsn {
  gen freq_`v' = `v'/epilength
}
gen epilength_1stwk0 = epilength - 7
replace epilength_1stwk0 = . if epilength <= 7
foreach v of varlist tnv_1stwk0 tnvsn_1stwk0 {
  gen freq_`v' = `v'/epilength_1stwk0
}
gen epilength_1stwk1 = epilength if epilength <= 7
replace epilength_1stwk1 = 7 if epilength > 7
assert epilength_1stwk1!=.
foreach v of varlist tnv_1stwk1 tnvsn_1stwk1 {
  gen freq_`v' = `v'/epilength_1stwk1
}

*drop if patient had 0 nurse visits during the episode
drop if tnvsn==0

*drop episodes if the total summary cost ==0
drop if vtc_tr_pay==0

gen ma = ma_visit==1 | ma_epi==1
gen tm = 1-ma

egen hrrpcond_count = rowtotal(ami hf pn)
tab hrrpcond_count
drop if hrrpcond_count > 1

*------------------
*merge with data on other covariates
rename socdate visitdate_e
merge m:1 offid_nu visitdate_e using allepi, keep(1 3) nogen
merge m:1 offid_nu visitdate_e using nw_active_worker, keep(1 3) nogen

rename provider_hosp prvdr_num
destring prvdr_num, replace
merge m:1 prvdr_num fy using hospchars, keep(3) nogen

*-------------
*merge by patient's referring hospital and office, the penalty pressure data
merge m:1 offid_nu prvdr_num using HRRPpnlty_pressure_hj_2012-v2, keep(3) nogen keepusing(pnltprs* penrate* shref_hj)

*create a single penalty pressure variable containing pressure for each condition
gen pnltprs_c = .
gen pnltprs_hosp_c = .
foreach d in "ami" "hf" "pn" {
  replace pnltprs_c = pnltprs_`d' if `d'==1
  replace pnltprs_hosp_c = pnltprs_hosp_`d' if `d'==1
}
replace pnltprs_c = 0 if hrrpcond==0
replace pnltprs_hosp_c = 0 if hrrpcond==0
assert pnltprs_c!=.

*2 obs have pnltprs_hosp_c = .
drop if pnltprs_hosp_c==.

loc uami "AMI"
loc uhf "HF"
loc upn "PN"
foreach d in "ami" "hf" "pn" {
  capture drop pnltprs_c_X_`d'
  gen pnltprs_c_X_`d' = pnltprs_c *`d'
  gen pnltprs_hosp_c_X_`d' = pnltprs_hosp_c *`d'

  lab var pnltprs_c_X_`d' "Condition-specific penalty salience X `u`d''"
  lab var pnltprs_hosp_c_X_`d' "Alternative penalty salience (HH firm's share of hospital patients) X `u`d''"
  lab var `d' "Indicator for `u`d''"
}

* create log-transformed outcome variables & label them
foreach v of varlist vtc_tr_pay* visit_travel_cost* visit_tot_cost* payrate* epilength* lov* {
  gen ln`v' = ln(`v'+1)
}

loc l_lnlov "Ln Visit length (min)"
loc l_lnlovsn "Ln Nurse visit length (min)"
loc l_freq_tnv "Frequency of visits"
loc l_freq_tnvsn "Frequency of nurse visits"
loc l_lnvtc_tr_pay "Ln Total cost index ($)"
loc l_lnvisit_tot_cost "Ln Visit cost ($)"
loc l_lnpayrate "Ln Personnel cost ($)"
loc l_lnvisit_travel_cost "Ln Travel cost ($)"
loc l_hashosp30 "30-day readmission"

lab var startHH_1day "Prob. start HH within 1 day from hosp discharge"

foreach v in "lnlov" "lnlovsn" "freq_tnv" "freq_tnvsn" "lnvtc_tr_pay" "lnvisit_tot_cost" "lnpayrate" "lnvisit_travel_cost" "hashosp30" {
  lab var `v' "`l_`v''"
  lab var `v'_1stwk1 "`l_`v'' in the first week"
  lab var `v'_1stwk0 "`l_`v'' beyond the first week"
}

loc outcome lnlov lnlov_1stwk1 lnlovsn lnlovsn_1stwk1 freq_tnv freq_tnv_1stwk1 freq_tnvsn freq_tnvsn_1stwk1 startHH_1day lnvtc_tr_pay lnvtc_tr_pay_1stwk1 lnvisit_tot_cost lnvisit_tot_cost_1stwk1 lnpayrate lnpayrate_1stwk1 lnvisit_travel_cost lnvisit_travel_cost_1stwk1 hashosp30 hashosp30_1stwk1
des `outcome'


compress
save epilvl_rehosp_smpl, replace
