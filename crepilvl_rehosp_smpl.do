* create episode-level data starting for fy 2013-2015 where fy is a year ending June

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults

cd `path'

*construct effort measures using the visit-level data
use HHeffort_visit, clear

assert discipline!=""

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
*restrict to episodes without a visit on the day of readmission, who had a hospital stay before HH, who didn't have AIDS or blooad anemia, < age 65
use HHeffort_visit, clear

keep if facility=="Hosp"

*drop AIDS & blood anemia
drop if ynel16==1 | ynel25==1

drop if age < 65

*drop episodes that had a visit on the day of readmission
sort epiid visitdate
gen hadvisit_onra = firsthospdate == visitdate & hashosp==1
bys epiid: egen max = max(hadvisit_onra)
drop if max==1
drop max hadvisit_onra

*5-year age bins
egen age5yr = cut(age), at(65,70,75,80,85,90,95)
replace age5yr = 95 if age > 94
assert age5yr!=.

*create episode level 30-day hospital readmission indicator
*get 30-day readmission indicator
gen days2hosp = firsthospdate - inpat_dcd
gen hashosp30 = hashosp * (days2hosp <= 30)

lab var hashosp "Any hospital readmission indicator"
lab var hashosp30 "30-day hospital readmission indicator"

loc riskhosp riskhosp_fall riskhosp_manyhos riskhosp_mental riskhosp_ge5 riskhosp_oth
loc priorcond priorcond_dis priorcond_impd priorcond_cath priorcond_pain priorcond_mem priorcond_inc
loc hrfactor hrfactor_alco hrfactor_drug hrfactor_smoke hrfactor_obese
loc overallst overallst_vbad overallst_bad overallst_tem
loc ins ma_visit ma_epi dual
loc ages age5yr
loc demog `ages' female white noassist livealone `ins'
loc comorbid ynch* `overallst' `hrfactor' `priorcond'

loc chars `comorbid' `riskhosp' `demog'
loc vars epiid provider_hosp socdate2 offid_nu clientid fy inpat_dcd ami hf pneu copd stroke hashosp* days2hosp age
keep `chars' `vars'
duplicates drop

rename pneu pn
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

*merge by patient's referring hospital and office, the penalty pressure data
rename provider_hosp prvdr_num
destring prvdr_num, replace
merge m:1 offid_nu prvdr_num using HRRPpnlty_pressure_hj_2012, keep(3) nogen keepusing(pnltprs* z_pnltprs*)

gen ma = ma_visit==1 | ma_epi==1
gen tm = 1-ma

*create a single penalty pressure variable containing pressure for each condition
gen pnltprs_c = .
foreach d in "ami" "hf" "pn" {
  replace pnltprs_c = pnltprs_`d' if `d'==1
}
replace pnltprs_c = 0 if hrrpcond==0
assert pnltprs_c!=.

*drop episodes if the total summary cost ==0
drop if vtc_tr_pay==0

egen hrrpcond_count = rowtotal(ami hf pn)
tab hrrpcond_count
drop if hrrpcond_count > 1

tempfile an2
save `an2'

*-----------
gen pp = pnltprs_c > 0
assert pp!=.
tab pp, summarize(pnltprs_c)
tab hrrpcond pp


*get total number of episodes going on, # active workers in the office on each day
use epi_visit, clear
keep offid_nu visitdate epiid payrollno
duplicates drop
keep if visitdate <= mdy(7,1,2015)

preserve
drop payrollno
duplicates drop
assert epiid!=.
gen allepi = 1
collapse (sum) allepi, by(offid_nu visitdate)
*subtract 1 to not count the episode that starts on that day
replace allepi = allepi - 1
gen lnallepi = ln(allepi)
tempfile allepi
save `allepi'
restore

preserve
drop epiid
duplicates drop
assert payrollno!=""
gen nw_active_worker = 1
collapse (sum) nw_active_worker, by(offid_nu visitdate)
gen lnnw_active_worker = ln(nw_active_worker)
tempfile nw_active_worker
save `nw_active_worker'
restore

*get characteristics of the referring hospital
use hosp_chars_cr, clear
keep if fy >=2013 & fy <= 2015
keep provid fy vi_hha teaching urban own_* size beds
rename provid prvdr_num
duplicates drop
tempfile hospchars
save `hospchars'

use `an2', clear
merge m:1 prvdr_num fy using `hospchars', keep(3) nogen

rename socdate visitdate_e
merge m:1 offid_nu visitdate_e using `allepi', keep(1 3) nogen
merge m:1 offid_nu visitdate_e using `nw_active_worker', keep(1 3) nogen

foreach v of varlist vtc_tr_pay* visit_travel_cost* visit_tot_cost* payrate* epilength* lov* {
  gen ln`v' = ln(`v'+1)
}
loc uami "AMI"
loc uhf "HF"
loc upn "PN"
foreach d in "ami" "hf" "pn" {
  capture drop pnltprs_c_X_`d'
  gen pnltprs_c_X_`d' = pnltprs_c *`d'
  gen pnltprs_X_`d' = pnltprs * `d'

  lab var pnltprs_c_X_`d' "Condition-specific penalty salience X `u`d''"
  lab var `d' "Indicator for `u`d''"
  lab var pnltprs_X_`d' "Aggregate penalty salience X `u`d''"
}

compress
save epilvl_rehosp_smpl, replace
