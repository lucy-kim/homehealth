*Run descriptive stats

loc path /home/hcmg/kunhee/hrrp-home/data/
loc reg /home/hcmg/kunhee/hrrp-home/output
set matsize 11000

cd `path'

loc riskhosp riskhosp_fall riskhosp_manyhos riskhosp_mental riskhosp_ge5 riskhosp_oth
loc priorcond priorcond_dis priorcond_impd priorcond_cath priorcond_pain priorcond_mem priorcond_inc
loc hrfactor hrfactor_alco hrfactor_drug hrfactor_smoke hrfactor_obese
loc overallst overallst_vbad overallst_bad overallst_tem
loc ins dual
loc ages i.age5yr
loc demog `ages' female white noassist livealone `ins'
loc comorbid ynch* `overallst' `hrfactor' `priorcond'
loc hospchars vi_hha teaching urban own_* i.size
loc officechars lnallepi lnnw_active_w

loc sp `riskhosp' `demog' `comorbid' i.fy `officechars' `hospchars'

*---------------------------------
*create the TM patient sample used for main analysis
loc pp1 ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn

use epilvl_rehosp_smpl, clear

drop if fy==2012
keep if tm==1
drop if copd | stroke | (cardioresp & hrrpcond==0)

loc n 1
loc yv lnlov
areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)
keep if e(sample)

tempfile insmpl
save `insmpl'


*--------------------------------------
*Table 1 : summary stats by target vs non-target condition and for all patients
*--------------------------------------
use `insmpl', clear

*count observations in each condition
bys hrrpcond: egen nobs = sum(1)

*create penalty rate by condition covariates
capture drop pr_c
gen pr_c = .
foreach d in "ami" "hf" "pn" {
  replace pr_c = penrate_`d' if `d'==1
}
replace pr_c = 0 if hrrpcond==0
lab var pr_c "Condition-specific penalty rate in 2012"
assert pr_c!=.

*C. patient characteristics

*D & E: office & referring hospital characteristics
tab size
gen small = size==1
gen medium = size==2
gen big = size==3

lab var nobs "Observations"
lab var pnltprs_c "Mean penalty salience"
lab var pr_c "Mean penalty rate"
lab var hashosp30 "30-day readmission rate"
lab var vtc_tr_pay "Total cost index ($ / episode)"
lab var visit_tot_cost "Visit cost"
lab var payrate "Personnel cost"
lab var lov "Visit length (min)"
lab var riskhosp_fall "Risk for hospitalization: History of 2+ falls"
lab var riskhosp_manyhosp "Risk for hospitalization: 2+ hospitalizations"
lab var riskhosp_mental "Risk for hospitalization: Recent decline in Mental"
lab var riskhosp_ge5med "Risk for hospitalization: Take 5+ medications"
lab var riskhosp_oth "Risk for hospitalization: Other"
lab var age "Age"
lab var female "Female"
lab var white "White"
lab var small "Beds 1-100"
lab var medium "Beds 101-300"
lab var big "Beds 301+"
lab var own_fp "For-profit"
lab var own_np "Not for-profit"
lab var urban "Urban"
lab var teaching "Teaching"
lab var vi_hha "Have a hospital-based HHA"

loc mainoutc nobs pr_c pnltprs_c hashosp30
loc costoutc vtc_tr_pay visit_tot_cost payrate
loc effortoutc lov freq_tnv
loc patchars `riskhosp' age female white
loc hospchars small medium big own_np own_fp teaching urban vi_hha
des `mainoutc' `costoutc' `effortoutc' `patchars' `hospchars'

*summary stats by target vs non-target condition
preserve
keep hrrpcond `mainoutc' `costoutc' `effortoutc' `patchars' `hospchars'
order `mainoutc' `costoutc' `effortoutc' `patchars' `hospchars'
bys hrrpcond: outreg2 using `reg'/summstats_bytarget.xls, replace sum(log) eqkeep(N mean) label dec(2)
restore

*for target patients, SD penalty rate & SD penalty salience
sum pr_c pnltprs_c if hrrpcond
* SD(pr_c) = .09
* SD(pnltprs_c) = .043

*summary stats for all patients
preserve
*count observations for all patients
capture drop nobs
egen nobs = sum(1)
keep `mainoutc' `costoutc' `effortoutc' `patchars' `hospchars'
order `mainoutc' `costoutc' `effortoutc' `patchars' `hospchars'
outreg2 using `reg'/summstats_allpat.xls, replace sum(log) eqkeep(N mean) label dec(2)
restore

*--------------------------------------------
*Table A1. Comparison of Medicare population and study sample
*B. for targeted patients, # patients in each target condition & episode length
*--------------------------------------------
*create groupings for AMI, HF, PN, and non-target
use `insmpl', clear
keep if hrrpcond

gen gp = 1 if ami==1
replace gp = 2 if hf
replace gp = 3 if pn
lab define cond 1 "AMI" 2 "HF" 3 "PN", replace
lab val gp cond

*count observations in each condition
bys gp: egen nobs = sum(1)
tab gp

*mean episode length (days), male, white, age
gen male = 1-female
sum epilength male white age

*manually type in the above info in Section B of Table A1

*---------------------------------
*# referring hospitals
use `insmpl', clear
keep prvdr_num
duplicates drop
count
*---------------------------------
*proportion of episodes are 1 week or longer
use `insmpl', clear
keep epiid epilength
gen gteq1wk = epilength >= 7
tab gteq1wk
// 94% have >= 1 week duration
*---------------------------------
*what proportion of hospitals are penalized for at least 1 condition in our sample?
use `insmpl', clear
keep prvdr_num offid_nu shref_hj penrate_ami penrate_hf penrate_pn
duplicates drop
duplicates tag prvdr_num offid_nu, gen(dup)
tab dup
assert dup==0
drop dup

preserve
keep prvdr_num penrate*
duplicates drop
count
sum penrate*
gen penalized = penrate_ami > 0 | penrate_hf > 0 | penrate_pn > 0
tab penalized
restore

assert shref_hj!=.
gen posshare = shref_hj > 0
tab posshare

gen pnltprs = shref_hj * (penrate_ami + penrate_hf + penrate_pn)
gen pressure = pnltprs > 0
tab pressure

*---------------------------------
*what are the top 5 conditions among non-target patients?
use `insmpl', clear
keep epiid hrrpcond clientid socdate_e category
duplicates drop

keep if hrrpcond==0

merge 1:m clientid socdate_e using inpat_dx, keep(1 3) nogen

*keep only the 3 digits before period
split inpat_dx_cor, p(".")

keep inpat_dx_cor1 epiid category
gen i = 1
collapse (sum) freq = i, by(inpat_dx_cor1)
gsort inpat_dx_cor1
egen tot = sum(freq)
gen pct = 100*freq /tot
gen x = sum(pct)
rename x cumulative_pct
gsort -pct
outsheet using `reg'/inpat_dx_icd_ranking.csv, replace names comma

*------------------
*additional summary stats
*Average episode length in days (targeted, non-targeted, overall)
*Average duration between hospital discharge and start of episode (need targeted only)

use `insmpl', clear
lab var epilength "Episode length (days)"
lab var time2hh "Duration between hospital discharge and start of episode"
loc outc epilength time2hh
des `outc'

bys hrrpcond: sum epilength
sum epilength
sum time2hh if hrrpcond

*---------------------------------
*What is the median penalty salience for patients that were discharged from a penalized hospital? In other words, the difference in penalty salience between being discharged by a non-penalized hospital (0) or for a non-targeted condition (0) vs. being discharged by the median penalized hospital. I think this may be a more meaningful measure to interpret the magnitude of the program on care and readmissions than using a generic 1 s.d. increase in penalty salience which may be much more or less than the above difference.

use `insmpl', clear
gen penalized = penrate_ami > 0 | penrate_hf > 0 | penrate_pn > 0

assert pnltprs_ami!=.
assert pnltprs_hf!=.
assert pnltprs_pn!=.

*since the condition-specific penalty salience variables are not zero for non-target conditions, recode them to zero
foreach d in "ami" "hf" "pn" {
  replace pnltprs_`d' = 0 if hrrpcond==0
  replace pnltprs_hosp_`d' = 0 if hrrpcond==0
}

egen meanpp = rowmean(pnltprs_ami pnltprs_hf pnltprs_pn)
assert meanpp==0 if hrrpcond==0 | penalized==0

* Get median-of-the-mean: get the mean penalty salience across targeted conditions for patients from each penalized hospital, and then take the median across all penalized hospitals
sum meanpp if penalized & hrrpcond, de
*median = .0061597

*Median penalty salience among target patients across ALL hospitals, using the main measure
sum meanpp if hrrpcond, de

* Summary stats on penalty salience for target patients across all hospital, using the alternative, HHA's share-weighted measure
egen meanpp2 = rowmean(pnltprs_hosp_ami pnltprs_hosp_hf pnltprs_hosp_pn)
sum meanpp2 if hrrpcond, de

*Along similar lines (and this is mainly for my academic interest), if we split penalty salience into its components - what is the median penalty rate for a patient discharged from a penalized hospital and what is the median patient share of home health office for penalized hospitals

*median penalty rate for a patient discharged from a penalized hospital
egen meanpr = rowmean(penrate_ami penrate_hf penrate_pn)

* Get median-of-the-mean
sum meanpr if penalized & hrrpcond, de
*median = .0568383

*median patient share of home health office for a patient discharged from a penalized hospital
sum shref_hj if penalized, de
*median = .130064

*---------------------------------

*are the alternative measure smaller than penalty salience?
foreach c in "ami" "hf" "pn" {
  gen rat_alt_sal_`c' = pnltprs_hosp_`c'/pnltprs_`c'
}
sum rat_alt_sal_* if hrrpcond

*---------------------------------
*# states
*get office ID
use epilvl_rehosp_smpl, clear
keep offid_nu
duplicates drop
merge 1:m offid_nu using office, keep(1 3) nogen
keep offid_nu addr_st
duplicates drop
count
keep addr_st
duplicates drop
count
*16 states
tab addr_st

*sample period
use epilvl_rehosp_smpl, clear
drop if fy==2012
keep epiid socdate_e
duplicates drop
sum socdate_e
tab socdate_e if socdate_e==`r(min)' | socdate_e==`r(max)'
* 01 Jul 12 - 11 Jun 15
