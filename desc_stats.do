*Run descriptive stats

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults

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


*create TM and MA samples
forval x = 0/1 {
  use epilvl_rehosp_smpl2, clear

  *drop if had a visit on the day of readmission
  *keep if hadvisit_onra==0
  assert ma + tm==1

  *drop episode with no nurse visits
  drop if tnvsn==0

  *replace pnltprs_c = pnltprs_agg  if hrrpcond==0
  tab hrrpcond, summarize(pnltprs_c)

  *create cost per day
  loc cc vtc_tr_pay visit_tot_cost payrate visit_travel_cost
  foreach v of varlist `cc' {
      gen `v'_pd = `v'/epilength

      loc c2 `v'_1stwk1
      gen `c2'_pd = `c2'/7 if epilength >=7
      replace `c2'_pd = `c2'/epilength if epilength <7
      assert `c2'_pd !=. if `c2'!=.

      loc c3 `v'_1stwk0
      loc nl = epilength - 7
      gen `c3'_pd = `c3'/`nl' if epilength >7
      replace `c3'_pd = 0 if epilength <=7
      assert `c3'_pd !=. if `c3'!=.
  }

  loc cc2 vtc_tr_pay vtc_tr_pay_1stwk1 vtc_tr_pay_1stwk0 visit_tot_cost visit_tot_cost_1stwk1 visit_tot_cost_1stwk0 payrate payrate_1stwk1 payrate_1stwk0 visit_travel_cost visit_travel_cost_1stwk1 visit_travel_cost_1stwk0
  foreach c of varlist `cc2' {
    loc v `c'_pd
    gen ln`v' = ln(`v'+1)
  }

  preserve
  keep if tm==`x'
  count
  tempfile smpl_tm`x'
  save `smpl_tm`x''
  restore
}

*---------------------------------
*restrict to sample
loc pp1 ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn

*1) main test: AMI/HF/PN vs control (whicch excludes COPD, Stroke, cardiorespiratory condition patients)
use `smpl_tm1', clear
drop if copd | stroke | (cardioresp & hrrpcond==0)
tab hrrpcond
*control group = 6846 patients
assert hrrpcond_count <2

loc n 1
loc yv lnlov
areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)
keep if e(sample)

tempfile insmpl
save `insmpl'

*---------------------------------
*# referring hospitals
use `insmpl', clear
keep prvdr_num
duplicates drop
count
*---------------------------------

*what are the top 5 conditions among non-target patients?
use `insmpl', clear
keep epiid hrrpcond
duplicates drop
merge 1:1 epiid using masterclientadmiss2, keep(1 3) nogen keepusing(clientid socdate_e category)

keep if hrrpcond==0
/* bys category: gen freq = _N

tab category  if freq >= 551, sort
keep if freq >= 551 */

merge 1:m clientid socdate_e using inpat_dx, keep(1 3) nogen

*keep only the 3 digits before period
split inpat_dx_cor, p(".")

tempfile tmp
save `tmp'

keep inpat_dx_cor1 epiid category
gen i = 1

preserve
collapse (sum) freq = i, by(inpat_dx_cor1)
gsort inpat_dx_cor1
egen tot = sum(freq)
gen pct = 100*freq /tot
gen x = sum(pct)
rename x cumulative_pct
gsort -pct
outsheet using `reg'/inpat_dx_icd_ranking.csv, replace names comma
restore

preserve
destring inpat_dx_cor1, replace
gen badcontrol = inpat_dx_cor1 >=390 & inpat_dx_cor1<=519
keep epiid badcontrol
duplicates drop
restore
/*
foreach l in "ENCOUNTERING" "SYMPTOMS" "RHEUMATISM" "CEREBROVASCULAR" "HEART" {
  preserve
  keep if regexm(category,"`l'")
  keep inpat_dx_cor
  gen i = 1
  collapse (sum) freq = i, by(inpat_dx_cor)
  gsort -freq
  outsheet using `reg'/`l'.csv, replace cross
  restore
}

bys epiid: gen ndiag = _N
tab ndiag
*80% episodes have 1-4 inpat diagnoses codes */

*groupings by ICD-10 codes (see below for the top 5 categories). The first one is "Persons encountering health services in other circumstances".
tab category if hrrpcond==0, sort
/* Persons encountering health services in other circumstances |      5,524       37.39       37.39
                               SYMPTOMS |      1,016        6.88       44.27
         RHEUMATISM, EXCLUDING THE BACK |        685        4.64       48.91
                CEREBROVASCULAR DISEASE |        643        4.35       53.26
           OTHER FORMS OF HEART DISEASE |        551        3.73       56.99 */
*---------------------------------


*-------------------
*summary stats
*-------------------
use `insmpl', clear

*create penalty rate by condition covariates
merge m:1 offid_nu prvdr_num using HRRPpnlty_pressure_hj_2012, keep(3) nogen
capture drop pr_c
gen pr_c = .
foreach d in "ami" "hf" "pn" {
  replace pr_c = penalty2012_`d' if `d'==1
}
replace pr_c = 0 if hrrpcond==0
lab var pr_c "Condition-specific penalty rate in 2012"
assert pr_c!=.

*create groupings for AMI, HF, PN, and non-target
gen nontarget = hrrpcond==0
assert ami + hf + pn + nontarget==1
gen gp = 1 if ami==1
replace gp = 2 if hf
replace gp = 3 if pn
replace gp = 4 if nontarget

*count observations in each condition
bys gp: egen nobs = sum(1)

loc mainoutc pnltprs_c pr_c hashosp30
loc effortoutc epilength lov lovsn freq_tnv freq_tnvsn startHH_1day
loc costoutc vtc_tr_pay visit_tot_cost payrate visit_travel_cost

lab var nobs "Observations"
lab var pnltprs_c "Mean penalty salience"
lab var pr_c "Mean penalty rate"
lab var hashosp30 "30-day readmission rate"
lab var epilength "Episode length (days)"
lab var startHH_1day "Start HH within 1 day from discharge"

des nobs `mainoutc' `effortoutc' `costoutc'

*A. Sample size in each target condition, Mean penalty salience and rate, 30-day readmission rate
preserve
keep gp nobs `mainoutc'
order nobs `mainoutc'
bys gp: outreg2 using `reg'/summstats1.xls, replace sum(log)  eqkeep(N mean) label
restore

*-------------
*B. Mean measures of effort for whole episode, 1st week, after 1st week
preserve
foreach v of varlist lov lovsn freq_tnv freq_tnvsn {
  rename `v'_1stwk0 `v'_1stwk2
  rename `v' `v'_1stwk0
}

keep epiid lov* lovsn* freq_tnv* freq_tnvsn*
reshape long lov_1stwk lovsn_1stwk freq_tnv_1stwk freq_tnvsn_1stwk, i(epiid) j(wk)

lab var lov_ "Visit length (min)"
lab var lovsn "Nurse visit length (min)"
lab var freq_tnv_ "Frequency of visits (per day)"
lab var freq_tnvsn "Frequency of nurse visits (per day)"

drop epiid
bys wk: outreg2 using `reg'/summstats2.xls, replace sum(log) eqkeep(N mean) label
restore

preserve
keep epiid vtc_tr_pay*pd visit_tot_cost*pd payrate*pd visit_travel_cost*pd

foreach v in "vtc_tr_pay" "visit_tot_cost" "payrate" "visit_travel_cost" {
  rename `v'_1stwk1_pd `v'_pd_1stwk1
  rename `v'_1stwk0_pd `v'_pd_1stwk2
  rename `v'_pd `v'_pd_1stwk0
}

reshape long vtc_tr_pay_pd_1stwk visit_tot_cost_pd_1stwk payrate_pd_1stwk visit_travel_cost_pd_1stwk, i(epiid) j(wk)

lab var vtc_tr_pay_pd "Total cost index per day ($)"
lab var visit_tot_cost_pd "Visit cost per day ($)"
lab var payrate_pd "Visiting worker pays per day ($)"
lab var visit_travel_cost_pd "Travel reimbursements per day ($)"

drop epiid
bys wk: outreg2 using `reg'/summstats3.xls, replace sum(log) eqkeep(N mean) label
restore

*---------------
*C. Patient characteristics (all patients)
egen ynchsum = rowtotal(ynch? ynch??)

sum startHH_1day `riskhosp' age female white noassist livealone dual ynchsum `overallst' `hrfactor' `priorcond'

lab var age "Age"
lab var female "Female"
lab var dual "Dual eligible"
lab var avgvgap "Mean number of days between SN visits"
lab var overallst_vbad "Overall status: (Very bad) Progressive conditions"
lab var overallst_bad "Overall status: (Bad) Remain in fragile health"
lab var overallst_tempbad "Overall status: Temporarily facing high health risks"
lab var hrfactor_alcohol "High risk factor: Alcohol dependency"
lab var hrfactor_drug "High risk factor: Drug dependency"
lab var hrfactor_smoke "High risk factor: Heavy smoking"
lab var hrfactor_obese "High risk factor: Obesity"
lab var priorcond_disrupt "Pre-HHC condition: Disruptive behavior"
lab var priorcond_impdm "Pre-HHC condition: Impaired decision-making"
lab var priorcond_cath "Pre-HHC condition: Indwelling/Suprapublic catheter"
lab var priorcond_pain "Pre-HHC condition: Intractable pain"
lab var priorcond_memloss "Pre-HHC condition: Memory loss"
lab var priorcond_incontn "Pre-HHC condition: Urinary incontinence"
lab var white "White"
lab var ma_visit "Enrolled in per-visit paying Medicare Advantage"
lab var ma_epi "Enrolled in per-episode paying Medicare Advantage"
lab var dual "Dual eligible"
lab var noassist "No assistance available"
lab var livealone "Living alone"
lab var riskhosp_fall "Risk for hospitalization: History of 2+ falls"
lab var riskhosp_manyhosp "Risk for hospitalization: 2+ hospitalizations"
lab var riskhosp_mental "Risk for hospitalization: Recent decline in Mental"
lab var riskhosp_ge5med "Risk for hospitalization: Take 5+ medications"
lab var riskhosp_oth "Risk for hospitalization: Other"
lab var ynchsum "Sum of 17 Charlson comorbidity indicators"

capture drop nobs
egen nobs = sum(1)
lab var nobs "Observations"

preserve
keep nobs `mainoutc' startHH_1day `riskhosp' age female white noassist livealone dual ynchsum `overallst' `hrfactor' `priorcond'
order nobs `mainoutc' startHH_1day `riskhosp' age female white noassist livealone dual ynchsum `overallst' `hrfactor' `priorcond'
outreg2 using `reg'/summstats4.xls, replace sum(log) label eqkeep(N mean)
restore

*D & E: office & referring hospital characteristics
tab size
gen small = size==1
gen medium = size==2
gen big = size==3

loc hospchars small medium big own_* teaching urban vi_hha
lab var small "Beds 1-100"
lab var medium "Beds 101-300"
lab var big "Beds 301+"
lab var own_fp "For-profit"
lab var own_np "Not for-profit"
lab var own_gv "Government owned"
lab var urban "Urban"
lab var teaching "Teaching"
lab var vi_hha "Have a hospital-based HHA"
lab var allepi "Number of ongoing episodes in the office"
lab var nw_active_w "Number of active practitioners in the office"
loc officevar allepi nw_active_w

preserve
keep `officevar' `hospchars'
order `officevar' `hospchars'
outreg2 using `reg'/summstats5.xls, replace sum(log) label eqkeep(N mean)
restore

*---------------------------------
*Crude diff-in-diff : Mean outcomes for 4 different groups created by 2 axes: 1) zero penalty _rate_ (not salience) vs >0 penalty rate & 2) Target vs non-target conditions

use `insmpl', clear
merge m:1 prvdr_num offid_nu using HRRPpnlty_pressure_hj_2012, keep(1 3) nogen keepusing(totpenalty2012 shref_hj penalty2012_ami penalty2012_hf penalty2012_pn)

assert totpenalty2012!=.
gen penalized = totpenalty2012 > 0
tab penalized hrrpcond, summarize(vtc_tr_pay_pd)
tab penalized hrrpcond, summarize(hashosp30)

preserve
keep hashosp30 vtc_tr_pay_pd penalized hrrpcond
bys penalized hrrpcond: outreg2 using `reg'/did.xls, replace sum(log) label eqkeep(N mean)
restore

assert pnltprs_c!=.
gen penalized_prs = pnltprs_c > 0
tab penalized_prs hrrpcond, summarize(vtc_tr_pay_pd)
tab penalized_prs hrrpcond, summarize(hashosp30)

loc pp1 ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
reg hashosp30 c.pnltprs##hrrpcond

gen interaction = pnltprs_c * hrrpcond
reg hashosp30 hrrpcond interaction

preserve
keep hashosp30 vtc_tr_pay_pd penalized_prs hrrpcond
bys penalized_prs hrrpcond: outreg2 using `reg'/did2.xls, replace sum(log) label eqkeep(N mean)
restore
*---------------------------------
*What is the median penalty salience for patients that were discharged from a penalized hospital? In other words, the difference in penalty salience between being discharged by a non-penalized hospital (0) or for a non-targeted condition (0) vs. being discharged by the median penalized hospital. I think this may be a more meaningful measure to interpret the magnitude of the program on care and readmissions than using a generic 1 s.d. increase in penalty salience which may be much more or less than the above difference.

assert pnltprs_ami!=.
assert pnltprs_hf!=.
assert pnltprs_pn!=.

*since the condition-specific penalty salience variables are not zero for non-target conditions, recode them to zero
foreach d in "ami" "hf" "pn" {
  replace pnltprs_`d' = 0 if hrrpcond==0
}

egen meanpp = rowmean(pnltprs_ami pnltprs_hf pnltprs_pn)
assert meanpp==0 if hrrpcond==0 | penalized==0

* Get median-of-the-mean: get the mean penalty salience across targeted conditions for patients from each penalized hospital, and then take the median across all penalized hospitals
sum meanpp if penalized & hrrpcond, de
*median = .0045845  

*Along similar lines (and this is mainly for my academic interest), if we split penalty salience into its components - what is the median penalty rate for a patient discharged from a penalized hospital and what is the median patient share of home health office for penalized hospitals

*---------------------------------
*variation across offices in penalty pressure (penalty rate in appendix)
use `insmpl' , clear

*drop non-target patients
drop if hrrpcond==0

*pr_c
keep offid_nu pnltprs_c prvdr_num

collapse (mean) pnltprs_c, by(offid_nu prvdr_num)

*width(0.00001)
hist pnltprs_c if pnltprs_c < 0.1, frac width(0.001) xti(Mean penalty salience for each referring hospital-office pair [%]) graphregion(color(white))
*caption("Notes. Two outlier values of 0.12 and 0.5 omitted from the figure.")
graph export `reg'/hist_pnltprs_c.eps, replace

hist pr_c, frac width(0.00001) xti(Mean penalty rate across referring hospitals [%])
graph export `reg'/hist_pr_c.eps, replace

*---------------------------------
* Plot showing how difference in effort increases between penalized and non-penalized conditions as we go from less penalty pressure offices to more penalty pressure offices (quartiles or just two groups, whatever works better). Penalized conditions can be aggregated or not, depending on what is better.

/* gen pp = 1 if pnltprs_c==0
tab pnltprs_c if pnltprs_c > 0
gen pp_c = pnltprs_c if pnltprs_c > 0
xtile x = pp_c, n(2)
replace pp = x+1 if pnltprs_c > 0
assert pp!=. */

use `insmpl' , clear
sum pnltprs_c hashosp30, de

*use aggregate penalty rate (not condition-specific ones)
gen pp = pnltprs > 0
assert pp!=.
tab pp, summarize(pnltprs)

tab pp pn if ami==0 & hf==0

sum vtc_tr_pay if pp==1 & hrrpcond==0

lab define l1 0 "Non-target" 1 "Target", replace
lab define l2 0 "Penalty salience = 0" 1 "Penalty salience > 0", replace
lab val hrrpcond l1
lab val pp l2

tab pp
tab hrrpcond

gen hrrpcond4 = 1 if ami==1
replace hrrpcond4 = 2 if hf==1
replace hrrpcond4 = 3 if pn==1
replace hrrpcond4 = 4 if hrrpcond==0
assert hrrpcond4!=.

graph bar (mean) vtc_tr_pay, over(hrrpcond) over(pp) subti(Target vs Non-target condition) yti(Total cost index ($))
*ysc(r(4 8)) ylab(4(1)8)
graph export `reg'/effect_illust.eps, replace

preserve
collapse (mean) lnvtc_tr_pay vtc_tr_pay, by(pp hrrpcond)
restore

preserve
collapse (mean) lnvtc_tr_pay vtc_tr_pay, by(pp cond)
list
reshape wide lnvtc_tr_pay vtc_tr_pay, i(pp) j(cond)

forval x = 1/3 {
  gen diff`x' = lnvtc_tr_pay`x' - lnvtc_tr_pay4
}
list pp diff*

restore

*---------------------------------
*# states
*get office ID
use epilvl_rehosp_smpl_old, clear
keep offid_nu
duplicates drop
merge 1:m offid_nu using office, keep(1 3) nogen
keep offid_nu addr_st
duplicates drop
count
keep addr_st
duplicates drop
count
tab addr_st

use office, clear
keep offid_nu addr_st
duplicates drop
count
keep addr_st
duplicates drop
count

*sample period
use epilvl_rehosp_smpl_old, clear
keep epiid socdate_e
duplicates drop
merge 1:1 epiid using HHeffort_epilvl, keep(2 3) nogen
sum socd
tab socd if socd==19267
tab socd if socd==20250
