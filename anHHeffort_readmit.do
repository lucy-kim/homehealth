*run regressions of hospital penalty salience on HH efforts and readmission
* Remove cardiorespiratory conditions from the control group; run regs for P(Readmission) in the first week as outcome; and replace the 2012 penalty rate with the 2012 predicted probability of penalty

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
loc outcome lnepilength lnlov lnlovsn freq_tnv freq_tnvsn startHH_1day lnvtc_tr_pay

loc l_lnepilength "Ln Episode length (days)"
loc l_lnlov "Ln Visit length (min)"
loc l_lnlovsn "Ln Nurse visit length (min)"
loc l_freq_tnv "Frequency of visits"
loc l_freq_tnvsn "Frequency of nurse visits"
loc l_startHH_1day "Start HH within 1 day from discharge"
loc l_lnvtc_tr_pay "Ln Total cost index"


*identify cardiorespiratory conditions from the control groups
use masterclientadmiss2, clear
keep epiid clientid socdate_e
duplicates drop
merge 1:1 clientid epiid using epilvl_rehosp_smpl, keep(2 3) nogen
merge 1:m clientid socdate_e using inpat_dx, keep(3) nogen
split inpat_dx_cor, p(".")
destring inpat_dx_cor1, replace
tab inpat_dx_cor1 if inpat_dx_cor1>=390 & inpat_dx_cor1 <= 519
tab ccsdesc if inpat_dx_cor1>=390 & inpat_dx_cor1 <= 519

gen x = inpat_dx_cor1>=390 & inpat_dx_cor1 <= 519
bys epiid: egen cardioresp = max(x)
tab cardioresp hrrpcond
*hrrp condition can be cardiorespiratory
tab stroke cardioresp
*all stroke patients without HRRP target conditions have cardioresp conditions
*drop if hrrpcond==0 & cardioresp==1
drop x inpat_dx-inpat_dx_cor2
duplicates drop

tempfile an
save `an'

*-------------
*create 30-day readmission indicator in the first week (trun off if the readmission occurred after first week)
use HHeffort_visit, clear

*create episode level 30-day hospital readmission indicator
*get 30-day readmission indicator
gen days2hosp = firsthospdate - inpat_dcd
gen start_1stwk = fvd
gen end_1stwk = fvd+6
gen hospoccur_1stwk = firsthospdate >= start_1stwk & firsthospdate <= end_1stwk
assert hospoccur_1stwk==0 if firsthospdate==.
*list epiid visitd wkidx hashosp firsthospdate hospoccur_1stwk start_1stwk end_1stwk in 40/70
gen hashosp30 = hashosp * (days2hosp <= 30)
gen hashosp30_1stwk1 = hashosp * (days2hosp <= 30) * hospoccur_1stwk
gen hashosp30_1stwk0 = hashosp * (days2hosp <= 30) * (1-hospoccur_1stwk)
assert (hashosp30_1stwk1+ hashosp30_1stwk0==1 ) |  (hashosp30_1stwk1 + hashosp30_1stwk0==0)
assert (hashosp30_1stwk1==1 | hashosp30_1stwk0==1 ) if hashosp30==1
lab var hashosp30_1stwk1 "30-day hospital readmission in the first week"
lab var hashosp30_1stwk0 "30-day hospital readmission beyond the first week"
keep epiid hashosp30_1stwk1 hashosp30_1stwk0
duplicates drop
merge 1:1 epiid using `an', keep(2 3) nogen

*use predicted penalty probabilityfor 2012 instead of 2012 penalty rate
merge m:1 offid_nu prvdr_num using HRRPpnlty_pressure_hj_2012, keepusing(pnltprs_pred_hosp_* pnltprs_pred_* pnltprs_hosp_* sharetoBayada) keep(1 3) nogen

loc uami "AMI"
loc uhf "HF"
loc upn "PN"
foreach d in "ami" "hf" "pn" {
  replace pnltprs_pred_hosp_`d' = 0 if hrrpcond==0
  capture drop pnltprs_pred_hosp_c_X_`d'
  gen pnltprs_pred_hosp_c_X_`d' = pnltprs_pred_hosp_`d' *`d'

  replace pnltprs_pred_`d' = 0 if hrrpcond==0
  *capture drop pnltprs_pred_c_X_`d'
  gen pnltprs_pred_c_X_`d' = pnltprs_pred_`d' *`d'

  replace pnltprs_hosp_`d' = 0 if hrrpcond==0
  capture drop pnltprs_hosp_c_X_`d'
  gen pnltprs_hosp_c_X_`d' = pnltprs_hosp_`d' *`d'
}
tab hrrpcond, summarize(pnltprs_pred_c_X_ami)
tab hrrpcond, summarize(pnltprs_pred_hosp_c_X_ami)
tab hrrpcond, summarize(pnltprs_hosp_c_X_ami)

*use the median penalty rate for the hospital across the three target conditions for COPD and (placebo test conditions but no penalty rate available for them) before calculating the penalty pressure
merge m:1 offid_nu prvdr_num using HRRPpnlty_pressure_hj_2012, keepusing(pnltprs_med) keep(1 3) nogen
foreach d in "copd" "stroke" {
  gen pnltprs_med_`d' = 0 if hrrpcond==0
  replace pnltprs_med_`d' = pnltprs_med*`d' if `d'==1
  assert hrrpcond==1 if pnltprs_med_`d'==.
}

compress
save epilvl_rehosp_smpl2, replace
*-------------

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
  gen nl = epilength - 7

  loc cc vtc_tr_pay visit_tot_cost payrate visit_travel_cost
  foreach v of varlist `cc' {
      gen `v'_pd = `v'/epilength

      loc c2 `v'_1stwk1
      gen `c2'_pd = `c2'/7 if epilength >=7
      replace `c2'_pd = `c2'/epilength if epilength <7
      assert `c2'_pd !=. if `c2'!=.

      loc c3 `v'_1stwk0
      gen `c3'_pd = `c3'/nl if epilength >7
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

*----------------------------------------------------------------------
*TM patients : effect on HH efforts
*----------------------------------------------------------------------

loc outcome lnlov lnlov_1stwk1 lnlov_1stwk0 lnlovsn lnlovsn_1stwk1 lnlovsn_1stwk0 freq_tnv freq_tnv_1stwk1 freq_tnv_1stwk0 freq_tnvsn freq_tnvsn_1stwk1 freq_tnvsn_1stwk0 startHH_1day lnvtc_tr_pay lnvtc_tr_pay_1stwk1 lnvtc_tr_pay_1stwk0 lnvisit_tot_cost lnvisit_tot_cost_1stwk1 lnvisit_tot_cost_1stwk0 lnpayrate lnpayrate_1stwk1 lnpayrate_1stwk0 lnvisit_travel_cost lnvisit_travel_cost_1stwk1 lnvisit_travel_cost_1stwk0 hashosp30 hashosp30_1stwk1 hashosp30_1stwk0
*just cost per day outcomes
*loc outcome lnvtc_tr_pay_pd lnvtc_tr_pay_1stwk1_pd lnvtc_tr_pay_1stwk0_pd lnvisit_tot_cost_pd lnvisit_tot_cost_1stwk1_pd lnvisit_tot_cost_1stwk0_pd lnpayrate_pd lnpayrate_1stwk1_pd lnpayrate_1stwk0_pd lnvisit_travel_cost_pd lnvisit_travel_cost_1stwk1_pd lnvisit_travel_cost_1stwk0_pd

loc pp1 ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
*loc pp ami hf pn pnltprs pnltprs_X_ami pnltprs_X_hf pnltprs_X_pn
loc pp2 ami hf pn pnltprs_hosp_c_X_ami pnltprs_hosp_c_X_hf pnltprs_hosp_c_X_pn
loc pp3 ami hf pn pnltprs_pred_c_X_ami pnltprs_pred_c_X_hf pnltprs_pred_c_X_pn
loc pp4 ami hf pn pnltprs_pred_hosp_c_X_ami pnltprs_pred_hosp_c_X_hf pnltprs_pred_hosp_c_X_pn
loc pp5 copd pnltprs_med_copd
loc pp6 stroke pnltprs_med_stroke

*control group should remain same across the tests
*main test: AMI/HF/PN vs control (whicch excludes COPD, Stroke, cardiorespiratory condition patients)
*placebo test 1 : COPD (none of the 3 target conditions) vs control (excludes stroke, cardiorespiratory)
*placebo test 2 : Stroke (none of the 3 target conditions or COPD) vs control (excludes stroke, cardiorespiratory)

*1) main test: AMI/HF/PN vs control (whicch excludes COPD, Stroke, cardiorespiratory condition patients)
use `smpl_tm1', clear
drop if copd | stroke | (cardioresp & hrrpcond==0)
tab hrrpcond
*control group = 6846 patients
assert hrrpcond_count <2

loc n 1
loc yv lnlov
areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)

tab ami if e(sample)
tab hf if e(sample)
tab pn if e(sample)
tab hrrpcond if e(sample)
sum pnltprs_ami pnltprs_hf pnltprs_pn pnltprs_hosp_ami pnltprs_hosp_hf pnltprs_hosp_pn if e(sample)


loc file HHeffort_TM`n'
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp`n'') addtext(F statistic, `fstat', Mean dep. var., `mdv')
}

*----------------------------------------------------------------------
*Heterogneity Analysis: HF patients: are the sicker people driving the result? if the effect we find for heart failure patients is driven by the sicker patients among them. We could test this using a simple triple diff where we add an interaction term with an indicator for the patient being in the top half of sick patients (by some sickness severity score)
*----------------------------------------------------------------------

use `smpl_tm1', clear
drop if copd | stroke | (cardioresp & hrrpcond==0)
tab hrrpcond
*control group = 6846 patients
assert hrrpcond_count <2

* create a severity measure: sum of risk of hospitalization categories at baseline
capture drop riskhosp
egen riskhosp = rowtotal(riskhosp_* hrfactor_* priorcond_*)
tab riskhosp

*divide patients into 4 groups: AMI, HF, PN, non-targeted
gen group = ""
replace group = "AMI" if ami==1
replace group = "HF" if hf==1
replace group = "PN" if pn==1
replace group = "Non-target" if hrrpcond==0
assert group!=""

gen sicker = .
*use median for each condition separately
foreach x in "AMI" "HF" "PN" "Non-target" {
  sum riskhosp if group=="`x'", de
  loc p50 = `r(p50)'
  replace sicker = riskhosp > `p50' if group=="`x'"
}
*use median for target vs non-target conditions separately
// forval x = 0/1 {
//   sum riskhosp if hrrpcond==`x', de
//   loc p50 = `r(p50)'
//   replace sicker = riskhosp > `p50' if hrrpcond==`x'
// }
assert sicker!=.
lab var sicker "Sicker"

* if the sicker includes patients with riskhosp=median, then those with HF (or PN) in the bottom half of severity always have zero penalty salience, so the triple interaction terms drop out when interacted with HF / PN
tab ami if sicker, summarize(pnltprs_c)
tab hf if sicker, summarize(pnltprs_c)
tab pn if sicker, summarize(pnltprs_c)

foreach v of varlist ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn {
  gen sicker_`v' = sicker*`v'

  loc xlab: di "`: var label `v''"
  lab var sicker_`v' "Sicker X `xlab'"
}
loc v pnltprs_c
gen sicker_pnltprs_c = sicker*`v'
lab var `v' "Condition-specific penalty salience"
loc xlab: di "`: var label `v''"
lab var sicker_`v' "Sicker X `xlab'

des sicker*

loc pp ami hf pn sicker sicker_ami sicker_hf sicker_pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn sicker_pnltprs_c_X_ami sicker_pnltprs_c_X_hf sicker_pnltprs_c_X_pn

loc file HHeffort_TM1_tripleDiff
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp' `sp', absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv')
}
*----------------------------------------------------------------------
*2) placebo test 1 : COPD (none of the 3 target conditions) vs control (excludes stroke, cardiorespiratory) - RESUME
use `smpl_tm1', clear
drop if hrrpcond
drop if stroke
drop if (cardioresp & copd==0)
tab copd
*control group = 6846 patients
assert hrrpcond_count <2

loc n 5
loc file HHeffort_TM`n'
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp`n'') addtext(F statistic, `fstat', Mean dep. var., `mdv')
}

tab copd if e(sample)
sum pnltprs_med if e(sample)


*3) placebo test 2 : Stroke (none of the 3 target conditions or COPD) vs control (excludes stroke, cardiorespiratory)

use `smpl_tm1', clear
drop if hrrpcond
drop if copd
drop if (cardioresp & stroke==0)
tab stroke
*control group = 6846 patients
assert hrrpcond_count <2

loc n 6
loc file HHeffort_TM`n'
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp`n'') addtext(F statistic, `fstat', Mean dep. var., `mdv')
}

tab stroke if e(sample)
sum pnltprs_med if e(sample)


*----------------------------------------------------------------------
*falsification check by re-estimating with only MA patients
*----------------------------------------------------------------------

*effect on HH efforts

loc outcome lnlov lnlov_1stwk1 lnlov_1stwk0 lnlovsn lnlovsn_1stwk1 lnlovsn_1stwk0 freq_tnv freq_tnv_1stwk1 freq_tnv_1stwk0 freq_tnvsn freq_tnvsn_1stwk1 freq_tnvsn_1stwk0 startHH_1day lnvtc_tr_pay_pd lnvtc_tr_pay_1stwk1_pd lnvtc_tr_pay_1stwk0_pd lnvisit_tot_cost_pd lnvisit_tot_cost_1stwk1_pd lnvisit_tot_cost_1stwk0_pd lnpayrate_pd lnpayrate_1stwk1_pd lnpayrate_1stwk0_pd lnvisit_travel_cost_pd lnvisit_travel_cost_1stwk1_pd lnvisit_travel_cost_1stwk0_pd hashosp30 hashosp30_1stwk1 hashosp30_1stwk0
*just cost per day outcomes
*loc outcome lnvtc_tr_pay_pd lnvtc_tr_pay_1stwk1_pd lnvtc_tr_pay_1stwk0_pd lnvisit_tot_cost_pd lnvisit_tot_cost_1stwk1_pd lnvisit_tot_cost_1stwk0_pd lnpayrate_pd lnpayrate_1stwk1_pd lnpayrate_1stwk0_pd lnvisit_travel_cost_pd lnvisit_travel_cost_1stwk1_pd lnvisit_travel_cost_1stwk0_pd

loc pp1 ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
*loc pp ami hf pn pnltprs pnltprs_X_ami pnltprs_X_hf pnltprs_X_pn
loc pp2 ami hf pn pnltprs_hosp_c_X_ami pnltprs_hosp_c_X_hf pnltprs_hosp_c_X_pn
loc pp3 ami hf pn pnltprs_pred_c_X_ami pnltprs_pred_c_X_hf pnltprs_pred_c_X_pn
loc pp4 ami hf pn pnltprs_pred_hosp_c_X_ami pnltprs_pred_hosp_c_X_hf pnltprs_pred_hosp_c_X_pn

use `smpl_tm0', clear
drop if copd | stroke | (cardioresp & hrrpcond==0)
tab hrrpcond
*control group = 6846 patients
assert hrrpcond_count <2

loc n 1
loc yv lnlov
areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)

tab ami if e(sample)
tab hf if e(sample)
tab pn if e(sample)
tab hrrpcond if e(sample)
sum pnltprs_ami pnltprs_hf pnltprs_pn pnltprs_hosp_ami pnltprs_hosp_hf pnltprs_hosp_pn if e(sample)

forval n = 1/2 {
  loc file HHeffort_MA`n'
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, dec(3) label append nocons"

  foreach yv of varlist `outcome' {
    areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)
    *if hashosp==0
    sum `yv' if e(sample)
    loc mdv: display %9.2f `r(mean)'
    loc ar2: display %9.2f `e(r2_a)'

    qui test
    loc fstat: display %9.2f `r(F)'

    `out' ctitle(`l_`yv'') keep(`pp`n'') addtext(F statistic, `fstat', Mean dep. var., `mdv')
  }
}

*----------------------------------------------------------------------
*Robustness check by using penalty rate alone instead of penalty salience
*----------------------------------------------------------------------
use `smpl_tm1', clear
drop if copd | stroke | (cardioresp & hrrpcond==0)
tab hrrpcond
*control group = 6846 patients
assert hrrpcond_count <2

loc n 1
loc yv lnlov
areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)

tab ami if e(sample)
tab hf if e(sample)
tab pn if e(sample)
tab hrrpcond if e(sample)
sum pnltprs_ami pnltprs_hf pnltprs_pn pnltprs_hosp_ami pnltprs_hosp_hf pnltprs_hosp_pn if e(sample)

preserve
*get 2012 penalty rate to calculate penalty pressure for each office j - hospital h pair later
use hrrp_penalty, clear
keep prvdr_num penalty2012_*
duplicates drop

*rename fyear fy
destring prvdr_num, replace

foreach v of varlist *penalty* {
  replace `v' = 0 if `v'==.
}

rename penalty2012_chf penalty2012_hf
rename penalty2012_pneum penalty2012_pn

tempfile hrrp_penalty
save `hrrp_penalty'
restore

merge m:1 prvdr_num using `hrrp_penalty', keepusing(penalty2012*) keep(1 3) nogen

*create condition-specific penalty rate in a single variable
gen prate_c = .
foreach d in "ami" "hf" "pn" {
  replace prate_c = penalty2012_`d' if `d'==1
}
replace prate_c = 0 if hrrpcond==0
assert prate_c!=.

*create interaction term of penalty rate and target condition indicators
loc uami "AMI"
loc uhf "HF"
loc upn "PN"
foreach d in "ami" "hf" "pn" {
  capture drop prate_c_X_`d'
  gen prate_c_X_`d' = prate_c *`d'

  lab var prate_c_X_`d' "Condition-specific penalty rate X `u`d''"
}

loc pp7 ami hf pn prate_c_X_ami prate_c_X_hf prate_c_X_pn

loc n 7
loc file HHeffort_TM`n'_prate
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp`n'') addtext(F statistic, `fstat', Mean dep. var., `mdv')
}

*------------------------------------------------------------------------
* Robustness check: use probability of readmission within 30 days of the start of the episode, instead of 30 days from the hospital discharge date
*------------------------------------------------------------------------
use `smpl_tm1', clear
drop if copd | stroke | (cardioresp & hrrpcond==0)
tab hrrpcond
*control group = 6846 patients
assert hrrpcond_count <2

loc n 1
loc yv lnlov
areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)
keep if e(sample)

tab ami if e(sample)
tab hf if e(sample)
tab pn if e(sample)
tab hrrpcond if e(sample)
sum pnltprs_ami pnltprs_hf pnltprs_pn pnltprs_hosp_ami pnltprs_hosp_hf pnltprs_hosp_pn if e(sample)

*define readmission within 30 days of the start of the episode

*get the first hospitalization date & first episode date from visit-level data
preserve
use HHeffort_visit, clear
keep epiid firsthospdate fvd
duplicates drop
tempfile tmp
save `tmp'
restore

merge 1:1 epiid using `tmp', keep(1 3) nogen

gen days2hosp_fromHHstart = firsthospdate - fvd
gen hashosp30_fromHHstart = hashosp * (days2hosp_fromHHstart <= 30)
loc outcome hashosp30_fromHHstart

loc file readmit_TM`n'_fromHHstart
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp`n'') addtext(F statistic, `fstat', Mean dep. var., `mdv')
}
