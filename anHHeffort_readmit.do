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

// * 1000 more obs after correcting the data error
// use epilvl_rehosp_smpl, clear
// keep epiid
// merge 1:1 epiid using epilvl_rehosp_smpl2, keep(1) nogen
// keep epiid
// save newly_added_pats, replace
//
// merge 1:1 epiid using epilvl_rehosp_smpl, keep(3) nogen
*----------------------------------------------------------------------
*create TM and MA samples
forval x = 0/1 {
  use epilvl_rehosp_smpl, clear

  *drop if had a visit on the day of readmission
  *keep if hadvisit_onra==0
  assert ma + tm==1

  preserve
  keep if tm==`x'
  count
  tempfile smpl_tm`x'
  save `smpl_tm`x''
  restore
}

*----------------------------------------------------------------------
*Table 2 & 3 (main regression results): TM patients - effect on HH efforts
*----------------------------------------------------------------------

*selected outcomes
*loc outcome lnvtc_tr_pay lnvtc_tr_pay_1stwk1 lnlov lnlov_1stwk1 freq_tnv freq_tnv_1stwk1 hashosp30 hashosp30_1stwk1
loc outcome lnlov lnlov_1stwk1 lnlovsn lnlovsn_1stwk1 freq_tnv freq_tnv_1stwk1 freq_tnvsn freq_tnvsn_1stwk1 startHH_1day lnvtc_tr_pay lnvtc_tr_pay_1stwk1 lnvisit_tot_cost lnvisit_tot_cost_1stwk1 lnpayrate lnpayrate_1stwk1 lnvisit_travel_cost lnvisit_travel_cost_1stwk1 hashosp30 hashosp30_1stwk1

loc pp1 ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
loc pp2 ami hf pn pnltprs_hosp_c_X_ami pnltprs_hosp_c_X_hf pnltprs_hosp_c_X_pn

*control group should remain same across the tests
*main test: AMI/HF/PN vs control (which excludes COPD, Stroke, cardiorespiratory condition patients)

*1) main test: AMI/HF/PN vs control (which excludes COPD, Stroke, cardiorespiratory condition patients)
use `smpl_tm1', clear
drop if copd | stroke | (cardioresp & hrrpcond==0)
tab hrrpcond
*control group = 6846 patients
assert hrrpcond_count <2

loc n 1
loc yv lnlov
areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)
keep if e(sample)

tab ami
tab hf
tab pn
tab hrrpcond

forval n=1/2 {
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
}


*----------------------------------------------------------------------
* Table 4 Heterogneity Analysis: HF patients: are the sicker people driving the result? if the effect we find for heart failure patients is driven by the sicker patients among them. We could test this using a simple triple diff where we add an interaction term with an indicator for the patient being in the top half of sick patients (by some sickness severity score)
*----------------------------------------------------------------------

use `smpl_tm1', clear
drop if copd | stroke | (cardioresp & hrrpcond==0)
tab hrrpcond
*control group = 6846 patients
assert hrrpcond_count <2

*divide patients into 4 groups: AMI, HF, PN, non-targeted
gen group = ""
replace group = "AMI" if ami==1
replace group = "HF" if hf==1
replace group = "PN" if pn==1
replace group = "Non-target" if hrrpcond==0
assert group!=""

gen sicker = .
*use median for target vs non-target conditions separately
forval x = 0/1 {
  sum riskhosp if hrrpcond==`x', de
  loc p50 = `r(p50)'
  replace sicker = riskhosp > `p50' if hrrpcond==`x'
}
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

loc file HHeffort_TM_tripleDiff
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
*falsification check by re-estimating with only MA patients
*----------------------------------------------------------------------

use `smpl_tm0', clear
drop if copd | stroke | (cardioresp & hrrpcond==0)
tab hrrpcond
*control group = 6846 patients
assert hrrpcond_count <2

loc n 1
loc yv lnlov
areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)
keep if e(sample)

tab ami
tab hf
tab pn
tab hrrpcond

forval n = 1/1 {
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
