*run regressions of hospital penalty salience on HH efforts and readmission

loc path /home/hcmg/kunhee/hrrp-home/data/
loc reg /home/hcmg/kunhee/hrrp-home/output
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

*----------------------------------------------------------------------
*create TM and MA samples
forval x = 0/1 {
  use epilvl_rehosp_smpl, clear

  drop if fy==2012

  *control group should remain same across the tests
  *main test: AMI/HF/PN (treatment) vs control group (which excludes COPD, Stroke, cardiorespiratory condition patients)
  drop if copd | stroke | (cardioresp & hrrpcond==0)

  assert ma + tm==1

  preserve
  keep if tm==`x'
  count
  tempfile smpl_tm`x'
  save `smpl_tm`x''
  restore
}

*----------------------------------------------------------------------
*Table 2 & 3: Diff-in-diff analysis of the impact of penalty salience measure on HH efforts and readmission among TM patients
*----------------------------------------------------------------------

loc outcome lnvtc_tr_pay lnvtc_tr_pay_1stwk1 lnlov lnlov_1stwk1 freq_tnv freq_tnv_1stwk1 hashosp30 hashosp30_1stwk1

loc pp1 ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
loc pp2 ami hf pn pnltprs_hosp_c_X_ami pnltprs_hosp_c_X_hf pnltprs_hosp_c_X_pn

use `smpl_tm1', clear

loc n 1
loc file HHeffort_TM_pp`n'
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)

  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  `out' ctitle(`l_`yv'') keep(`pp`n'') addtext(Mean dep. var., `mdv')
}

*----------------------------------------------------------------------
* Table 4 Heterogneity Analysis using a triple difference model
*----------------------------------------------------------------------

use `smpl_tm1', clear

*divide patients into 4 groups: AMI, HF, PN, non-targeted
gen group = ""
replace group = "AMI" if ami==1
replace group = "HF" if hf==1
replace group = "PN" if pn==1
replace group = "Non-target" if hrrpcond==0
assert group!=""

*use median for target vs non-target conditions separately
gen sicker = .
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

loc outcome lnvtc_tr_pay lnvtc_tr_pay_1stwk1 hashosp30 hashosp30_1stwk1
loc pp ami hf pn sicker sicker_ami sicker_hf sicker_pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn sicker_pnltprs_c_X_ami sicker_pnltprs_c_X_hf sicker_pnltprs_c_X_pn

loc file HHeffort_TM_tripleDiff
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp' `sp', absorb(offid_nu) vce(cluster offid_nu)

  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  `out' ctitle(`l_`yv'') keep(`pp') addtext(Mean dep. var., `mdv')
}

*----------------------------------------------------------------------
*Table 5 Robustness check: Diff-in-diff analysis of the impact of _alternative_ penalty salience measure on total cost and readmission among TM patients
*----------------------------------------------------------------------

loc outcome lnvtc_tr_pay lnvtc_tr_pay_1stwk1 hashosp30 hashosp30_1stwk1
loc pp1 ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
loc pp2 ami hf pn pnltprs_hosp_c_X_ami pnltprs_hosp_c_X_hf pnltprs_hosp_c_X_pn

use `smpl_tm1', clear

loc n 2
loc file HHeffort_TM_pp`n'
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)

  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  `out' ctitle(`l_`yv'') keep(`pp`n'') addtext(Mean dep. var., `mdv')
}

*----------------------------------------------------------------------
*Table 6: Falsification check by re-estimating on MA patients
*----------------------------------------------------------------------

use `smpl_tm0', clear

loc n 1
loc outcome lnvtc_tr_pay lnvtc_tr_pay_1stwk1 hashosp30 hashosp30_1stwk1

loc file HHeffort_MA
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)

  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  `out' ctitle(`l_`yv'') keep(`pp`n'') addtext(Mean dep. var., `mdv')
}

*----------------------------------------------------------------------
*Table A3: Impact on each cost component among TM patients
*----------------------------------------------------------------------
loc outcome lnvisit_tot_cost lnvisit_tot_cost_1stwk1 lnpayrate lnpayrate_1stwk1 lnvisit_travel_cost lnvisit_travel_cost_1stwk1

use `smpl_tm1', clear

loc n 1
loc file HHeffort_TM_costdetail
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)

  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  `out' ctitle(`l_`yv'') keep(`pp`n'') addtext(Mean dep. var., `mdv')
}
