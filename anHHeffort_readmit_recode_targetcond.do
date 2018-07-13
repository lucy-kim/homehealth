*re-run analysis after recoding the target condition patients based only on the first inpatient diagnosis (ICD) code recorded for the patient, rather than all the diagnosis codes reported for her

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


*-------------
*create 30-day readmission indicator in the first week (trun off if the readmission occurred after first week)
use HHeffort_visit_old, clear

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
merge 1:1 epiid using epilvl_rehosp_smpl_old_firstdx, keep(2 3) nogen

*use predicted penalty probabilityfor 2012 instead of 2012 penalty rate
drop pnltprs_pred_hosp_* pnltprs_pred_* pnltprs_hosp_*
merge m:1 offid_nu prvdr_num using HRRPpnlty_pressure_hj_2012, keepusing(pnltprs_pred_hosp_* pnltprs_pred_* pnltprs_hosp_* sharetoBayada) keep(1 3) nogen

sum pnltprs_hosp_hf pnltprs_hosp_pn if tm==1

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

compress
save epilvl_rehosp_smpl_old2_firstdx, replace

*--------------------

*create TM and MA samples
forval x = 0/1 {
  use epilvl_rehosp_smpl_old2_firstdx, clear

  *drop if had a visit on the day of readmission
  *keep if hadvisit_onra==0
  assert ma + tm==1

  *drop episode with no nurse visits
  drop if tnvsn==0

  *replace pnltprs_c = pnltprs_agg  if hrrpcond==0
  tab hrrpcond, summarize(pnltprs_c)

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
use `smpl_tm1', clear
*throughout the entire episode
loc outcome lnlov lnlov_1stwk1 lnlov_1stwk0 lnlovsn lnlovsn_1stwk1 lnlovsn_1stwk0 freq_tnv freq_tnv_1stwk1 freq_tnv_1stwk0 freq_tnvsn freq_tnvsn_1stwk1 freq_tnvsn_1stwk0 startHH_1day lnvtc_tr_pay lnvtc_tr_pay_1stwk1 lnvtc_tr_pay_1stwk0 lnvisit_tot_cost lnvisit_tot_cost_1stwk1 lnvisit_tot_cost_1stwk0 lnpayrate lnpayrate_1stwk1 lnpayrate_1stwk0 lnvisit_travel_cost lnvisit_travel_cost_1stwk1 lnvisit_travel_cost_1stwk0 hashosp30 hashosp30_1stwk1 hashosp30_1stwk0

loc pp1 ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
*loc pp ami hf pn pnltprs pnltprs_X_ami pnltprs_X_hf pnltprs_X_pn
loc pp2 ami hf pn pnltprs_hosp_c_X_ami pnltprs_hosp_c_X_hf pnltprs_hosp_c_X_pn
loc pp3 ami hf pn pnltprs_pred_c_X_ami pnltprs_pred_c_X_hf pnltprs_pred_c_X_pn
loc pp4 ami hf pn pnltprs_pred_hosp_c_X_ami pnltprs_pred_hosp_c_X_hf pnltprs_pred_hosp_c_X_pn

loc n 1
loc yv lnlov
areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)

tab ami if e(sample)
tab hf if e(sample)
tab pn if e(sample)
tab hrrpcond if e(sample)
sum pnltprs_ami pnltprs_hf pnltprs_pn pnltprs_hosp_ami pnltprs_hosp_hf pnltprs_hosp_pn if e(sample)


forval n = 1/2 {
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
*falsification check by re-estimating with only MA patients
*----------------------------------------------------------------------

*effect on HH efforts
use `smpl_tm0', clear

tab hf, summarize(pnltprs_c)
tab ami, summarize(pnltprs_c)

*throughout the entire episode
loc outcome lnlov lnlov_1stwk1 lnlov_1stwk0 lnlovsn lnlovsn_1stwk1 lnlovsn_1stwk0 freq_tnv freq_tnv_1stwk1 freq_tnv_1stwk0 freq_tnvsn freq_tnvsn_1stwk1 freq_tnvsn_1stwk0 startHH_1day lnvtc_tr_pay lnvtc_tr_pay_1stwk1 lnvtc_tr_pay_1stwk0 lnvisit_tot_cost lnvisit_tot_cost_1stwk1 lnvisit_tot_cost_1stwk0 lnpayrate lnpayrate_1stwk1 lnpayrate_1stwk0 lnvisit_travel_cost lnvisit_travel_cost_1stwk1 lnvisit_travel_cost_1stwk0 hashosp30 hashosp30_1stwk1 hashosp30_1stwk0

loc pp1 ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
*loc pp ami hf pn pnltprs pnltprs_X_ami pnltprs_X_hf pnltprs_X_pn
loc pp2 ami hf pn pnltprs_hosp_c_X_ami pnltprs_hosp_c_X_hf pnltprs_hosp_c_X_pn
loc pp3 ami hf pn pnltprs_pred_c_X_ami pnltprs_pred_c_X_hf pnltprs_pred_c_X_pn
loc pp4 ami hf pn pnltprs_pred_hosp_c_X_ami pnltprs_pred_hosp_c_X_hf pnltprs_pred_hosp_c_X_pn


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
