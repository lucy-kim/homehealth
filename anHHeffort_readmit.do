*run regressions of hospital penalty salience on HH efforts and readmission

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

forval x = 0/1 {
  use epilvl_rehosp_smpl_old, clear

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
loc outcome lnepilength lnlov lnlovsn freq_tnv freq_tnvsn startHH_1day lnvtc_tr_pay

loc pp ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
*loc pp ami hf pn pnltprs pnltprs_X_ami pnltprs_X_hf pnltprs_X_pn

loc file HHeffort_TM
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

*only during the first week
loc file HHeffort_TM_1stwk1
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, dec(3) label append nocons"

loc outcome lnepilength_1stwk1 lnlov_1stwk1 lnlovsn_1stwk1 freq_tnv_1stwk1 freq_tnvsn_1stwk1 startHH_1day lnvtc_tr_pay_1stwk1
des `outcome'

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

*after the first week_tnv freq_tnvsn
loc file HHeffort_TM_1stwk0
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, dec(3) label append nocons"

loc outcome lnepilength_1stwk0 lnlov_1stwk0 lnlovsn_1stwk0 freq_tnv_1stwk0 freq_tnvsn_1stwk0 startHH_1day lnvtc_tr_pay_1stwk0
des `outcome'

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
*IV using TM patients
use `smpl_tm1', clear

*define macros for key variables
loc yv hashosp30
sum `yv'
loc nend 1
loc end1 lnvtc_tr_pay
loc end2 lnvtc_tr_pay_1stwk1
loc end3 lnvtc_tr_pay_1stwk0

eststo clear

*initiate the tuple of first-stage reg results for each endog var (whose element will be each spec `n')
loc fsr

*initiate the tuple of second-stage reg results
loc ssr

*initiate control list
loc ctrl_hr
loc ctrl_dm
loc ctrl_cm

loc iv pnltprs_c pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
*loc iv pnltprs pnltprs_X_ami pnltprs_X_hf pnltprs_X_pn

forval nn = 1/3 {
  loc ev1 `end`nn''
  eststo y_n`nn': ivreg2 `yv' `sp' ami hf pn i.offid_nu (`end`nn'' = `iv'), cluster(offid_nu) first savefirst savefprefix(f_n`nn'_) gmm2s partial(i.fy i.offid_nu ) endog(`end`nn'')

  estimates dir

  mat fstat_n`nn' = e(first)

  *for each first stage, save in a separate file

  estadd scalar fs = fstat_n`nn'[4,`nend'] : f_n`nn'_`end`nn''

  foreach ev of varlist `end`nn'' {
    loc fsr `fsr' f_n`nn'_`ev'
  }
  loc ssr `ssr' y_n`nn'
  di "`ssr'"

  *get R-squared from first stage OLS regression
  areg `end`nn'' `iv' `sp' ami hf pn , absorb(offid_nu) vce(cluster offid_nu)
  estadd scalar fr2 = `e(r2)' : f_n`nn'_`end`nn''
}

loc file iv1
esttab `fsr' using `reg'/`file'.csv, replace stats(N fr2 fs, fmt(0 3 3) label(Observations "R-squared" "F-statistic")) cells(b(star fmt(3)) se(par fmt(3))) keep(`iv') label starlevels( * 0.10 ** 0.05 *** 0.010)

*save 2nd stage reg
loc file iv2s
esttab `ssr' using `reg'/`file'.csv, replace stats(r2 jp N, fmt(3 3 0) label("R-squared" "J-statistic p-value" "Observations")) cells(b(star fmt(3)) se(par fmt(3))) keep(`end1' `end2' `end3' ) label starlevels( * 0.10 ** 0.05 *** 0.010)

*----------------------------------------------------------------------
*reduced-form model

loc pp ami hf pn pnltprs_c pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
*loc pp ami hf pn pnltprs pnltprs_X_ami pnltprs_X_hf pnltprs_X_pn

loc file HHeffort_TM_read30_rf
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, dec(3) label append nocons"

loc yv hashosp30

areg `yv' `pp' `sp', absorb(offid_nu) vce(cluster offid_nu)
*if hashosp==0
sum `yv' if e(sample)
loc mdv: display %9.2f `r(mean)'
loc ar2: display %9.2f `e(r2_a)'

qui test
loc fstat: display %9.2f `r(F)'

`out' ctitle(`l_`yv'') keep(ami hf pn `pp`n'') addtext(F statistic, `fstat', Mean dep. var., `mdv')

*----------------------------------------------------------------------
*falsification check by re-estimating with only MA patients
*----------------------------------------------------------------------

use `smpl_tm0', clear

*effect on HH efforts

*throughout the entire episode
loc outcome lnepilength lnlov lnlovsn freq_tnv freq_tnvsn startHH_1day lnvtc_tr_pay hashosp30

loc pp ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
*loc pp ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn

loc file HHeffort_MA
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

*only during the first week
loc file HHeffort_MA_1stwk1
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, dec(3) label append nocons"

loc outcome lnepilength_1stwk1 lnlov_1stwk1 lnlovsn_1stwk1 freq_tnv_1stwk1 freq_tnvsn_1stwk1 startHH_1day lnvtc_tr_pay_1stwk1
des `outcome'

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

*after the first week_tnv freq_tnvsn
loc file HHeffort_MA_1stwk0
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, dec(3) label append nocons"

loc outcome lnepilength_1stwk0 lnlov_1stwk0 lnlovsn_1stwk0 freq_tnv_1stwk0 freq_tnvsn_1stwk0 startHH_1day lnvtc_tr_pay_1stwk0
des `outcome'

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
*IV using MA patients


*define macros for key variables
loc yv hashosp30
sum `yv'
loc nend 1
loc end1 lnvtc_tr_pay
loc end2 lnvtc_tr_pay_1stwk1
loc end3 lnvtc_tr_pay_1stwk0

eststo clear

*initiate the tuple of first-stage reg results for each endog var (whose element will be each spec `n')
foreach ev of varlist `end' {
    loc fsr_`ev'
}

*initiate the tuple of second-stage reg results
loc ssr

*initiate control list
loc ctrl_hr
loc ctrl_dm
loc ctrl_cm

*loc iv pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
loc iv pnltprs pnltprs_X_ami pnltprs_X_hf pnltprs_X_pn

forval nn = 1/3 {
  loc ev1 `end`nn''
  eststo y_n`nn': ivreg2 `yv' `sp' ami hf pn i.offid_nu (`end`nn'' = `iv'), cluster(offid_nu) first savefirst savefprefix(f_n`nn'_) gmm2s partial(i.fy i.offid_nu ) endog(`end`nn'')

  estimates dir

  mat fstat_n`nn' = e(first)

  *for each first stage, save in a separate file
  forval j = 1/`nend' {
    estadd scalar fs_`ev`j'' = fstat_n`nn'[4,`j'] : f_n`nn'_`ev`j''
  }

  foreach ev of varlist `end`nn'' {
    loc fsr_`ev' `fsr_`ev'' f_n`nn'_`ev'
  }
  loc ssr `ssr' y_n`nn'
  di "`ssr'"

  *get R-squared from first stage OLS regression
  foreach ev of varlist `end`nn'' {
    areg `ev' `iv' `sp' , absorb(offid_nu) vce(cluster offid_nu)
    estadd scalar fr2_`ev' = `e(r2)' : f_n`nn'_`ev'
  }

  *for each first stage, save in a separate file
  foreach ev of varlist `end`nn'' {
    di "`ev'"
    di "`fsr_`ev''"
    loc file iv1s_`ev'_MA
    esttab `fsr_`ev'' using `reg'/`file'.csv, replace stats(N fr2_`ev' fs_`ev', fmt(0 3 3) label(Observations "R-squared" "F-statistic")) cells(b(star fmt(3)) se(par fmt(3))) keep(`iv') label starlevels( * 0.10 ** 0.05 *** 0.010)
  }
}

*save 2nd stage reg
loc file iv2s_MA
esttab `ssr' using `reg'/`file'.csv, replace stats(r2 jp N, fmt(3 3 0) label("R-squared" "J-statistic p-value" "Observations")) cells(b(star fmt(3)) se(par fmt(3))) keep(`end1' `end2' `end3' ) label starlevels( * 0.10 ** 0.05 *** 0.010)


*----------------------------------------------------------------------
*reduced-form model using MA patients

*loc pp ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
loc pp ami hf pn pnltprs pnltprs_X_ami pnltprs_X_hf pnltprs_X_pn

loc file HHeffort_MA_read30_rf
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

loc yv hashosp30

areg `yv' `pp' `sp', absorb(offid_nu) vce(cluster offid_nu)
*if hashosp==0
sum `yv' if e(sample)
loc mdv: display %9.2f `r(mean)'
loc ar2: display %9.2f `e(r2_a)'

qui test
loc fstat: display %9.2f `r(F)'

`out' ctitle(`l_`yv'') keep(ami hf pn `pp`n'') addtext(F statistic, `fstat', Mean dep. var., `mdv')



*----------------------------------------------------------------------
*TM patients : effect on HH efforts but with only penalty salience and condition indicators, but without the DD interaction of penalty salience * condition indicators. I will use these results to argue that the main results are driven largely by variation across hospitals (penalty salience) rather than between target and non-target conditions.
*----------------------------------------------------------------------
use `smpl_tm1', clear
*throughout the entire episode
loc outcome lnlov lnlov_1stwk1 lnlov_1stwk0 freq_tnv freq_tnv_1stwk1 freq_tnv_1stwk0 startHH_1day lnvtc_tr_pay lnvtc_tr_pay_1stwk1 lnvtc_tr_pay_1stwk0

loc pp ami hf pn pnltprs_c
*loc pp ami hf pn pnltprs pnltprs_X_ami pnltprs_X_hf pnltprs_X_pn

loc file HHeffort_TM_nointeract
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
