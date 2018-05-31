*HHeffort4_iv.do
*IV regression of resource spending on the readmission outcome using the 3 interaction terms [condition-specific penalty pressure X each target condition dummy] as instruments

*endogenous var: lepilvl_vtc_tr_pay

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults

cd `path'

use HHeffort_epilvl, clear

tab cond if tm==1
table cond if tm==1, contents(mean pnltprs_c mean pr_c mean shref_hj)
tab cond if tm==1 & hrrpcond_count < 2
table cond if tm==1 & hrrpcond_count < 2, contents(mean pnltprs_c mean pr_c mean shref_hj)

*regression macros
loc ages i.age5yr
loc riskhosp riskhosp_fall riskhosp_manyhos riskhosp_mental riskhosp_ge5 riskhosp_oth
loc priorcond priorcond_dis priorcond_impd priorcond_cath priorcond_pain priorcond_mem priorcond_inc
loc hrfactor hrfactor_alco hrfactor_drug hrfactor_smoke hrfactor_obese
loc overallst overallst_vbad overallst_bad overallst_tem
loc demog `ages' female white noassist livealone dual
loc comorbid ynch* `overallst' `hrfactor' `priorcond'
*ynch*  depressed

loc sp ami hf pn `riskhosp' `demog' `comorbid' i.fy

*all patients
loc pp1 pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
loc pp2 pr_c_X_ami pr_c_X_hf pr_c_X_pn


*define macros for key variables
loc yv hospoccur30
sum `yv'
loc nend 1
loc end lepilvl_vtc_tr_pay
loc ev1 lepilvl_vtc_tr_pay

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


forval nn = 1/2 {
  loc iv `pp`nn''

  di "-------- Use instruments: `iv' ----------"

  eststo y_n`nn': ivreg2 `yv' `sp' i.offid_nu (`end' = `iv') if tm==1 & hrrpcond_count < 2, cluster(offid_nu) first savefirst savefprefix(f_n`nn'_) gmm2s partial(i.fy i.offid_nu ) endog(`end')

  estimates dir

  mat fstat_n`nn' = e(first)

  *for each first stage, save in a separate file
  forval j = 1/`nend' {
    estadd scalar fs_`ev`j'' = fstat_n`nn'[4,`j'] : f_n`nn'_`ev`j''
  }

  foreach ev of varlist `end' {
    loc fsr_`ev' `fsr_`ev'' f_n`nn'_`ev'
  }
  loc ssr `ssr' y_n`nn'
  di "`ssr'"

  *get R-squared from first stage OLS regression
  foreach ev of varlist `end' {
    areg `ev' `iv' `sp' if tm==1 & hrrpcond_count < 2, absorb(offid_nu) vce(cluster offid_nu)
    estadd scalar fr2_`ev' = `e(r2)' : f_n`nn'_`ev'
  }
}

*for each first stage, save in a separate file
foreach ev of varlist `end' {
  di "`ev'"
  di "`fsr_`ev''"
  loc file iv1s_`ev'
  esttab `fsr_`ev'' using `reg'/`file'.csv, replace stats(N fr2_`ev' fs_`ev', fmt(0 3 3) label(Observations "R-squared" "F-statistic")) cells(b(star fmt(3)) se(par fmt(3))) keep(`pp1' `pp2') order(`pp1' `pp2') label starlevels( * 0.10 ** 0.05 *** 0.010)
}

*save 2nd stage reg
loc file iv2s
esttab `ssr' using `reg'/`file'.csv, replace stats(r2 jp N, fmt(3 3 0) label("R-squared" "J-statistic p-value" "Observations")) cells(b(star fmt(3)) se(par fmt(3))) keep(`end') label starlevels( * 0.10 ** 0.05 *** 0.010)


*reduced-form model
foreach d in "ami" "hf" "pn" {
  sum pnltprs_c if `d'==1
  loc pp_sd_`d' : di %9.3f `r(sd)'
}

loc pp1 pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
loc pp2 pr_c_X_ami pr_c_X_hf pr_c_X_pn

loc file HHeffort4_read
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

loc yv hospoccur30
forval n=1/2 {
  areg `yv' `pp`n'' `sp' if tm==1 & hrrpcond_count < 2, absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(ami hf pn `pp`n'') addtext(F statistic, `fstat', Mean dep. var., `mdv',SD of AMI penalty pressure, `pp_sd_ami', SD of HF penalty pressure, `pp_sd_hf', SD of PN penalty pressure, `pp_sd_pn', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}
