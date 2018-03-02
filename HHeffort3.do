*compare HH efforts by penalty pressure, or HRRP or non-HRRP condition
* Run regression analysis using the patient-week level data

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults

cd `path'

*--------------------------------
*identify top 10-12 conditions in the patient same and mean visit length for each condition to check the variation in visit length across penalized and non-penalized conditions
use HHeffort_week, clear

keep epiid ami hf copd pn ynch* ynel*
duplicates drop
count

foreach d in "ami" "hf" "copd" "pn" {
  sum ynch* if `d'==1
}

*using Charlson
* AMI: ynch1=1
* HF: ynch2=1
* COPD: ynch6=1
* pn: no indicator

foreach d in "ami" "hf" "copd" "pn" {
  sum ynel* if `d'==1
}
*using Elixhauser
* AMI: no indicator
* HF: ynel1=1
* COPD: ynel9=1
* pn: no indicator
*--------------------------------

*get mean visit length per week for each condition idenfied by the Elixhauser (has more specific categories)

*note: charlson or elixhauser comorbidity categories are not mutually exclusive

*use visit-level data to get the per-visit length not mean visit length per week
use HHeffort_visit, clear
capture rename pneu pn

lab var pn "Pneumonia"
lab var ami "AMI"
loc maxel 31
loc cond ynel

loc newmax = `maxel'
*burnsand-ulcerswounds
foreach v of varlist pn ami {
  loc newmax = `newmax' + 1
  gen `cond'`newmax' = `v'==1
  loc lab: variable label `v'
  lab var `cond'`newmax' "`lab'"
}

capture drop cat
gen cat = .
forval x = 1/`newmax' {
  di "`x'"
  replace cat = `x' if `cond'`x'==1
}
count if cat==.
*have zero for all elixhauser categories -> tag them as "Others"
loc newmax = `newmax' + 1
gen `cond'`newmax' = cat==.
lab var `cond'`newmax' "All other conditions (Zero in all Elixhauser indicators)"

preserve
keep epiid `cond'*
duplicates drop
count

keep `cond'*
outreg2 using `reg'/topcond.xls, replace sum(log) keep(`cond'*) eqkeep(N mean) label
restore

*create a visit sequence number
sort epiid visitdate visittime
capture drop vseq
bys epiid: gen vseq = _n

*tag first & last visits for the patient
gen fv_epi = vseq==1
bys epiid: gen totv = _N
gen lv_epi = vseq==totv
sum lov if fv_epi==1
sum lov if lv_epi==1
sum lov if fv_epi==0 & lv_epi==0
*the first visit tends to be longer

replace lov = . if fv_epi==1
gen snlov = lov if discipline=="SN"

collapse (mean) mvl = lov mvlsn = snlov hashosp `cond'*, by(epiid)

foreach v of varlist mvl mvlsn {
  forval x=1/`newmax' {
      sum `v' if `cond'`x'==1
      capture loc `v'_`x'= `r(mean)'
  }
}

preserve
keep `cond'* mvl mvlsn epiid
keep in 1/10
reshape long `cond', i(epiid mvl*) j(new)
drop `cond' epiid
rename new `cond'
keep if _n < `newmax'+1

foreach v of varlist mvl mvlsn  {
  forval x=1/`newmax' {
    *multiply by 60 to convert to minutes
    capture replace `v' = ``v'_`x''*60 if _n==`x'
  }
}

order `cond'
compress
outsheet using `reg'/mvl_bycond.csv, comma names replace
restore


*---------------------------------
*run regressions
use HHeffort_week, clear

*drop outliers:
sum mvl* nv* mtenure_esd mvisitorder, de
foreach v of varlist mvl* nv* mtenure_esd mvisitorder {
  sum `v', de
  replace `v' = . if `v' > `r(p99)'
}

*take logs for # visits & length of visits
foreach v of varlist mvl* nv* mtenure_* mvisitorder {
  capture drop ln`v'
  gen ln`v' = ln(`v' +1)
}

*exclude the last week
drop if wkidx == epilength_wk

histogram wkidx, discrete freq xti(Home health episode week) yti(Number of episodes) ti(Distribution of ongoing home health episodes by week) xlab(1(1)8)
graph export `gph'/hist_wkidx.eps, replace

*regression macros
loc ages i.age5yr
loc riskhosp riskhosp_fall riskhosp_manyhos riskhosp_mental riskhosp_ge5 riskhosp_oth
loc priorcond priorcond_dis priorcond_impd priorcond_cath priorcond_pain priorcond_mem priorcond_inc
loc hrfactor hrfactor_alco hrfactor_drug hrfactor_smoke hrfactor_obese
loc overallst overallst_vbad overallst_bad overallst_tem
loc ins ma_visit ma_epi dual
loc demog `ages' female white noassist livealone `ins'
loc comorbid ynch* `overallst' `hrfactor' `priorcond'
*ynch*  depressed

loc sp1 `riskhosp'
loc sp2 `sp1' `demog'
loc sp3 `sp2' `comorbid' i.fy

foreach d in "pn" {
  foreach yv of varlist lnmvl  {
    forval t=6/6 {
      di "`d': `yv' week `t'"
      reg `yv' z_pnltprs_`d' hrrpcond z_pnltprs_`d'_X_hrrpcond if hashosp==0  & wk==`t' & (`d'==1 | hrrp==0), vce(cluster offid_nu)
      tab hrrpcond if e(sample), summarize(z_pnltprs_`d'_X_hrrpcond)
      tab hrrpcond if e(sample), summarize(`yv')
    }
  }
}

*analysis of efforts separately by week among all patients
foreach d in "ami" "hf" "pn" {
  loc file HHeffort3_allpat_bywk_`d'
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons"

  *analysis of efforts separately by week among all patients
  foreach yv of varlist lnmvl* lnnv* pnv* lnmtenure_esd lnmvisitorder {
    forval t=1/8 {
      capture areg `yv' z_pnltprs_`d' hrrpcond z_pnltprs_`d'_X_hrrpcond `sp3' if wk==`t' & (`d'==1 | hrrp==0), absorb(offid_nu) vce(cluster offid_nu)

      capture sum `yv' if e(sample)
      loc mdv: display %9.2f `r(mean)'
      loc ar2: display %9.2f `e(r2_a)'

      `out' ctitle(Week `t') keep(z_pnltprs_`d'_X_hrrpcond) label addtext(Adjusted R-squared, `ar2', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y)
    }
  }
}

*analysis of efforts separately by week among patients not readmitted
foreach d in "ami" "hf" "pn" {
  loc file HHeffort3_norapat_bywk_`d'
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons"

  *analysis of efforts separately by week among all patients
  foreach yv of varlist lnmvl* lnnv* pnv* lnmtenure_esd lnmvisitorder {
    forval t=1/8 {
      capture areg `yv' z_pnltprs_`d' hrrpcond z_pnltprs_`d'_X_hrrpcond `sp3' if hashosp==0 & wk==`t' & (`d'==1 | hrrp==0), absorb(offid_nu) vce(cluster offid_nu)

      capture sum `yv' if e(sample)
      loc mdv: display %9.2f `r(mean)'
      loc ar2: display %9.2f `e(r2_a)'

      `out' ctitle(Week `t') keep(z_pnltprs_`d'_X_hrrpcond) label addtext(Adjusted R-squared, `ar2', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y)
    }
  }
}

*----------------------
*analysis of efforts per week in a single model by controlling for week index FE
loc l_lnmvl "Log Mean visit length per week"
loc l_lnmvlsn "Log Mean nurse visit length per week"
loc l_lnnvall "Log # All visits per week"
loc l_lnnvsn "Log # Nurse visits per week"
loc l_pnvall "% visits occurring in each week"
loc l_pnvsn "% nurse visits occurring in each week"
loc l_lnmtenure_esd "Log Mean tenure of nurses visiting each week"
loc l_lnmvisitorder "Log Mean order of visit during the nurse's day in each week"

loc sp1 `riskhosp'
loc sp2 `sp1' `demog'
loc sp3 `sp2' `comorbid' i.fy

foreach d in "ami" "hf" "pn" {
  loc file HHeffort3_allpat_`d'
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons"

  foreach yv of varlist lnmvl* lnnv* pnv* lnmtenure_esd lnmvisitorder {
    areg `yv' z_pnltprs_`d' hrrpcond z_pnltprs_`d'_X_hrrpcond `sp3' i.wkidx if (`d'==1 | hrrp==0), absorb(offid_nu) vce(cluster offid_nu)
    *if hashosp==0
    sum `yv' if e(sample)
    loc mdv: display %9.2f `r(mean)'
    loc ar2: display %9.2f `e(r2_a)'

    `out' ctitle(`l_`yv'') keep(z_pnltprs_`d'_X_hrrpcond) label addtext(Adjusted R-squared, `ar2', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Home health week FE, Y)
  }
}

*non-readmitted pats
foreach d in "ami" "hf" "pn" {
  loc file HHeffort3_norapat_`d'
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons"

  foreach yv of varlist lnmvl* lnnv* pnv* lnmtenure_esd lnmvisitorder {
    areg `yv' z_pnltprs_`d' hrrpcond z_pnltprs_`d'_X_hrrpcond `sp3' i.wkidx if hashosp==0 & (`d'==1 | hrrp==0), absorb(offid_nu) vce(cluster offid_nu)

    sum `yv' if e(sample)
    loc mdv: display %9.2f `r(mean)'
    loc ar2: display %9.2f `e(r2_a)'

    `out' ctitle(`l_`yv'') keep(z_pnltprs_`d'_X_hrrpcond) label addtext(Adjusted R-squared, `ar2', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Home health week FE, Y)
  }
}
