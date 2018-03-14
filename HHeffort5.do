*Compare HH efforts, readmissions (Reduced form model) by penalty pressure & TA vs MA patients (don't differentiate by penalty vs non-penalty condition)

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults

cd `path'

*identify patients who have unplanned readmissions & one of the specialty cohorts defined by Horwitz 2014 (see supplements in doi:10.7326/M13-3000)
*use the inpatient diagnosis code file that contains CCS category, and identify the unique CCS category for
use inpat_dx, clear

*exclude Diagnosis Categories that are Always Planned regardless of Procedure (Table PR2 in Horwitz 2014 Appendix)
destring ccs, replace
drop if ccs==45 | ccs==194 | ccs==196 | ccs==254

*specialty cohorts
gen cardioresp = ccs==103 | ccs==108 | ccs==122 | ccs==125 | ccs==127 | ccs==128 | ccs==131

gen cardiovas = ccs==96 | ccs==97 | (ccs >= 100 & ccs <= 102) | (ccs >= 104 & ccs <= 107) | (ccs >= 114 & ccs <= 117) | ccs==213

gen neuro = (ccs >= 78 & ccs <= 83) | ccs==85 | ccs==95 | (ccs>=109 & ccs <= 113) | ccs==216 | ccs==227 | ccs==233

gen medicine = (ccs >= 1 & ccs <= 10) | (ccs >= 46 & ccs <= 64) | (ccs >= 76 & ccs <= 77) | ccs==84 | (ccs >= 86 & ccs <= 94) | (ccs >= 98 & ccs <= 99) | (ccs >= 118 & ccs <= 121) | (ccs >= 123 & ccs <= 124) | ccs==126 | ccs==129 | ccs==130 | (ccs >= 132 & ccs <= 149) | (ccs >= 151 & ccs <= 173) | ccs==175 | (ccs >= 197 & ccs <= 212) | (ccs >= 214 & ccs <= 215) | ccs==217 | (ccs >= 225 & ccs <= 226) | (ccs >= 228 & ccs <= 232) | (ccs >= 234 & ccs <= 253) | (ccs >= 255 & ccs <= 259) | ccs==653 | (ccs >= 660 & ccs <= 663)

collapse (max) cardioresp cardiovas neuro medicine , by(clientid socdate)

tempfile nontarget
save `nontarget'

*---------------------------------
use HHeffort_week, clear
capture rename pneu pn

*if not HRRP conditions, should be one of the 4 diagnosis CCS categories
merge m:1 clientid socdate_e using `nontarget', keep(1 3) nogen
drop if hrrpcond==0 & !(cardioresp==1 | cardiovas==1 | neuro==1 | medicine==1)

*how many MA patients
gen ma = ma_epi==1 | ma_visit==1
gen tm = 1-ma
lab var ma "Medicare Advantage"
lab var tm "Traditional Medicare"

* tag a week of readmission
bys epiid: egen lwk = max(wkidx)
gen hospoccur = hashosp==1 & wkidx==lwk
tab hospoccur
drop lwk
*list epiid wkidx hashosp hospoccur in 1/50

*drop copd patients
drop if copd==1

*estimate triple DDD: penalty pressure X HRRP condition X MA patients
capture drop z_pnltprs_X_tm
gen z_pnltprs_X_tm = z_pnltprs * tm
lab var z_pnltprs_X_tm "Overall penalty pressure X TM"

* drop one episode
drop if z_pnltprs==.

sort epiid wkidx

tempfile an
save `an'

*---------------------------------
*get more outcome variables:
/* - Indicator for whether the HH episode started within 1 day from hospital discharge
- Frequency of nurse visits in the episode
- Frequency of nurse visits in the first week
- Frequency of any visits in the episode (exposure)
- Number of handoffs
- % FT nurse visits */
use `an', clear
keep epiid
duplicates drop
tempfile epi
save `epi'

use HHeffort_visit, clear
merge m:1 epiid using `epi', keep(3) nogen

*Indicator for whether the HH episode started within 1 day from hospital discharge
gen time2hh = fvd - inpat_dcdate_e
replace time2hh = . if time2hh < 0
gen startHH_1day = time2hh <=1 if time2hh!=.

*get 30-day readmission indicator
gen days2hosp = firsthospdate - inpat_dcd
gen hashosp30 = hashosp * (days2hosp <= 30)

sort epiid visitdate
*list epiid visitd hashosp firsthospdate inpat_dcd days2hosp hashop30 in 1/50

*get frequency of visits

*count the total length of episode from first visit date to hospitalization or last visit date, whichever is later
capture drop epilength
capture egen llvd = rowmax(firsthospdate lvd)
format llvd %d
gen epilength = llvd - fvd + 1

gen freq_tnvall = tnvall/epilength
gen freq_tnvsn = tnvsn/epilength
gen freq_nvall = nvall/7
gen freq_nvsn = nvsn/7

* create episode-level Frequency of nurse visits in the first week
foreach v of varlist freq_nvall freq_nvsn {
  capture drop x
  capture drop `v'_wk1
  gen x = `v' if wkidx==1
  bys epiid: egen `v'_wk1 = max(x)
}
drop freq_nvall freq_nvsn

*get mean # visits and length of visits during the episode (not per week)

* construct total # visits during episode
capture drop tnvall tnvsn
bys epiid: egen tnvall = sum(1)
* later drop outliers: if tnv > 60
bys epiid: egen tnvsn = sum(discipline=="SN")

*mean visit length by time periods
capture drop mvl mvlsn
bys epiid: egen mvl = mean(lov) if fvd!=visitdate & lvd!=visitdate
bys epiid: egen mvlsn = mean(lov) if  fvd!=visitdate & lvd!=visitdate & (discipline=="SN")
foreach v of varlist mvl mvlsn {
  bys epiid: egen `v'2 = max(`v')
  drop `v'
  rename `v'2 `v'
}
*mean LOV is missing if the episode only has first and last visit dates

sort epiid visitdate

tempfile visit
save `visit'

*get # handoffs
use `visit', clear
keep if discipline=="SN"
sort epiid visitdate visittime
bys epiid: gen lastworker = payrollno[_n-1] if _n > 1
bys epiid: gen ho = payrollno!=lastworker if _n > 1
bys epiid: egen tho = sum(ho)

* create % handoff visits during the episode
gen tfho = tho / tnvsn

*get % FT nurse visits
gen nv_ft = discipline=="SN" & status=="VFT"
bys epiid: egen tnvsn_ft = sum(nv_ft)
gen tfnv = tnvsn_ft / tnvsn
sum tfnv

list epiid visitdate discipline jobcode status tnvsn tnvsn_ft firsthospdate tfnv if epiid==537956

*wkidx
*leave observations unique at the episode level
keep epiid startHH_1day freq_tnvall freq_tnvsn freq_nvall_wk1 freq_nvsn_wk1 tho tfnv hashosp30 mvl* tnv* tfho
duplicates drop

tempfile moreoutcome
save `moreoutcome'

use `an', clear
drop mvl* tnv* lnmvl*
merge m:1 epiid using `moreoutcome', keep(3) nogen

*take logs for frequency of visits, # handoffs, mean # visits & length of visits
foreach v of varlist freq* tho mvl* tnv* tho {
  capture drop ln`v'
  gen ln`v' = ln(`v' +1)
}

*drop outliers:
sum mvl* nv* , de
foreach v of varlist mvl* nv* freq* {
  qui sum `v', de
  replace `v' = . if `v' > `r(p99)'
}

sort epiid wkidx

lab var lnmvl "Log Mean visit length"
lab var lnmvlsn "Log Mean nurse visit length"
lab var lntnvall "Log Total number of visits"
lab var lntnvsn "Log Total number of nurse visits"
lab var lnfreq_tnvall "Ln Frequency of all visits"
lab var lnfreq_tnvsn "Ln Frequency of nurse visits"
lab var lnfreq_nvall_wk1 "Ln Frequency of all visits in first week"
lab var lnfreq_nvsn_wk1 "Ln Frequency of nurse visits in first week"
lab var tfho "Fraction of nurse handoffs"
lab var tfnv "Fraction of full-time nurse visits"
lab var startHH_1day "Starting home health within one day from hospital discharge"

compress
save HHeffort5, replace

*---------------------------------
*run regressions with interaction of TM dummy and penalty pressure

*regression macros
loc ages i.age5yr
loc riskhosp riskhosp_fall riskhosp_manyhos riskhosp_mental riskhosp_ge5 riskhosp_oth
loc priorcond priorcond_dis priorcond_impd priorcond_cath priorcond_pain priorcond_mem priorcond_inc
loc hrfactor hrfactor_alco hrfactor_drug hrfactor_smoke hrfactor_obese
loc overallst overallst_vbad overallst_bad overallst_tem
loc demog `ages' female white noassist livealone dual
loc comorbid ynch* `overallst' `hrfactor' `priorcond'
*ynch*  depressed

*analysis of efforts per week in a single model by controlling for week index FE
loc l_lnmvl "Log Mean visit length"
loc l_lnmvlsn "Log Mean nurse visit length"
loc l_hospoccur "Hospital readmission indicator"
loc l_lnmvl "Log Mean visit length"
loc l_lnmvlsn "Log Mean nurse visit length"
loc l_lntnvall "Log Total number of visits"
loc l_lntnvsn "Log Total number of nurse visits"
loc l_lnfreq_tnvall "Ln Frequency of all visits"
loc l_lnfreq_tnvsn "Ln Frequency of nurse visits"
loc l_lnfreq_nvall_wk1 "Ln Frequency of all visits in first week"
loc l_lnfreq_nvsn_wk1 "Ln Frequency of nurse visits in first week"
loc l_tfho "Fraction of nurse handoffs"
loc l_tfnv "Fraction of full-time nurse visits"
loc l_startHH_1day "Starting home health within one day from hospital discharge"


loc sp1 `riskhosp'
loc sp2 `sp1' `demog'
loc sp3 `sp2' `comorbid'

*leave only one obs per episode
use HHeffort5, clear

*since among HF patients, if they are MA, always have zero penalty pressure, so the triple DDD is not identified below
tab ma if hf==1, summarize(z_pnltprs)
tab ma if ami==1, summarize(z_pnltprs)
tab ma if pn==1, summarize(z_pnltprs)
table ma, contents(mean ami mean hf mean pn)

loc outcome lnmvl lnmvlsn lntnvall lntnvsn lnfreq_tnvall lnfreq_tnvsn lnfreq_nvall_wk1 lnfreq_nvsn_wk1 tfho tfnv startHH_1day

drop if wkidx == epilength_wk
keep epiid tm z_pnltprs_X_tm `sp1' age5yr female white noassist livealone dual `comorbid' offid_nu fy `outcome' hashosp hrrpcond cardioresp cardiovas neuro medicine
duplicates drop
duplicates tag epiid, gen(dup)
assert dup==0
drop dup

tempfile an2
save `an2'

loc sp3 `sp2' `comorbid' i.fy

loc file HHeffort5_allpat
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons label"

foreach yv of varlist `outcome' {
  areg `yv' tm z_pnltprs_X_tm `sp3' , absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(tm z_pnltprs_X_tm) addtext(F statistic, `fstat', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}

* by penalty condition vs non-penalty condition patients
forval x = 0/1 {
  loc file HHeffort5_allpat_c`x'
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons label"

  foreach yv of varlist `outcome' {
    areg `yv' tm z_pnltprs_X_tm `sp3' if hrrpcond==`x', absorb(offid_nu) vce(cluster offid_nu)
    *if hashosp==0
    sum `yv' if e(sample)
    loc mdv: display %9.2f `r(mean)'
    loc ar2: display %9.2f `e(r2_a)'

    test
    loc fstat: display %9.2f `r(F)'

    `out' ctitle(`l_`yv'') keep(tm z_pnltprs_X_tm) addtext(F statistic, `fstat', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
  }
}


*--------
*non-readmitted pats
loc file HHeffort5_norapat
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons label"

foreach yv of varlist `outcome' {
  areg `yv' tm z_pnltprs_X_tm `sp3' if hashosp==0, absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(tm z_pnltprs_X_tm) addtext(F statistic, `fstat', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}

* by penalty condition vs non-penalty condition patients
forval x = 0/1 {
  loc file HHeffort5_norapat_c`x'
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons label"

  foreach yv of varlist `outcome' {
    areg `yv' tm z_pnltprs_X_tm `sp3' if hashosp==0, absorb(offid_nu) vce(cluster offid_nu)
    *if hashosp==0
    sum `yv' if e(sample)
    loc mdv: display %9.2f `r(mean)'
    loc ar2: display %9.2f `e(r2_a)'

    test
    loc fstat: display %9.2f `r(F)'

    `out' ctitle(`l_`yv'') keep(tm z_pnltprs_X_tm) addtext(F statistic, `fstat', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
  }
}

*------------------------

*for readmission indicator, include last week of episode
use HHeffort5, clear

loc file HHeffort5_read
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

loc yv hospoccur
areg `yv' tm z_pnltprs_X_tm `sp3' i.wkidx if hashosp30==1, absorb(offid_nu) vce(cluster offid_nu)
*if hashosp==0
sum `yv' if e(sample)
loc mdv: display %9.2f `r(mean)'
loc ar2: display %9.2f `e(r2_a)'

test
loc fstat: display %9.2f `r(F)'

`out' ctitle(`l_`yv'') keep(tm z_pnltprs_X_tm) addtext(F statistic, `fstat', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Home health week FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)

loc penalty0 "Non-penalty conditions"
loc penalty1 "Penalty conditions"
forval x=0/1 {
  loc yv hospoccur
  areg `yv' tm z_pnltprs_X_tm `sp3' i.wkidx if hashosp30==1 & hrrpcond==`x', absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`penalty`x'') keep(tm z_pnltprs_X_tm) addtext(F statistic, `fstat', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Home health week FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}
*--------
*non-readmitted pats

foreach yv of varlist lnmvl* lnnv* {
  areg `yv' ami hf pn tm pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn ami_tm hf_tm pn_tm pnltprs_c_X_ami_X_tm pnltprs_c_X_hf_X_tm pnltprs_c_X_pn_X_tm `sp3' i.wkidx if hashosp==0 & wkidx != epilength_wk, absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(ami hf pn tm pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn ami_tm hf_tm pn_tm pnltprs_c_X_ami_X_tm pnltprs_c_X_hf_X_tm pnltprs_c_X_pn_X_tm) addtext(F statistic, `fstat', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Home health week FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}
