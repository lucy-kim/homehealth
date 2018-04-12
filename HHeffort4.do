*compare HH efforts, readmissions (Reduced form model) by penalty pressure & HRRP or non-HRRP condition & TA vs MA patients
* Run regression analysis using the patient-week level data

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

*multiple CCS categories per admission
/* sort clientid socdate ccs
bys clientid socdate ccs: gen i = _n==1
bys clientid socdate: egen si = sum(i)
tab si */

collapse (max) cardioresp cardiovas neuro medicine , by(clientid socdate)

tempfile nontarget
save `nontarget'

*---------------------------------
use HHeffort_week, clear
capture rename pneu pn

*if not HRRP conditions, should be one of the 4 diagnosis CCS categories
/* merge m:1 clientid socdate_e using `nontarget', keep(1 3) nogen
drop if hrrpcond==0 & !(cardioresp==1 | cardiovas==1 | neuro==1 | medicine==1) */

*take logs for # visits & length of visits
foreach v of varlist mvl* nv* mtenure_* mvisitorder {
  capture drop ln`v'
  gen ln`v' = ln(`v' +1)
}

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

*drop if patients have more than one of HRRP conditions
egen x = rowtotal(ami hf pn)
drop if x > 1
drop x

*drop copd patients
drop if copd==1

*reshape long so that we have condition-patient-week level data by just dissecting and appending sample of patients for each HRRP condition & non-target condition
count

gen cond = 4
gen pnltprs_c = .
loc i = 1
foreach d in "ami" "hf" "pn" {
  replace cond = `i' if `d'==1
  loc i = `i'+1
  replace pnltprs_c = pnltprs_`d' if `d'==1
}
replace pnltprs_c = 0 if hrrpcond==0

count if cond < 4
lab define ll 1 "AMI" 2 "HF" 3 "PN" 4 "Non-target"
lab val cond ll

sort epiid wkidx

loc uami "AMI"
loc uhf "HF"
loc upn "PN"

*create interaction of penalty pressure X each HRRP condition
foreach d in "ami" "hf" "pn" {
  capture drop pnltprs_c_X_`d'
  gen pnltprs_c_X_`d' = pnltprs_c *`d'

  lab var pnltprs_c_X_`d' "Condition-specific penalty pressure X `u`d''"
  lab var `d' "Indicator for `u`d''"
}
lab var pnltprs_c "Condition-specific penalty pressure"

*estimate triple DDD: penalty pressure X HRRP condition X MA patients
foreach d in "ami" "hf" "pn" {
  capture drop pnltprs_c_X_`d'_X_tm
  gen pnltprs_c_X_`d'_X_tm = pnltprs_c *`d' * tm

  capture drop `d'_tm
  gen `d'_tm = `d' * tm

  lab var pnltprs_c_X_`d'_X_tm "Condition-specific penalty pressure X `u`d'' X TM"
  lab var `d'_tm "`u`d'' X TM"
}
capture drop pnltprs_c_X_tm
gen pnltprs_c_X_tm = pnltprs_c * tm
lab var pnltprs_c_X_tm "Condition-specific penalty pressure X TM"

* drop one episode
drop if pnltprs_c==.

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
tab time2hh
*all the episodes started HH within 14 days from hospital discharge
gen startHH_1day = time2hh <=1 if time2hh!=.

*get 30-day readmission indicator
gen days2hosp = firsthospdate - inpat_dcd
gen hashosp30 = hashosp * (days2hosp <= 30)

* construct total # visits during episode
capture drop tnvall tnvsn
bys epiid: egen tnvall = sum(1)
* later drop outliers: if tnv > 60
bys epiid: egen tnvsn = sum(discipline=="SN")

*first drop if the episode has zero nurse visits
drop if tnvsn==0

/* *drop if the total number of visits > 99th percentile - if i drop these episodes, the HF patients always come from zero penalty pressure hospitals and the interaction of pnltprs_cXhf drop out
preserve
keep epiid tnvall tnvsn
duplicates drop
sum tnvall tnvsn, de
sum tnvall, de
loc p99 = `r(p99)'
sum tnvsn if tnvall <= `p99'
keep if tnvall <= `p99'
keep epiid
tempfile epi_keep
save `epi_keep'
restore

merge m:1 epiid using `epi_keep', keep(3) nogen */

tempfile tmp
save `tmp'

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

*mean visit length by time periods
bys epiid: gen visitorder = _n
table visitorder if discipline=="SN" & lvd!=visitdate, contents(mean lov p75 lov)

table wkidx if discipline=="SN" & lvd!=visitdate, contents(mean lov p75 lov)

capture drop mvl mvlsn
bys epiid: egen mvl = mean(lov) if lvd!=visitdate
bys epiid: egen mvlsn = mean(lov) if  lvd!=visitdate & (discipline=="SN")

foreach v of varlist mvl mvlsn {
  bys epiid: egen `v'2 = max(`v')
  drop `v'
  rename `v'2 `v'
}
sort epiid visitdate
list epiid visitdate lov discipline mvl* in 1/30

*get the any visit & nurse visit length in the first week
bys epiid: egen mvl_wk1 = mean(lov) if wkidx==1 & lvd!=visitdate
bys epiid: egen mvlsn_wk1 = mean(lov) if wkidx==1 & discipline=="SN" & lvd!=visitdate

foreach v of varlist mvl_wk1 mvlsn_wk1 {
  bys epiid: egen `v'2 = max(`v')
  drop `v'
  rename `v'2 `v'
}
sum mvl*

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

keep epiid startHH_1day freq_tnvall freq_tnvsn freq_nvall_wk1 freq_nvsn_wk1 tho tfnv hashosp30 mvl* tnv* tfho inpat_dcd
duplicates drop

tempfile moreoutcome
save `moreoutcome'

use `an', clear
drop mvl* tnv*
merge m:1 epiid using `moreoutcome', keep(3) nogen

*take logs for frequency of visits, # handoffs
foreach v of varlist freq* tho mvl* tnv* tho {
  capture drop ln`v'
  gen ln`v' = ln(`v' +1)
}

/* *drop outliers:
sum mvl* nv* , de
foreach v of varlist mvl* nv* freq* {
  qui sum `v', de
  replace `v' = . if `v' > `r(p99)'
} */

*estimate DiD: penalty pressure X MA patients
capture drop pnltprs_X_tm
gen pnltprs_X_tm = pnltprs * tm
lab var pnltprs_X_tm "Overall penalty pressure X TM"


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


*replace the 2012 hypothetical penalty rate with 2013 penalty rate & reconstruct the penalty pressure
merge m:1 offid_nu prvdr_num using HRRPpnlty_pressure_hj_2012, keepusing(pnltprs13 z_pnltprs13) keep(1 3) nogen

*estimate DiD: 2013 penalty pressure X MA patients
capture drop pnltprs13_X_tm
gen pnltprs13_X_tm = pnltprs13 * tm
lab var pnltprs13_X_tm "Overall penalty pressure in 2013 X TM"
lab var pnltprs13 "Overall penalty pressure in 2013"

*add the summary resource spending measure for each episode
capture drop epilvl_vtc-vtc_tr_payST
merge m:1 epiid using resource_index, keep(1 3) nogen

foreach v of varlist epilvl_vtc epilvl_vtc_tr epilvl_vtc_tr_pay {
  qui count if `v'==0
  di "`v' has `r(N)' obs with 0 values"
  gen l`v' = log(`v'+1)
  sum `v', de
  drop if `v' < `r(p1)'
}
lab var lepilvl_vtc_tr_pay "Log Total visit costs (incl. transportation and pays)"
lab var lepilvl_vtc_tr "Log Total visit costs (incl. transportation)"
lab var lepilvl_vtc "Log Total visit costs"

preserve
use office, clear
keep offid_nu addr_st
duplicates drop
tempfile office_st
save `office_st'
restore

merge m:1 offid_nu using `office_st', keep(1 3) nogen
gen con = addr_st=="NC" | addr_st=="NJ" | addr_st=="VT"

sort epiid wkidx

gen epidur = log(epilength+1)
lab var epidur "Ln Length of episode"
lab var lnmvl_wk1 "Ln Mean visit length in the first week"
lab var lnmvlsn_wk1 "Ln Mean nurse visit length in the first week"
lab var lnmvl "Ln Mean visit length"
lab var lnmvlsn "Ln Mean nurse visit length"

compress
save HHeffort4, replace


*---------------------------------
*run regressions with interaction of condition dummies and penalty pressure

*regression macros
loc ages i.age5yr
loc riskhosp riskhosp_fall riskhosp_manyhos riskhosp_mental riskhosp_ge5 riskhosp_oth
loc priorcond priorcond_dis priorcond_impd priorcond_cath priorcond_pain priorcond_mem priorcond_inc
loc hrfactor hrfactor_alco hrfactor_drug hrfactor_smoke hrfactor_obese
loc overallst overallst_vbad overallst_bad overallst_tem
loc demog `ages' female white noassist livealone dual
loc comorbid ynch* `overallst' `hrfactor' `priorcond'
*ynch*  depressed

*analysis of efforts in a single model at the episode level
loc l_hospoccur "Hospital readmission indicator"
loc l_lnmvl "Ln Mean visit length"
loc l_lnmvlsn "Ln Mean nurse visit length"
loc l_lnmvl_wk1 "Ln Mean visit length in the first week"
loc l_lnmvlsn_wk1 "Ln Mean nurse visit length in the first week"
loc l_lntnvall "Ln Total number of visits"
loc l_lntnvsn "Ln Total number of nurse visits"
loc l_lnfreq_tnvall "Ln Frequency of all visits"
loc l_lnfreq_tnvsn "Ln Frequency of nurse visits"
loc l_lnfreq_nvall_wk1 "Ln Frequency of all visits in first week"
loc l_lnfreq_nvsn_wk1 "Ln Frequency of nurse visits in first week"
loc l_tfho "Fraction of nurse handoffs"
loc l_tfnv "Fraction of full-time nurse visits"
loc l_startHH_1day "Starting home health within one day from hospital discharge"
loc l_lepilvl_vtc_tr_pay "Ln Total visit costs (incl. transportation and pays)"
loc l_lepilvl_vtc_tr "Ln Total visit costs (incl. transportation)"
loc l_lepilvl_vtc "Ln Total visit costs"
loc l_epidur "Ln Episode length"

loc sp1 `riskhosp'
loc sp2 `sp1' `demog'
loc sp3 `sp2' `comorbid' i.fy

* leave only one obs per episode
loc outcome lepilvl_vtc_tr lepilvl_vtc_tr_pay lnmvl* lnmvlsn* lntnvall lntnvsn lnfreq_tnvall lnfreq_tnvsn lnfreq_nvall_wk1 lnfreq_nvsn_wk1 tfho tfnv startHH_1day epidur

use HHeffort4, clear

/* *drop MA patients
drop if ma==1 */

keep epiid tm z_pnltprs* `sp1' age5yr female white noassist livealone dual `comorbid' offid_nu fy `outcome' hashosp hrrpcond lepilvl* ami hf pn pnltprs pnltprs13 pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn pnltprs_c

duplicates drop
duplicates tag epiid, gen(dup)
assert dup==0
drop dup

tempfile epilvl
save `epilvl'

*------------------------------------
* use difference in penalty pressure between target and non-target conditions
*------------------------------------
*non-readmitted pats

use `epilvl', clear

loc sp3 `sp2' `comorbid' i.fy
loc outcome epidur lnmvl lnmvlsn lnmvl_wk1 lnmvlsn_wk1 lnfreq_tnvall lnfreq_tnvsn lnfreq_nvall_wk1 lnfreq_nvsn_wk1 lepilvl_vtc_tr_pay tfho tfnv startHH_1day
*lepilvl_vtc_tr lntnvall lntnvsn
loc pp ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn

loc file HHeffort4_norapat
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

foreach d in "ami" "hf" "pn" {
  sum pnltprs_c if `d'==1
  loc pp_sd_`d' : di %9.3f `r(sd)'
}

foreach yv of varlist `outcome' {
  areg `yv' `pp' `sp3' if hashosp==0 & tm==1, absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of AMI penalty pressure, `pp_sd_ami', SD of HF penalty pressure, `pp_sd_hf', SD of PN penalty pressure, `pp_sd_pn', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}

* use the interaction with aggregate 2012 penalty pressure, not the condition-specific ones
loc pp ami hf pn pnltprs pnltprs_X_ami pnltprs_X_hf pnltprs_X_pn

foreach d in "ami" "hf" "pn" {
  gen pnltprs_X_`d' = pnltprs * `d'
}
lab var pnltprs "Overall penalty pressure in 2012"
lab var pnltprs_X_ami "Penalty pressure X AMI"
lab var pnltprs_X_hf "Penalty pressure X HF"
lab var pnltprs_X_pn "Penalty pressure X PN"

sum pnltprs
loc pp_sd : di %9.3f `r(sd)'

*non-readmitted pats
loc file HHeffort4_norapat_agpp12
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp' `sp3' if hashosp==0 & tm==1, absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}

*use the target condition dummy instead of each condition dummy

loc pp hrrpcond pnltprs pnltprs_X_hrrpcond

loc d hrrpcond
gen pnltprs_X_`d' = pnltprs * `d'

lab var pnltprs "Overall penalty pressure in 2012"
lab var pnltprs_X_hrrpcond "Penalty pressure X Target condition"

sum pnltprs
loc pp_sd : di %9.3f `r(sd)'

loc file HHeffort4_norapat_agpp12_cond
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp' `sp3' if hashosp==0 &  tm==1, absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}


* use the interaction with aggregate 2013 penalty pressure
loc pp ami hf pn pnltprs13 pnltprs13_X_ami pnltprs13_X_hf pnltprs13_X_pn

foreach d in "ami" "hf" "pn" {
  gen pnltprs13_X_`d' = pnltprs13 * `d'
}
lab var pnltprs13_X_ami "Penalty pressure X AMI"
lab var pnltprs13_X_hf "Penalty pressure X HF"
lab var pnltprs13_X_pn "Penalty pressure X PN"

sum pnltprs13
loc pp_sd : di %9.3f `r(sd)'

loc file HHeffort4_norapat_agpp13
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp' `sp3' if hashosp==0 & tm==1, absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}

*--------------------------
*all patients

use `epilvl', clear

loc sp3 `sp2' `comorbid' i.fy
loc outcome epidur lnmvl lnmvlsn lnmvl_wk1 lnmvlsn_wk1 lnfreq_tnvall lnfreq_tnvsn lnfreq_nvall_wk1 lnfreq_nvsn_wk1 lepilvl_vtc_tr_pay tfho tfnv startHH_1day
*lepilvl_vtc_tr lntnvall lntnvsn
loc pp ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn

loc file HHeffort4_allpat
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

foreach d in "ami" "hf" "pn" {
  sum pnltprs_c if `d'==1
  loc pp_sd_`d' : di %9.3f `r(sd)'
}

foreach yv of varlist `outcome' {
  areg `yv' `pp' `sp3' if tm==1, absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of AMI penalty pressure, `pp_sd_ami', SD of HF penalty pressure, `pp_sd_hf', SD of PN penalty pressure, `pp_sd_pn', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}


* use the interaction with aggregate 2012 penalty pressure, not the condition-specific ones
loc pp ami hf pn pnltprs pnltprs_X_ami pnltprs_X_hf pnltprs_X_pn

foreach d in "ami" "hf" "pn" {
  gen pnltprs_X_`d' = pnltprs * `d'
}
lab var pnltprs "Overall penalty pressure in 2012"
lab var pnltprs_X_ami "Penalty pressure X AMI"
lab var pnltprs_X_hf "Penalty pressure X HF"
lab var pnltprs_X_pn "Penalty pressure X PN"

sum pnltprs
loc pp_sd : di %9.3f `r(sd)'

loc file HHeffort4_allpat_agpp12
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp' `sp3' if tm==1, absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}

*use the target condition dummy instead of each condition dummy

loc pp hrrpcond pnltprs pnltprs_X_hrrpcond

loc d hrrpcond
gen pnltprs_X_`d' = pnltprs * `d'

lab var pnltprs "Overall penalty pressure in 2012"
lab var pnltprs_X_hrrpcond "Penalty pressure X Target condition"

sum pnltprs
loc pp_sd : di %9.3f `r(sd)'

loc file HHeffort4_allpat_agpp12_cond
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp' `sp3' if tm==1, absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}



* use the interaction with aggregate 2013 penalty pressure
loc pp ami hf pn pnltprs13 pnltprs13_X_ami pnltprs13_X_hf pnltprs13_X_pn

foreach d in "ami" "hf" "pn" {
  gen pnltprs13_X_`d' = pnltprs13 * `d'
}
lab var pnltprs13_X_ami "Penalty pressure X AMI"
lab var pnltprs13_X_hf "Penalty pressure X HF"
lab var pnltprs13_X_pn "Penalty pressure X PN"

sum pnltprs13
loc pp_sd : di %9.3f `r(sd)'

loc file HHeffort4_allpat_agpp13
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp' `sp3' if tm==1, absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}

*for readmission outcome
use HHeffort4, clear

gen hospoccur30 = hospoccur* hashosp30
lab var hospoccur30 "30-day hospital readmission indicator"
loc l_hospoccur30 "30-day hospital readmission indicator"

foreach d in "ami" "hf" "pn" {
  gen pnltprs_X_`d' = pnltprs * `d'
}
lab var pnltprs "Overall penalty pressure in 2012"
lab var pnltprs_X_ami "Penalty pressure X AMI"
lab var pnltprs_X_hf "Penalty pressure X HF"
lab var pnltprs_X_pn "Penalty pressure X PN"

foreach d in "ami" "hf" "pn" {
  gen pnltprs13_X_`d' = pnltprs13 * `d'
}
lab var pnltprs13_X_ami "Penalty pressure X AMI"
lab var pnltprs13_X_hf "Penalty pressure X HF"
lab var pnltprs13_X_pn "Penalty pressure X PN"


foreach d in "ami" "hf" "pn" {
  sum pnltprs_c if `d'==1
  loc pp_sd_`d' : di %9.3f `r(sd)'
}

sum pnltprs
loc pp_sd2 : di %9.3f `r(sd)'
sum pnltprs13
loc pp_sd3 : di %9.3f `r(sd)'

loc pp1 ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
loc pp2 ami hf pn pnltprs pnltprs_X_ami pnltprs_X_hf pnltprs_X_pn
loc pp3 ami hf pn pnltprs13 pnltprs13_X_ami pnltprs13_X_hf pnltprs13_X_pn

loc file HHeffort4_read
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

loc yv hospoccur30
forval n=1/3 {
  areg `yv' `pp`n'' `sp3' i.wkidx if tm==1, absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp`n'') addtext(F statistic, `fstat', Mean dep. var., `mdv',SD of AMI penalty pressure, `pp_sd_ami', SD of HF penalty pressure, `pp_sd_hf', SD of PN penalty pressure, `pp_sd_pn', SD of penalty pressure, `pp_sd', Office FE, Y, Home health week FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}


*----------------------------------------
*triple Diff in DIff: TM vs MA patients & target vs non-target conditions
*----------------------------------------

use `epilvl', clear

loc pp ami hf pn tm pnltprs ami_tm tm_pnltprs pnltprs_X_ami pnltprs_X_ami_tm hf_tm pnltprs_X_hf pnltprs_X_hf_tm pn_tm pnltprs_X_pn pnltprs_X_pn_tm

foreach d in "ami" "hf" "pn" {
  gen pnltprs_X_`d' = pnltprs * `d'
}
lab var pnltprs "Overall penalty pressure in 2012"
lab var pnltprs_X_ami "Penalty pressure X AMI"
lab var pnltprs_X_hf "Penalty pressure X HF"
lab var pnltprs_X_pn "Penalty pressure X PN"

sum pnltprs
loc pp_sd : di %9.3f `r(sd)'


foreach d in "ami" "hf" "pn" {
  gen `d'_tm = `d'*tm
  lab var `d'_tm "`d' X TM"

  gen pnltprs_X_`d'_tm = pnltprs_X_`d' * tm
  lab var pnltprs_X_`d'_tm "Penalty pressure X `d' X TM"
}

gen tm_pnltprs = tm * pnltprs
lab var tm_pnltprs "TM X Penalty pressure"


loc file HHeffort4_allpat_agpp12_triple
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons label"

foreach yv of varlist `outcome' {
  areg `yv' `pp' `sp3', absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}

loc file HHeffort4_norapat_agpp12_triple
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons label"

foreach yv of varlist `outcome' {
  areg `yv' `pp' `sp3' if hashosp==0, absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}

*for readmission outcome
use HHeffort4, clear

gen hospoccur30 = hospoccur* hashosp30
lab var hospoccur30 "30-day hospital readmission indicator"
loc l_hospoccur30 "30-day hospital readmission indicator"

loc pp ami hf pn tm pnltprs ami_tm tm_pnltprs pnltprs_X_ami pnltprs_X_ami_tm hf_tm pnltprs_X_hf pnltprs_X_hf_tm pn_tm pnltprs_X_pn pnltprs_X_pn_tm

foreach d in "ami" "hf" "pn" {
  gen pnltprs_X_`d' = pnltprs * `d'
}
lab var pnltprs "Overall penalty pressure in 2012"
lab var pnltprs_X_ami "Penalty pressure X AMI"
lab var pnltprs_X_hf "Penalty pressure X HF"
lab var pnltprs_X_pn "Penalty pressure X PN"

sum pnltprs
loc pp_sd : di %9.3f `r(sd)'

foreach d in "ami" "hf" "pn" {
  capture drop `d'_tm
  gen `d'_tm = `d'*tm
  lab var `d'_tm "`d' X TM"

  gen pnltprs_X_`d'_tm = pnltprs_X_`d' * tm
  lab var pnltprs_X_`d'_tm "Penalty pressure X `d' X TM"
}

gen tm_pnltprs = tm * pnltprs
lab var tm_pnltprs "TM X Penalty pressure"


loc file HHeffort4_read_triple
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

loc yv hospoccur30
areg `yv' `pp' `sp3' i.wkidx , absorb(offid_nu) vce(cluster offid_nu)
*if hashosp==0
sum `yv' if e(sample)
loc mdv: display %9.2f `r(mean)'
loc ar2: display %9.2f `e(r2_a)'

qui test
loc fstat: display %9.2f `r(F)'

`out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Home health week FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)


*----------------------------------------
*Diff in DIff: use difference in penalty pressure between TM vs MA patients for all, target and non-target conditions, separately
*----------------------------------------

use `epilvl', clear

loc sp3 `sp2' `comorbid' i.fy
loc outcome epidur lnmvl lnmvlsn lnmvl_wk1 lnmvlsn_wk1 lnfreq_tnvall lnfreq_tnvsn lnfreq_nvall_wk1 lnfreq_nvsn_wk1 lepilvl_vtc_tr_pay tfho tfnv startHH_1day
*lepilvl_vtc_tr lntnvall lntnvsn

*-----------------------
* use the interaction with aggregate 2012 penalty pressure

gen pnltprs_X_tm = pnltprs*tm
lab var pnltprs_X_tm "Penalty pressure in 2012 X TM"
lab var pnltprs "Overall penalty pressure in 2012"

sum pnltprs
loc pp_sd : di %9.3f `r(sd)'

loc pp pnltprs tm pnltprs_X_tm

*non-readmitted patients
loc file HHeffort4_norapat_tm_agpp12
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp' `sp3' if hashosp==0, absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}

* by penalty condition vs non-penalty condition patients
forval x = 0/1 {
  loc file HHeffort4_norapat_tm_c`x'
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons label"

  foreach yv of varlist `outcome' {
    areg `yv' `pp' `sp3' if hashosp==0 & hrrpcond==`x', absorb(offid_nu) vce(cluster offid_nu)
    *if hashosp==0
    sum `yv' if e(sample)
    loc mdv: display %9.2f `r(mean)'
    loc ar2: display %9.2f `e(r2_a)'

    qui test
    loc fstat: display %9.2f `r(F)'

    `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
  }
}

*all patients

*non-readmitted patients
loc file HHeffort4_allpat_tm_agpp12
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp' `sp3', absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}

* by penalty condition vs non-penalty condition patients
forval x = 0/1 {
  loc file HHeffort4_allpat_tm_c`x'
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons label"

  foreach yv of varlist `outcome' {
    areg `yv' `pp' `sp3' if hrrpcond==`x', absorb(offid_nu) vce(cluster offid_nu)
    *if hashosp==0
    sum `yv' if e(sample)
    loc mdv: display %9.2f `r(mean)'
    loc ar2: display %9.2f `e(r2_a)'

    qui test
    loc fstat: display %9.2f `r(F)'

    `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
  }
}

*readmission outcome
use HHeffort4, clear

gen hospoccur30 = hospoccur* hashosp30
lab var hospoccur30 "30-day hospital readmission indicator"
loc l_hospoccur30 "30-day hospital readmission indicator"

gen pnltprs_X_tm = pnltprs*tm
lab var pnltprs_X_tm "Penalty pressure in 2012 X TM"
lab var pnltprs "Overall penalty pressure in 2012"

sum pnltprs
loc pp_sd : di %9.3f `r(sd)'

loc pp pnltprs tm pnltprs_X_tm

*non-readmitted patients
loc file HHeffort4_read_tm_agpp12
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

loc yv hospoccur30

areg `yv' `pp' `sp3' i.wkidx, absorb(offid_nu) vce(cluster offid_nu)
*if hashosp==0
sum `yv' if e(sample)
loc mdv: display %9.2f `r(mean)'
loc ar2: display %9.2f `e(r2_a)'

qui test
loc fstat: display %9.2f `r(F)'

`out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)

forval x = 0/1 {
  areg `yv' `pp' `sp3' i.wkidx if hrrpcond==`x', absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Home health week FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}



*-----------------------
* use the interaction with aggregate 2013 penalty pressure

gen pnltprs13_X_tm = pnltprs13*tm
lab var pnltprs13_X_tm "Penalty pressure in 2013 X TM"
lab var pnltprs13 "Overall penalty pressure in 2013"

sum pnltprs13
loc pp_sd : di %9.3f `r(sd)'

loc pp pnltprs13 tm pnltprs13_X_tm

*non-readmitted patients
loc file HHeffort4_norapat_tm_agpp13
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp' `sp3' if hashosp==0, absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}

* by penalty condition vs non-penalty condition patients
forval x = 0/1 {
  loc file HHeffort4_norapat_tm_c`x'_agpp13
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons label"

  foreach yv of varlist `outcome' {
    areg `yv' `pp' `sp3' if hashosp==0 & hrrpcond==`x', absorb(offid_nu) vce(cluster offid_nu)
    *if hashosp==0
    sum `yv' if e(sample)
    loc mdv: display %9.2f `r(mean)'
    loc ar2: display %9.2f `e(r2_a)'

    qui test
    loc fstat: display %9.2f `r(F)'

    `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
  }
}

*all patients

*non-readmitted patients
loc file HHeffort4_allpat_tm_agpp13
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

foreach yv of varlist `outcome' {
  areg `yv' `pp' `sp3', absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}

* by penalty condition vs non-penalty condition patients
forval x = 0/1 {
  loc file HHeffort4_allpat_tm_c`x'_agpp13
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons label"

  foreach yv of varlist `outcome' {
    areg `yv' `pp' `sp3' if hrrpcond==`x', absorb(offid_nu) vce(cluster offid_nu)
    *if hashosp==0
    sum `yv' if e(sample)
    loc mdv: display %9.2f `r(mean)'
    loc ar2: display %9.2f `e(r2_a)'

    qui test
    loc fstat: display %9.2f `r(F)'

    `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
  }
}

*readmission outcome
use HHeffort4, clear

gen hospoccur30 = hospoccur* hashosp30
lab var hospoccur30 "30-day hospital readmission indicator"
loc l_hospoccur30 "30-day hospital readmission indicator"

gen pnltprs13_X_tm = pnltprs13*tm
lab var pnltprs13_X_tm "Penalty pressure in 2013 X TM"
lab var pnltprs13 "Overall penalty pressure in 2013"

sum pnltprs13
loc pp_sd : di %9.3f `r(sd)'

loc pp pnltprs13 tm pnltprs13_X_tm

*non-readmitted patients
loc file HHeffort4_read_tm_agpp13
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

loc yv hospoccur30

areg `yv' `pp' `sp3' i.wkidx, absorb(offid_nu) vce(cluster offid_nu)
*if hashosp==0
sum `yv' if e(sample)
loc mdv: display %9.2f `r(mean)'
loc ar2: display %9.2f `e(r2_a)'

qui test
loc fstat: display %9.2f `r(F)'

`out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)

forval x = 0/1 {
  areg `yv' `pp' `sp3' i.wkidx if hrrpcond==`x', absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  qui test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(`pp') addtext(F statistic, `fstat', Mean dep. var., `mdv', SD of penalty pressure, `pp_sd', Office FE, Y, Fiscal Year FE, Y, Home health week FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}
