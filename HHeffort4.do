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
merge m:1 clientid socdate_e using `nontarget', keep(1 3) nogen
drop if hrrpcond==0 & !(cardioresp==1 | cardiovas==1 | neuro==1 | medicine==1)

*drop outliers:
sum mvl* nv* mtenure_esd mvisitorder, de
foreach v of varlist mvl* nv* mtenure_esd mvisitorder {
  qui sum `v', de
  replace `v' = . if `v' > `r(p99)'
}

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
gen startHH_1day = time2hh <=1 if time2hh!=.

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

*for the last week, adjust down the number of days in the last week if the episode ends mid-week
bys epiid: egen epilength_wk = max(wkidx)
gen lastwkdays = mod(epilength,7)
replace lastwkdays = 7 if lastwkdays==0
replace freq_nvall = nvall/lastwkdays if wkidx==epilength_wk
replace freq_nvsn = nvsn/lastwkdays if wkidx==epilength_wk

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
bys epiid wkidx: egen nho_wk = sum(ho)

*get % FT nurse visits
gen nv_ft = discipline=="SN" & status=="VFT"
bys epiid: egen tnvsn_ft = sum(nv_ft)
bys epiid wkidx: egen nvsn_ft = sum(nv_ft)
gen tfnv = tnvsn_ft / tnvsn
gen fnv_wk = nvsn_ft / nvsn
sum tfnv fnv_wk

list epiid visitdate discipline jobcode status tnvsn tnvsn_ft firsthospdate tfnv if epiid==537956

keep epiid wkidx startHH_1day freq_tnvall freq_tnvsn freq_nvall freq_nvsn tho nho_wk tfnv fnv_wk
duplicates drop

tempfile moreoutcome
save `moreoutcome'

use `an', clear
merge m:1 epiid wkidx using `moreoutcome', keep(3) nogen

*take logs for frequency of visits, # handoffs
foreach v of varlist freq* tho nho_wk {
  capture drop ln`v'
  gen ln`v' = ln(`v' +1)
}

* create % handoff visits during the episode
gen tfho = tho / tnvsn
gen fho_wk = nho_wk / nvsn

sort epiid wkidx

compress
save HHeffort4, replace


*---------------------------------
*run regressions with interaction of condition dummies and penalty pressure

*should i run regression at the episode-week level for some outcomes & at the episode level for other outcomes? for the latter, e.g. whether the HH epi starts within 1 day, frequency of visits during the entire episode?????

use HHeffort4, clear

*drop MA patients
drop if ma==1

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
loc l_lnmvl "Log Mean visit length per week"
loc l_lnmvlsn "Log Mean nurse visit length per week"
loc l_lnnvall "Log # Any visits per week"
loc l_lnnvsn "Log # Nurse visits per week"
loc l_hospoccur "Hospital readmission indicator"

loc sp1 `riskhosp'
loc sp2 `sp1' `demog'
loc sp3 `sp2' `comorbid' i.fy

loc file HHeffort4_allpat
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons label"

foreach yv of varlist lnmvl* lnnv*  {
  areg `yv' ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn `sp3' i.wkidx if wkidx != epilength_wk, absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn) addtext(F statistic, `fstat', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Home health week FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}

*for readmission indicator, include last week of episode
loc yv hospoccur
areg `yv' ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn `sp3' i.wkidx, absorb(offid_nu) vce(cluster offid_nu)
*if hashosp==0
sum `yv' if e(sample)
loc mdv: display %9.2f `r(mean)'
loc ar2: display %9.2f `e(r2_a)'

test
loc fstat: display %9.2f `r(F)'

`out' ctitle(`l_`yv'') keep(ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn) addtext(F statistic, `fstat', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Home health week FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)

*non-readmitted pats
loc file HHeffort4_norapat
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

foreach yv of varlist lnmvl* lnnv* {
  areg `yv' ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn `sp3' i.wkidx if hashosp==0 & wkidx != epilength_wk, absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn) addtext(F statistic, `fstat', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Home health week FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}
*---------------
*use the episode as a unit of obs, not episode-week; so exclude week FE
use HHeffort4, clear

*drop MA patients
drop if ma==1

* create episode-level Frequency of nurse visits in the first week
gen x = lnfreq_nvsn if wkidx==1
bys epiid: egen lnfreq_nvsn_wk1 = max(x)

loc yv startHH_1day lnfreq_tnvall lnfreq_tnvsn lnfreq_nvsn_wk1 tfho tfnv

*regression macros
loc ages i.age5yr
loc riskhosp riskhosp_fall riskhosp_manyhos riskhosp_mental riskhosp_ge5 riskhosp_oth
loc priorcond priorcond_dis priorcond_impd priorcond_cath priorcond_pain priorcond_mem priorcond_inc
loc hrfactor hrfactor_alco hrfactor_drug hrfactor_smoke hrfactor_obese
loc overallst overallst_vbad overallst_bad overallst_tem
loc demog `ages' female white noassist livealone dual
loc comorbid ynch* `overallst' `hrfactor' `priorcond'
*ynch*  depressed

loc sp1 `riskhosp'
loc sp2 `sp1' `demog'
loc sp3 `sp2' `comorbid' i.fy

*keep one row per episode
keep `yv' `riskhosp' age5yr female white noassist livealone dual  `comorbid' fy ma offid_nu ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn epiid hashosp
duplicates drop
duplicates tag epiid, gen(dup)
assert dup==0
drop dup

loc l_startHH_1day "Start within 1 day from hospital discharge"
loc l_lnfreq_tnvall "Log Frequency of any visits"
loc l_lnfreq_tnvsn "Log Frequency of nurse visits"
loc l_lnfreq_nvsn_wk1 "Log Freqeuncy of nurse visits in the first week"
loc l_lntho "Log # nurse handoffs"
loc l_tfho "Proportion of visits with nurse handoffs"
loc l_tfnv "Proportion of full-time nurse visits"

*use the sample of all patients
loc file HHeffort4_allpat_epi
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons label"

foreach yv of varlist startHH_1day lnfreq_tnvall lnfreq_tnvsn lnfreq_nvsn_wk1 tfho tfnv {
  areg `yv' ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn `sp3', absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn) addtext(F statistic, `fstat', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Home health week FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}

*non-readmitted pats
loc file HHeffort4_norapat_epi
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons label"

foreach yv of varlist startHH_1day lnfreq_tnvall lnfreq_tnvsn lnfreq_nvsn_wk1 tfho tfnv {
  areg `yv' ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn `sp3' if hashosp==0, absorb(offid_nu) vce(cluster offid_nu)

  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn) addtext(F statistic, `fstat', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Home health week FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}




*--------------------------------


*run triple DDD model with interaction of condition dummies and penalty pressure and MA dummy


use HHeffort4, clear


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
loc l_lnmvl "Log Mean visit length per week"
loc l_lnmvlsn "Log Mean nurse visit length per week"
loc l_lnnvall "Log # Any visits per week"
loc l_lnnvsn "Log # Nurse visits per week"
loc l_hospoccur "Hospital readmission indicator"

loc sp1 `riskhosp'
loc sp2 `sp1' `demog'
loc sp3 `sp2' `comorbid' i.fy

*since among HF patients, if they are MA, always have zero penalty pressure, so the triple DDD is not identified below
tab ma if hf==1, summarize(pnltprs_c)
tab ma if ami==1, summarize(pnltprs_c)
tab ma if pn==1, summarize(pnltprs_c)
table ma, contents(mean ami mean hf mean pn)

loc file HHeffort4_allpat_3d
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons label"

foreach yv of varlist lnmvl* lnnv*  {
  areg `yv' ami hf pn tm pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn ami_tm hf_tm pn_tm pnltprs_c_X_ami_X_tm pnltprs_c_X_hf_X_tm pnltprs_c_X_pn_X_tm `sp3' i.wkidx if wkidx != epilength_wk, absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(ami hf pn tm pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn ami_tm hf_tm pn_tm pnltprs_c_X_ami_X_tm pnltprs_c_X_hf_X_tm pnltprs_c_X_pn_X_tm) addtext(F statistic, `fstat', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Home health week FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}

*for readmission indicator, include last week of episode
loc yv hospoccur
areg `yv' ami hf pn tm pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn ami_tm hf_tm pn_tm pnltprs_c_X_ami_X_tm pnltprs_c_X_hf_X_tm pnltprs_c_X_pn_X_tm `sp3' i.wkidx, absorb(offid_nu) vce(cluster offid_nu)
*if hashosp==0
sum `yv' if e(sample)
loc mdv: display %9.2f `r(mean)'
loc ar2: display %9.2f `e(r2_a)'

test
loc fstat: display %9.2f `r(F)'

`out' ctitle(`l_`yv'') keep(ami hf pn tm pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn ami_tm hf_tm pn_tm pnltprs_c_X_ami_X_tm pnltprs_c_X_hf_X_tm pnltprs_c_X_pn_X_tm) addtext(F statistic, `fstat', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Home health week FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)

*non-readmitted pats
loc file HHeffort4_norapat_3d
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) label append nocons"

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
*---------------
*use the episode as a unit of obs, not episode-week; so exclude week FE
use HHeffort4, clear

* create episode-level Frequency of nurse visits in the first week
gen x = lnfreq_nvsn if wkidx==1
bys epiid: egen lnfreq_nvsn_wk1 = max(x)

loc yv startHH_1day lnfreq_tnvall lnfreq_tnvsn lnfreq_nvsn_wk1 tfho tfnv

*regression macros
loc ages i.age5yr
loc riskhosp riskhosp_fall riskhosp_manyhos riskhosp_mental riskhosp_ge5 riskhosp_oth
loc priorcond priorcond_dis priorcond_impd priorcond_cath priorcond_pain priorcond_mem priorcond_inc
loc hrfactor hrfactor_alco hrfactor_drug hrfactor_smoke hrfactor_obese
loc overallst overallst_vbad overallst_bad overallst_tem
loc demog `ages' female white noassist livealone dual
loc comorbid ynch* `overallst' `hrfactor' `priorcond'
*ynch*  depressed

loc sp1 `riskhosp'
loc sp2 `sp1' `demog'
loc sp3 `sp2' `comorbid' i.fy

*keep one row per episode
keep `yv' `riskhosp' age5yr female white noassist livealone dual  `comorbid' fy ma offid_nu ami hf pn tm pnltprs_c pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn ami_tm hf_tm pn_tm pnltprs_c_X_ami_X_tm pnltprs_c_X_hf_X_tm pnltprs_c_X_pn_X_tm epiid hashosp
duplicates drop
duplicates tag epiid, gen(dup)
assert dup==0
drop dup

loc l_startHH_1day "Start within 1 day from hospital discharge"
loc l_lnfreq_tnvall "Log Frequency of any visits"
loc l_lnfreq_tnvsn "Log Frequency of nurse visits"
loc l_lnfreq_nvsn_wk1 "Log Freqeuncy of nurse visits in the first week"
loc l_lntho "Log # nurse handoffs"
loc l_tfho "Proportion of visits with nurse handoffs"
loc l_tfnv "Proportion of full-time nurse visits"

*use the sample of all patients
loc file HHeffort4_allpat_epi_3d
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons label"

foreach yv of varlist startHH_1day lnfreq_tnvall lnfreq_tnvsn lnfreq_nvsn_wk1 tfho tfnv {
  areg `yv' ami hf pn tm pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn ami_tm hf_tm pn_tm pnltprs_c_X_ami_X_tm pnltprs_c_X_hf_X_tm pnltprs_c_X_pn_X_tm `sp3', absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(ami hf pn tm pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn ami_tm hf_tm pn_tm pnltprs_c_X_ami_X_tm pnltprs_c_X_hf_X_tm pnltprs_c_X_pn_X_tm) addtext(F statistic, `fstat', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Home health week FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}

*non-readmitted pats
loc file HHeffort4_norapat_epi_3d
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append nocons label"

foreach yv of varlist startHH_1day lnfreq_tnvall lnfreq_tnvsn lnfreq_nvsn_wk1 tfho tfnv {
  areg `yv' ami hf pn tm pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn ami_tm hf_tm pn_tm pnltprs_c_X_ami_X_tm pnltprs_c_X_hf_X_tm pnltprs_c_X_pn_X_tm `sp3' if hashosp==0, absorb(offid_nu) vce(cluster offid_nu)

  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  test
  loc fstat: display %9.2f `r(F)'

  `out' ctitle(`l_`yv'') keep(ami hf pn tm pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn ami_tm hf_tm pn_tm pnltprs_c_X_ami_X_tm pnltprs_c_X_hf_X_tm pnltprs_c_X_pn_X_tm) addtext(F statistic, `fstat', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Home health week FE, Y, Hospitalization risk controls, Y, Demographic controls, Y, Comorbidity controls, Y)
}
