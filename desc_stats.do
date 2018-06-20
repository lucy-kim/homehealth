*Run descriptive stats

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

*restrict to sample

use epilvl_rehosp_smpl_old, clear

keep if tm==1

loc outcome lnepilength
loc pp ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
areg `yv' `pp' `sp', absorb(offid_nu) vce(cluster offid_nu)
keep if e(sample)

tempfile insmpl
save `insmpl'

*---------------------------------
*# referring hospitals

use `insmpl', clear
keep prvdr_num
duplicates drop
count
*---------------------------------

*what are the top 5 conditions among non-target patients?

keep epiid hrrpcond
duplicates drop
merge 1:1 epiid using masterclientadmiss2, keep(1 3) nogen keepusing(clientid socdate_e category)

keep if hrrpcond==0
/* bys category: gen freq = _N

tab category  if freq >= 551, sort
keep if freq >= 551 */

merge 1:m clientid socdate_e using inpat_dx, keep(1 3) nogen

*keep only the 3 digits before period
split inpat_dx_cor, p(".")

tempfile tmp
save `tmp'

keep inpat_dx_cor1 epiid category
gen i = 1

preserve
collapse (sum) freq = i, by(inpat_dx_cor1)
gsort inpat_dx_cor1
egen tot = sum(freq)
gen pct = 100*freq /tot
gen x = sum(pct)
rename x cumulative_pct
outsheet using `reg'/inpat_dx_icd_ranking.csv, replace names comma
restore

preserve
destring inpat_dx_cor1, replace
gen badcontrol = inpat_dx_cor1 >=390 & inpat_dx_cor1<=519
keep epiid badcontrol
duplicates drop

/*
foreach l in "ENCOUNTERING" "SYMPTOMS" "RHEUMATISM" "CEREBROVASCULAR" "HEART" {
  preserve
  keep if regexm(category,"`l'")
  keep inpat_dx_cor
  gen i = 1
  collapse (sum) freq = i, by(inpat_dx_cor)
  gsort -freq
  outsheet using `reg'/`l'.csv, replace cross
  restore
}

bys epiid: gen ndiag = _N
tab ndiag
*80% episodes have 1-4 inpat diagnoses codes */

*groupings by ICD-10 codes (see below for the top 5 categories). The first one is "Persons encountering health services in other circumstances".
tab category if hrrpcond==0, sort
/* Persons encountering health services in other circumstances |      5,524       37.39       37.39
                               SYMPTOMS |      1,016        6.88       44.27
         RHEUMATISM, EXCLUDING THE BACK |        685        4.64       48.91
                CEREBROVASCULAR DISEASE |        643        4.35       53.26
           OTHER FORMS OF HEART DISEASE |        551        3.73       56.99 */

*---------------------------------
*missing values in covariates?
use epilvl_rehosp_smpl_old, clear

xi i.fy i.age5yr i.size
loc ages _Iage5yr*
loc demog `ages' female white noassist livealone `ins'
loc comorbid ynch* `overallst' `hrfactor' `priorcond'
loc hospchars vi_hha teaching urban own_* _Isize*
loc officechars lnallepi lnnw_active_w
loc sp `riskhosp' `demog' `comorbid' _Ify* `officechars' `hospchars'

foreach v of varlist vtc_tr_pay* epilength* lov* {
  gen ln`v' = ln(`v'+1)
}

loc uami "AMI"
loc uhf "HF"
loc upn "PN"
foreach d in "ami" "hf" "pn" {
  capture drop pnltprs_c_X_`d'
  gen pnltprs_c_X_`d' = pnltprs_c *`d'

  lab var pnltprs_c_X_`d' "Penalty salience X `u`d''"
  lab var `d' "Indicator for `u`d''"
}


loc outcome lnepilength lnlov lnlovsn freq_tnv freq_tnvsn startHH_1day lnvtc_tr_pay
loc pp ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn

gen missing = 0
foreach v of varlist `sp' `outcome' `pp' {
  replace missing = 1 if `v'==.
}
tab missing if tm==1

*---------------------------------
*# obs for treated conditions
use epilvl_rehosp_smpl_old, clear

loc pp ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn
*loc pp ami hf pn pnltprs pnltprs_X_ami pnltprs_X_hf pnltprs_X_pn

areg hashosp30 `pp' `sp' if tm==1, absorb(offid_nu) vce(cluster offid_nu)
gen insmpl = e(sample)

keep if insmpl==1
tempfile insmpl
save `insmpl'


*# observations in each condition
foreach d in "ami" "hf" "pn" {
  di "# obs in `d'--------"
  count if `d'==1 & insmpl==1
}
tab hrrpcond if insmpl

*get penalty rate by condition
merge m:1 offid_nu prvdr_num using HRRPpnlty_pressure_hj_2012, keep(3) nogen
capture drop pr_c
gen pr_c = .
foreach d in "ami" "hf" "pn" {
  replace pr_c = penalty2012_`d' if `d'==1
}
replace pr_c = 0 if hrrpcond==0
lab var pr_c "Condition-specific penalty rate in 2012"
assert pr_c!=.


*mean penalty salience & penalty rate by condition
foreach d in "ami" "hf" "pn" {
  di "--------`d'--------"
  sum pnltprs_c pr_c hashosp30 shref_hj if `d'==1 & insmpl==1
}
sum pnltprs_c pr_c shref_hj hashosp30 if insmpl & hrrpcond==0
sum pnltprs_c pr_c shref_hj hashosp30 if insmpl

keep if insmpl==1

loc outcome1 epilength lov freq_tnv startHH_1day vtc_tr_pay
loc outcome2 lnepilength_1stwk1 lnlov_1stwk1 lnlovsn_1stwk1 freq_tnv_1stwk1 freq_tnvsn_1stwk1 lnvtc_tr_pay_1stwk1
loc outcome3 lnepilength_1stwk0 lnlov_1stwk0 lnlovsn_1stwk0 freq_tnv_1stwk0 freq_tnvsn_1stwk0 lnvtc_tr_pay_1stwk0

loc l_lnepilength "Ln Episode length (days)"
loc l_lnlov "Ln Visit length (min)"
loc l_lnlovsn "Ln Nurse visit length (min)"
loc l_freq_tnv "Frequency of visits"
loc l_freq_tnvsn "Frequency of nurse visits"
loc l_startHH_1day "Start HH within 1 day from discharge"
loc l_lnvtc_tr_pay "Ln Total cost index ($)"

loc l_epilength "Episode length (days)"
loc l_lov "Visit length (min)"
loc l_lovsn "Nurse visit length (min)"
loc l_freq_tnv "Frequency of visits"
loc l_freq_tnvsn "Frequency of nurse visits"
loc l_startHH_1day "Start HH within 1 day from discharge"
loc l_vtc_tr_pay "Total cost index ($)"

foreach v of varlist `outcome1' {
  lab var `v' "`l_`v''"
}

forval x=1/3 {
  preserve
  keep `outcome`x''
  order `outcome`x''
  outreg2 using `reg'/summstats`x'.xls, replace sum(log) label  eqkeep(N mean)
  restore
}

egen ynchsum = rowtotal(ynch? ynch??)

sum `riskhosp' age female white noassist livealone dual ynchsum `overallst' `hrfactor' `priorcond'

*publication-purpose var labels
lab var age "Age"
lab var female "Female"
lab var dual "Dual eligible"
lab var avgvgap "Mean number of days between SN visits"
lab var overallst_vbad "Overall status: (Very bad) Progressive conditions"
lab var overallst_bad "Overall status: (Bad) Remain in fragile health"
lab var overallst_tempbad "Overall status: Temporarily facing high health risks"
lab var hrfactor_alcohol "High risk factor: Alcohol dependency"
lab var hrfactor_drug "High risk factor: Drug dependency"
lab var hrfactor_smoke "High risk factor: Heavy smoking"
lab var hrfactor_obese "High risk factor: Obesity"
lab var priorcond_disrupt "Pre-HHC condition: Disruptive behavior"
lab var priorcond_impdm "Pre-HHC condition: Impaired decision-making"
lab var priorcond_cath "Pre-HHC condition: Indwelling/Suprapublic catheter"
lab var priorcond_pain "Pre-HHC condition: Intractable pain"
lab var priorcond_memloss "Pre-HHC condition: Memory loss"
lab var priorcond_incontn "Pre-HHC condition: Urinary incontinence"
lab var white "White"
lab var ma_visit "Enrolled in per-visit paying Medicare Advantage"
lab var ma_epi "Enrolled in per-episode paying Medicare Advantage"
lab var dual "Dual eligible"
lab var noassist "No assistance available"
lab var livealone "Living alone"
lab var riskhosp_fall "Risk for hospitalization: History of 2+ falls"
lab var riskhosp_manyhosp "Risk for hospitalization: 2+ hospitalizations"
lab var riskhosp_mental "Risk for hospitalization: Recent decline in Mental"
lab var riskhosp_ge5med "Risk for hospitalization: Take 5+ medications"
lab var riskhosp_oth "Risk for hospitalization: Other"
lab var ynchsum "Sum of 17 Charlson comorbidity indicators"

preserve
keep `riskhosp' age female white noassist livealone dual ynchsum `overallst' `hrfactor' `priorcond'
order `riskhosp' age female white noassist livealone dual ynchsum `overallst' `hrfactor' `priorcond'
outreg2 using `reg'/summstats4.xls, replace sum(log) label  eqkeep(N mean)
restore

*hospital characteristics
tab size
gen small = size==1
gen medium = size==2
gen big = size==3

loc hospchars small medium big own_* teaching urban vi_hha
lab var small "Beds 1-100"
lab var medium "Beds 101-300"
lab var big "Beds 301+"
lab var own_fp "For-profit"
lab var own_np "Not for-profit"
lab var own_gv "Government owned"
lab var urban "Urban"
lab var teaching "Teaching"
lab var vi_hha "Have a hospital-based HHA"
lab var lnallepi "Ln Num. ongoing episodes in the office"
lab var lnnw_active_w "Ln Num. active practitioners in the office"

preserve
keep `officechars' `hospchars'
order `officechars' `hospchars'
outreg2 using `reg'/summstats5.xls, replace sum(log) label  eqkeep(N mean)
restore

*---------------------------------
*by target vs non-target condition

*---------------------------------
*variation across offices in penalty pressure (penalty rate in appendix)
use `insmpl' , clear

*drop non-target patients
drop if hrrpcond==0

*pr_c
keep offid_nu pnltprs_c prvdr_num

collapse (mean) pnltprs_c, by(offid_nu prvdr_num)

*width(0.00001)
hist pnltprs_c if pnltprs_c < 0.1, frac width(0.001) xti(Mean penalty salience for each referring hospital-office pair [%]) graphregion(color(white))
*caption("Notes. Two outlier values of 0.12 and 0.5 omitted from the figure.")
graph export `reg'/hist_pnltprs_c.eps, replace

hist pr_c, frac width(0.00001) xti(Mean penalty rate across referring hospitals [%])
graph export `reg'/hist_pr_c.eps, replace

*---------------------------------
* Plot showing how difference in effort increases between penalized and non-penalized conditions as we go from less penalty pressure offices to more penalty pressure offices (quartiles or just two groups, whatever works better). Penalized conditions can be aggregated or not, depending on what is better.

/* gen pp = 1 if pnltprs_c==0
tab pnltprs_c if pnltprs_c > 0
gen pp_c = pnltprs_c if pnltprs_c > 0
xtile x = pp_c, n(2)
replace pp = x+1 if pnltprs_c > 0
assert pp!=. */

use `insmpl' , clear
sum pnltprs_c hashosp30, de

*use aggregate penalty rate (not condition-specific ones)
gen pp = pnltprs > 0
assert pp!=.
tab pp, summarize(pnltprs)

tab pp pn if ami==0 & hf==0

sum vtc_tr_pay if pp==1 & hrrpcond==0

lab define l1 0 "Non-target" 1 "Target", replace
lab define l2 0 "Penalty salience = 0" 1 "Penalty salience > 0", replace
lab val hrrpcond l1
lab val pp l2

tab pp
tab hrrpcond

gen hrrpcond4 = 1 if ami==1
replace hrrpcond4 = 2 if hf==1
replace hrrpcond4 = 3 if pn==1
replace hrrpcond4 = 4 if hrrpcond==0
assert hrrpcond4!=.

graph bar (mean) vtc_tr_pay, over(hrrpcond) over(pp) subti(Target vs Non-target condition) yti(Total cost index ($))
*ysc(r(4 8)) ylab(4(1)8)
graph export `reg'/effect_illust.eps, replace

preserve
collapse (mean) lnvtc_tr_pay vtc_tr_pay, by(pp hrrpcond)
restore

preserve
collapse (mean) lnvtc_tr_pay vtc_tr_pay, by(pp cond)
list
reshape wide lnvtc_tr_pay vtc_tr_pay, i(pp) j(cond)

forval x = 1/3 {
  gen diff`x' = lnvtc_tr_pay`x' - lnvtc_tr_pay4
}
list pp diff*

restore

*---------------------------------
*# states
*get office ID
use epilvl_rehosp_smpl_old, clear
keep offid_nu
duplicates drop
merge 1:m offid_nu using office, keep(1 3) nogen
keep offid_nu addr_st
duplicates drop
count
keep addr_st
duplicates drop
count
tab addr_st

use office, clear
keep offid_nu addr_st
duplicates drop
count
keep addr_st
duplicates drop
count

*sample period
use epilvl_rehosp_smpl_old, clear
keep epiid socdate_e
duplicates drop
merge 1:1 epiid using HHeffort_epilvl, keep(2 3) nogen
sum socd
tab socd if socd==19267
tab socd if socd==20250
