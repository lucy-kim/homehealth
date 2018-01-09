*anchng_chars_pat2HH.do
* investigate the potential changes over time in the characteristics of patients discharged to home health

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults
loc mvar admissionclie
loc social livealone noassist
loc severity overallst_bad riskhosp_ge5med charlindex

cd `path'

*create a patient-office-day level data where day is each day during her episode
use epi_visit, clear

*restrict to admission-episodes for which we can determine whether the admission ended for one of the following reasons: death, hospitalized (first hosp), discharged
*should restrict to episodes
tab dcdate_e
*since the number of obs with DC date after Sunday 8/9/2015 declines steeply, use 8/9 as the last available DC date
*for the episode that started on or before 8/9/2015 - 60 days, we know whether the episode ended with discharge; if it didn't end with a discharge, check if it had a hospitalization or death--if it didn't have either hosp or death, it should have a recertification afterwards. we don't want these admissions. then we are selectively dropping admissions that have multiple-episode admissions among admissions that have an episode starting on or after 8/9/2015 - 60 days. In the regression analysis, we only use 1-episode sample, so this is not a problem. But in the IV exogeneity analysis, we use all-admissions sample - for the IV analysis sample, just use the admissions whose last episode started on or before 8/9/2015 - 120 days (85% admissions have 1-2 episodes).

*restrict to episodes that started on or before 8/9/2015 - 59 days, i.e. 8/9/2015 is the 60th day of the episode
gen date = mdy(8,9,2015) - 59
count if epidate2 <= date
keep if epidate2 <= date
*0.3M obs dropped

sort `mvar' epiid visitd
list `mvar' epiid visitd epidate* hashosp firsthospdate death dcdate* in 1/30

*drop visitdates after first hospitalization date in the admission
drop if visitdate > firsthospdate & firsthospdate!=.

*count if DC date is missing
count if dcdate_e==.

*drop if no hospitalization or death occurred
count if dcdate_e==. & hashosp==0 & death==0
drop if dcdate_e==. & hashosp==0 & death==0

*assert that the admission ended for one of the following reasons: death, hospitalized (first hosp), discharged
count if dcdate_e==.
assert hashosp==1 | death==1 if dcdate_e==.

*create DC indicator
capture drop dced
gen dced = dcdate_e!=. & hashosp==0 & death==0
count if dced + hashosp + death!=1
egen rs = rowtotal(dced hashosp death)
tab rs
*2K obs have rs = 2 ; all of these died & had a hospitalization
tab death if rs==2
tab hashosp if rs==2
count if deathdate_e <= firsthospdate & rs==2
count if deathdate_e > firsthospdate & rs==2
*most of these 2K obs had the deaeth date after hospitalization

*create endstatus = 1,2,3 for discharge, hospitalized, died
gen endstatus = 1 if dced==1
*if the death occurred after the hosptialization, recode the hospitalization
replace endstatus = 2 if hashosp==1 & deathdate_e > firsthospdate
replace endstatus = 3 if death==1 & deathdate_e <= firsthospdate
assert endstatus!=.
lab define s3 1 "Discharged" 2 "Hospitalized" 3 "Died"
lab val endstatus s3
tab endstatus
lab var endstatus "End of admiss status: 1,2,3 for discharge, hospitalized, died"

count if payrollno==""
*0 obs
/*list workerID if payrollno==""*/
replace payrollno = workerID + "P" if payrollno=="" & workerID!=""
assert payrollno!=""

*tag each admission
sort `mvar' epidate2 visitdate
bys `mvar': gen i = _n==1
count if i==1
*75K admissions in total

*what is the % patients who have > 1 episode?
sort `mvar' epiid visitdate

*since there are admissions for which I may not have all the episodes, use the # days under HHC to get the # episodes if the # days under HHC indicates a different # episodes than # episode IDs we have
capture drop daysinHH
bys `mvar': egen ffvd = min(visitd)
bys `mvar': egen llvd = max(visitd)
egen latest = rowmax(llvd dcdate2)
gen daysinHH = latest - ffvd + 1

gen nepi2 = ceil(daysinHH/60)
count if nepi!=nepi2
*probably because I excluded visits after the first hospitalization if the patient returned afterwards
drop nepi
rename nepi2 nepi
lab var nepi "Number of episodes under an admission until the endpoint"
tab nepi if i==1

drop rs date i ffvd llvd

assert daysinHH >= 1 & daysinHH <= 60 if nepi==1
tab daysinHH

duplicates drop

tempfile tmp
save `tmp'
*-------
* keep only the admission-level data

*since an admission may be associated with multiple offices, choose the first episode office
sort admissionclie visitdate
bys admissionclie: keep if _n==1
keep admissionclie offid_nu socdate_e `social' `severity' facility

*merge with admission-level referral hospital ID data
gen x = string(admissionclie, "%13.0g")
rename admissionclie admitID_num
rename x admissionclientsocid
merge 1:m admissionclie offid_nu using referralhosp_mcrID, keep(1 3)
*it should be 1:1 match but there was one admission with 2 hospital referral sources

* randomly pick only 1 hospital referral source, if matched (affected only 1 admission)
bys admissionclie offid_nu: gen nu = _N
bys admissionclie offid_nu: drop if nu==2 & _n==1

* create fiscal year
gen fy = .
forval y=/2016 {
  loc yl1 = `y'-1
  replace fy = `y' if socdate >= mdy(10,1,`yl1') & socdate <= mdy(9,30,`y')
}
assert fy!=.

*get the monday of the week to which SOC belongs
gen day = dow(socdate)
gen monday = socdate - 6 if day==0
forval d = 1/6 {
  loc d2 = `d' - 1
  replace monday = socdate - `d2' if day==`d'
}
format monday %d
drop day
assert socdate!=.
assert monday!=.
lab var monday "monday of the SOC date of the admission"

*get the month of SOC date
gen ym = ym(year(socdate), month(socdate))
format ym %tm

*share of hospital referrals among all admissions across months
bys offid_nu ym: gen totadmit = _N
bys offid_nu ym: egen hospref = sum(_merge==3)
gen sh_hospref = hospref/totadmit

*drop referrals from non-hospitals
keep if _m==3
drop _m

*link with hospital-week level # discharges to HH
rename prvdr_num provider_hosp
merge m:1 provider_hosp monday using CMShospdisch_tohh, keep(1 3) gen(m2) keepusing(hdis2hh)
*m2 = 1 for 11K obs; m2 = 3 for 60K

*interpret unmatched obs as hospitals having no discharges to home health & recode
replace hdis2hh = 0 if m2==1
drop m2

compress
save pats_hospreferred, replace


*--------------
*restrict to AMI, HF, PNEU, COBG conditions
use pats_hospreferred, clear
merge 1:m clientid socdate_e using inpat_dx, keep(3) nogen
*dropped 3522 obs

*for ICD-9-CM codes, see https://www.qualitynet.org/dcs/ContentServer?cid=1228774371008&pagename=QnetPublic%2FPage%2FQnetTier4&c=Page
rename inpat_dx icd2

*remove "."
replace icd2 = subinstr(icd2, ".", "",.)
destring icd2, replace

* AMI (from Table D.1.1 in 2016 report)
gen ami = icd2==41000 | icd2==41001 | icd2==41010 | icd2==41011 | icd2==41020 | icd2==41021 | icd2==41030 | icd2==41031 | icd2==41040 | icd2==41050 | icd2==41051 | icd2==41060 | icd2==41061 | icd2==41070 | icd2==41071 | icd2==41080 | icd2==41081 | icd2==41090 | icd2==41091

* HF
tab icd2 if (icd2 >= 40201 & icd2 <= 42843) | icd2==4289
gen hf = icd2==40201 | icd2==40211 | icd2==40291 | icd2==40401 | icd2==40403 | icd2==40411 | icd2==40413 | icd2==40491 | icd2==40493 | icd2==4280 | icd2==4281 | icd2==4289 | icd2==42820 | icd2==42821 | icd2==42822 | icd2==42823 | icd2==42830 | icd2==42831 | icd2==42832 | icd2==42833 | icd2==42840 | icd2==42841 | icd2==42842 | icd2==42843

*PNEU
gen pneu = icd2==4800 | icd2==4801 | icd2==4802 | icd2==4803 | icd2==4808 | icd2==4809 | icd2==481 | icd2==4820 | icd2==4821 | icd2==4822 | icd2==48230 | icd2==48231 | icd2==48232 | icd2==48239 | icd2==48240 | icd2==48241 | icd2==48242 | icd2==48249 | icd2==48281 | icd2==48282 | icd2==48283 | icd2==48284 | icd2==48289 | icd2==4829 | icd2==4830 | icd2==4831 | icd2==4838 | icd2==485 | icd2==486 | icd2==4870 | icd2==48811 | icd2==5070

* COPD
tab icd2 if icd2 >=49121 & icd2 <= 49600
gen copd = icd2==49121 | icd2==49122 | icd2==4918 | icd2==4919 | icd2==4928 | icd2==49320 | icd2==49321 | icd2==49322 | icd2==496

tempfile an
save `an'

*--------------
*create a dummy for whether in each FY, the hospital belongs to > median penalty rate
use hrrp_penalty, clear
drop name
tempfile d1
save `d1'

use hrrp_penalty, clear
gen fyear2 = 2012
keep prvdr_num fyear2 *2012*
duplicates drop
rename fyear2 fyear
gen penalty = totpenalty2012

append using `d1'
sort prvdr_num fyear
list in 1/10

gen penalty_gtmed = .
gen penalty_gtmed2012 = .
forval y=2012/2015 {
  xtile c = penalty if fyear==`y'
  replace penalty_gtmed = c - 1 if fyear==`y'
  drop c

  xtile c = totpenalty2012 if fyear==`y'
  replace penalty_gtmed2012 = c - 1 if fyear==`y'
  drop c
}

foreach c in "ami" "chf" "pneu" {
  rename penalty2012_`c' penalty_`c'
  gen penalty_gtmed_`c' = .
  forval y=2012/2015 {
    xtile c = penalty_`c' if fyear==`y'
    replace penalty_gtmed_`c' = c - 1 if fyear==`y'
    drop c
  }
}

tempfile hrrp_penalty
save `hrrp_penalty'

*-----------------------
* link with hospital penalty data
use `an', clear
rename provider_hosp prvdr_num
rename fy fyear

merge m:1 prvdr_num fyear using `hrrp_penalty', keep(1 3)
*all the FY  are unmatched; for other FY, pats don't come from penalty program participants
keep if _m==3
drop _m

destring prvdr_num, replace

*label variables
lab var riskhosp_ge5med "Indicator for taking 5+ medications"
lab var overallst_bad "Indicator for being likely to remain in fragile health"
lab var charlindex "Charlson comorbidity index"
lab var livealone "Indicator for living alone"
lab var noassist "Indicator for having no assistance available"

*create interaction terms & label them
xi i.fyear
loc l_penalty_gtmed "Above median penalty rate in each FY"
loc l_penalty_gtmed2012 "Above median penalty rate in FY 2012"
loc l_penalty_gtmed_ami "Above median expected penalty for AMI in "
loc l_penalty_gtmed_hf "Above median expected penalty for HF in "
loc l_penalty_gtmed_pneu "Above median expected penalty for PNEU in "

rename penalty_chf penalty_hf
rename penalty_gtmed_chf penalty_gtmed_hf
forval y = 3/5 {
    lab var _Ifyear_201`y' "Fiscal year 201`y'"
    foreach v of varlist penalty_gtmed penalty_gtmed2012 penalty_gtmed_ami penalty_gtmed_hf penalty_gtmed_pneu {
      lab var `v' "`l_`v''"
      gen `v'_fy201`y' = `v'*_Ifyear_201`y'
      lab var `v'_fy201`y' "`l_`v'' X Fiscal year 201`y'"
    }
}

tempfile an2
save `an2'

*--------------
*regression
use `an2', clear

*by specific condition
foreach c in "ami" "hf" "pneu" "copd" {
  loc file `c'
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append"

  foreach yv of varlist `severity' `social' {

    areg `yv' penalty_gtmed _Ifyear_2014 _Ifyear_2015 penalty_gtmed_fy2014 penalty_gtmed_fy2015 i.offid_nu if fyear >= 2013 & `c'==1,  vce(cluster offid_nu) absorb(prvdr_num)
    *areg `yv' penalty_gtmed _Ifyear_2014 _Ifyear_2015 penalty_gtmed_fy2014 penalty_gtmed_fy2015 i.offid_nu if `c'==1,  vce(cluster offid_nu) absorb(prvdr_num)

    sum `yv' if e(sample)
    loc mdv: display %9.2f `r(mean)'

    loc rsq: display %9.2f e(r2)
    loc ar2: display %9.2f `e(r2_a)'

    `out' keep(penalty_gtmed _Ifyear_201? penalty_gtmed_fy201?) label addtext(Adjusted R-squared, `ar2', Mean dep. var., `mdv')
  }
}

*use the simulated penalty rate for 2012
*by specific condition

foreach c in "ami" "hf" "pneu" "copd" {
  loc file `c'_pnlt2012
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append"

  foreach yv of varlist `severity' `social' {

    areg `yv' penalty_gtmed2012 _Ifyear_2013 _Ifyear_2014 _Ifyear_2015 penalty_gtmed2012_fy2013 penalty_gtmed2012_fy2014 penalty_gtmed2012_fy2015 i.offid_nu if `c'==1,  vce(cluster offid_nu) absorb(prvdr_num)

    sum `yv' if e(sample)
    loc mdv: display %9.2f `r(mean)'

    loc rsq: display %9.2f e(r2)
    loc ar2: display %9.2f `e(r2_a)'

    `out' keep(penalty_gtmed2012 _Ifyear_201? penalty_gtmed2012_fy201?) label addtext(Adjusted R-squared, `ar2', Mean dep. var., `mdv')
  }
}

*--------------
*Regression at the office-hospital-FY level: use y = number (share) of patients coming from the hospital

*construct the share of patients referred by each hospital for each FY
loc tunit fy

*compute the total number of patients referred to home health from each hospital for each FY
use CMShospdisch_tohh, clear
rename provider_hosp prvdr_num

* create fiscal year
gen fy = .
forval y=2012/2016 {
  loc yl1 = `y'-1
  replace fy = `y' if monday >= mdy(10,1,`yl1') & monday <= mdy(9,30,`y')
}
assert fy!=.

*gen ym = ym(year(monday), month(monday))
collapse (sum) hdis2hh, by(prvdr_num `tunit')

tempfile totref_fromhosp
save `totref_fromhosp'

* compute the total number of patients referred to Bayada office from each hospital for each FY
use referralhosp_mcrID, clear

* create fiscal year - just for consistency, first create monday of week, & aggregate from the weekly data
*get the monday of the week to which SOC belongs
gen day = dow(socdate)
gen monday = socdate - 6 if day==0
forval d = 1/6 {
  loc d2 = `d' - 1
  replace monday = socdate - `d2' if day==`d'
}
format monday %d
drop day
assert socdate!=.
assert monday!=.
lab var monday "monday of the SOC date of the admission"

gen fy = .
forval y=2012/2016 {
  loc yl1 = `y'-1
  replace fy = `y' if monday >= mdy(10,1,`yl1') & monday <= mdy(9,30,`y')
}
assert fy!=.

gen i = 1
collapse (sum) hospref_to_office = i, by(prvdr_num offid_nu `tunit')

*merge with hospital-FY level total # referrals to home health to compute the share of pats to each Bayada office from hospital-FY
merge m:1 prvdr_num `tunit' using `totref_fromhosp'
keep if _m==3
drop _m

gen shref_jht = hospref_to_office/hdis2hh
sum shref_jht, de
*may have to drop the share is > 99th percentile (3.1)

destring prvdr_num, replace

tempfile shref_jht
save `shref_jht'

*-----------
use `an2', clear

*restructure data to the office-hospital-FY level
keep offid_nu prvdr_num fyear _I* penalty_gtmed* ami hf pneu copd
collapse (sum) ami hf pneu copd, by(offid_nu prvdr_num fyear _I* penalty_gtmed*)
duplicates tag offid_nu prvdr_num fyear, gen(dup)
assert dup==0
drop dup

*link with the office-hospital-FY level share of patients from the hospital to the office during FY
rename fyear fy
merge 1:1 offid_nu prvdr_num fy using `shref_jht', keep(3) nogen

foreach c in "ami" "hf" "pneu" "copd" {
  gen shref_jht_`c' = `c'/hdis2hh
}
lab var shref_jht "Share of all patients referred to office from hospital during FY"
lab var shref_jht_ami "Share of AMI patients referred to office from hospital during FY"
lab var shref_jht_hf "Share of HF patients referred to office from hospital during FY"
lab var shref_jht_copd "Share of COPD patients referred to office from hospital during FY"
lab var shref_jht_pneu "Share of PNEU patients referred to office from hospital during FY"

rename fy fyear

*if the total share for the office-hospital-FY pair has > 99th percentile, then remove it
sum shref_jht, de
drop if shref_jht > r(p99)
rename shref_jht shref_jht_

*-------------
loc file _shref
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append"

foreach c in "" "hf" "pneu" "ami" "copd" {
  loc yv shref_jht_`c'

  areg `yv' penalty_gtmed _Ifyear_2014 _Ifyear_2015 penalty_gtmed_fy2014 penalty_gtmed_fy2015 i.offid_nu if fyear >= 2013,  vce(cluster offid_nu) absorb(prvdr_num)
  *areg `yv' penalty_gtmed _Ifyear_2014 _Ifyear_2015 penalty_gtmed_fy2014 penalty_gtmed_fy2015 i.offid_nu if `c'==1,  vce(cluster offid_nu) absorb(prvdr_num)

  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'

  loc rsq: display %9.2f e(r2)
  loc ar2: display %9.2f `e(r2_a)'

  `out' keep(penalty_gtmed _Ifyear_201? penalty_gtmed_fy201?) label addtext(Adjusted R-squared, `ar2', Mean dep. var., `mdv')
}

loc file _shref2
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append"

foreach c in "" "hf" "pneu" "ami" "copd" {
  loc yv shref_jht_`c'
  areg `yv' penalty_gtmed2012 _Ifyear_2013 _Ifyear_2014 _Ifyear_2015 penalty_gtmed2012_fy2013 penalty_gtmed2012_fy2014 penalty_gtmed2012_fy2015 i.offid_nu,  vce(cluster offid_nu) absorb(prvdr_num)

  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'

  loc rsq: display %9.2f e(r2)
  loc ar2: display %9.2f `e(r2_a)'

  `out' keep(penalty_gtmed2012 _Ifyear_201? penalty_gtmed2012_fy201?) label addtext(Adjusted R-squared, `ar2', Mean dep. var., `mdv')
}
