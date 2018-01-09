*compare HH efforts by whether patient was before or after 30 days from hospital discharge, penalty pressure, or HRRP or non-HRRP condition
*create patient-week level data containing referring hospital's penalty pressure, some initial set of effort measures and covariates
*initial set of effort measures: mean length of visit (per week), # visits, % visits by nurses

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults

cd `path'

*use the single-episode patients and measure the efforts
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


*restrict to fy 2013-2015 data

* create fiscal year
capture drop fy
gen fy = .
forval y=2012/2016 {
  loc yl1 = `y'-1
  replace fy = `y' if socdate >= mdy(10,1,`yl1') & socdate <= mdy(9,30,`y')
}
assert fy!=.
drop if fy==2012

*create data unique at the admission ID-office ID level
keep admissionclie offid_nu ami hf pneu copd provider_hosp fy
duplicates drop
collapse (max) ami hf pneu copd, by(admissionclie offid_nu provider_hosp fy)
duplicates tag admissionclie offid_nu, gen(dup)
assert dup==0
drop dup

tempfile an
save `an'

*-----------------------------
* construct effort measures by merging with visit-level data

*first get epi ID by merging with admission ID - episode ID xwalk
use `an', clear
keep admissionclientsocid fy
duplicates drop
merge 1:m admissionclientsocid using epiid_admitID, keep(1 3) nogen
*all matched

*drop if matched to multiple episodes
duplicates tag admissionclientsocid, gen(dup)
drop if dup > 0

*merge with visit-level data for the episodes
destring admissionclie, replace
merge 1:m epiid using epi_visit, keep(1 3) nogen
*all matched
drop if offid_nu ==.

*--------------------------
*drop episodes that had a visit on the day of readmission
sort epiid visitdate

gen hadvisit_onra = firsthospdate == visitdate & hashosp==1
bys epiid: egen max = max(hadvisit_onra)
drop if max==1
drop max hadvisit_onra

tempfile tmp
save `tmp'
*-----------------------------
*merge by patient's admission ID-office ID, data on the referring hospital ID and HRRP conditions with visit-level data
use `an', clear
destring admissionclie, replace
merge 1:m offid_nu admissionclie using `tmp', keep(3) nogen
*7586 admission obs unmatched (_m=1) b/c I dropped episodes who had a visit on the day of readmission - drop them
*-----------------------------
*merge by patient's referring hospital and office, the penalty pressure data
rename provider_hosp prvdr_num
destring prvdr_num, replace
merge m:1 offid_nu prvdr_num using HRRPpnlty_pressure_hj_2012, keep(3) nogen keepusing(pnltprs z_pnltprs)

sort epiid visitdate
*-----------------------------
* duration of episode: make sure we have single episode cases
gen epilength = lvd - fvd + 1

bys epiid: gen i =_n==1
count if i==1
*35,803 episodes
tab epilength if i==1
drop if epilength > 60

*create indicators for the 1st week, 2nd week, 1st 2 weeks
assert fvd==visitd if i==1
gen dayidx = visitd - fvd + 1
gen wkidx = 1 if dayidx <= 7
forval x=2/9 {
  replace wkidx = `x' if dayidx > 7*(`x'-1) & dayidx <= 7*`x'
}
assert wkidx!=.
assert wkidx ==2 if dayidx > 7 & dayidx <= 14

gen first2wk = dayidx <= 14

count if epilength > 14 & i==1
count if i==1
*78% of episodes had duration of > 2 weeks

*-----------------------------
* construct effort measures

* construct total # visits during episode
bys epiid: egen tnvall = sum(1)
* later drop outliers: if tnv > 60
bys epiid: egen tnvsn = sum(1) if (jobcode=="LPN" | jobcode=="SN" | jobcode=="RN")

*mean visit length by time periods
bys epiid wkidx: egen mvl = mean(tot_time) if fvd!=visitdate & lvd!=visitdate
bys epiid wkidx: egen mvlsn = mean(tot_time) if  fvd!=visitdate & lvd!=visitdate & (jobcode=="LPN" | jobcode=="SN" | jobcode=="RN")

*# visits / week
bys epiid wkidx: egen nvall = sum(1)
bys epiid wkidx: egen nvsn = sum(1) if (jobcode=="LPN" | jobcode=="SN" | jobcode=="RN")

*% visits in each time period
gen pnvall = 100* nvall / tnvall
gen pnvsn = 100* nvsn / tnvsn if (jobcode=="LPN" | jobcode=="SN" | jobcode=="RN")


/**create episode-week index level data (long shape)
foreach v in "wkidx" "mvl" "mvlsn" "nvall" "nvsn" "pnvall" "pnvsn" {
  gen `v'=.
}
forval x=1/9 {
  replace wkidx = `x' if wk`x'==1

  foreach v in "mvl" "mvlsn" "nvall" "nvsn" "pnvall" "pnvsn" {
    replace `v' = `v'_wk`x' if wkidx==`x'
  }
}*/

list mvl mvlsn nvall nvsn pnvall pnvsn tnvall tnvsn wkidx visitd dayidx if epiid==121643

preserve
collapse (max) mvl mvlsn nvall nvsn pnvall pnvsn tnvall tnvsn, by(epiid wkidx)
list mvl mvlsn nvall nvsn pnvall pnvsn tnvall tnvsn wkidx if epiid==121643
foreach v of varlist tnv* pnv* nv* {
  replace `v' = 0 if `v'==.
}
tempfile epi_wkidx
save `epi_wkidx'
restore

drop tnv* nv* mvl* pnv*
merge m:1 epiid wkidx using `epi_wkidx', nogen keep(1 3)

drop if age < 65
*create episode-level risk factors
*5-year age bins
egen age5yr = cut(age), at(65,70,75,80,85,90,95)
replace age5yr = 95 if age > 94
assert age5yr!=.
table age5yr, contents(min age max age)

compress
save HHeffort_visit, replace

*--------------------------
*regression analysis
use HHeffort_visit, clear

tw histogram epilength if i==1, ti("Distribution of the home health episode duration") width(1) xti("Home health episode duration in days") frac
graph export `gph'/epilength.pdf, replace

tw histogram epilength if i==1, ti("Distribution of the home health episode duration") width(1) xti("Home health episode duration in days") freq
graph export `gph'/epilength2.pdf, replace

*--------------------------------
*create patient episode-week level dataset
use HHeffort_visit, clear

*create indicator for whether episode had 4 HRRP conditions
gen hrrpcond = ami==1 | hf==1 | copd==1 | pneu==1

*how many episodes are there on each week?
keep epiid `riskhosp' age5yr female white noassist livealone `ins' `comorbid' wkidx *pnltprs tnv* nv* mvl* pnv* hrrpcond prvdr epilength offid_nu fy hashosp
duplicates drop

*episode duration in number of weeks
bys epiid: egen epilength_wk = max(wkidx)
sort epiid wkidx
bys epiid: gen i = _n==1
tab epilength_wk if i==1

tw histogram epilength_wk if i==1, ti("Distribution of the home health episode duration") xti("Home health episode duration in weeks") freq xsc(range(0 9))
graph export `gph'/epilength3.pdf, replace


gen z_pnltprs_X_hrrpcond = z_pnltprs * hrrpcond
lab var z_pnltprs_X_hrrpcond "Penalty pressure X HRRP condition"
lab var z_pnltprs "Penalty pressure"
lab var hrrpcond "Indicator for HRRP condition"

*regression macros
loc ages i.age5yr
loc riskhosp riskhosp_fall riskhosp_manyhos riskhosp_mental riskhosp_ge5 riskhosp_oth
loc priorcond priorcond_dis priorcond_impd priorcond_cath priorcond_pain priorcond_mem priorcond_inc
loc hrfactor hrfactor_alco hrfactor_drug hrfactor_smoke hrfactor_obese
loc overallst overallst_vbad overallst_bad overallst_tem
loc ins ma_visit ma_epi dual
loc demog `ages' female white noassist livealone `ins'
loc comorbid charlindex `overallst' `hrfactor' `priorcond'
*ynch*  depressed

loc sp1 `riskhosp'
loc sp2 `sp1' `demog'
loc sp3 `sp2' `comorbid' i.fy

*for mean visit lengths, convert to minutes
foreach v of varlist mvl* {
  replace `v' = `v'*60
}

*take logs for # visits & length of visits
foreach v of varlist mvl* nv* {
  gen ln`v' = ln(`v')
}
*tnv*
*nv* mvl* pnv*

*analysis of efforts separately by week among those not readmitted
foreach yv of varlist lnmvl* lnnv* pnv* {
  loc file HHeffort2_`yv'_allpat
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append"

  forval t=1/9 {
    areg `yv' z_pnltprs hrrpcond z_pnltprs_X_hrrpcond `sp3' if wk==`t', absorb(offid_nu) vce(cluster offid_nu)

    sum `yv' if e(sample)
    loc mdv: display %9.2f `r(mean)'
    loc ar2: display %9.2f `e(r2_a)'

    `out' ctitle(Week `t') keep(z_pnltprs hrrpcond z_pnltprs_X_hrrpcond) label addtext(Adjusted R-squared, `ar2', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y)
  }
}

*analysis of efforts separately by week among those not readmitted
foreach yv of varlist mvl* nv* pnv* {
  loc file HHeffort2_`yv'_nora
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append"

  forval t=1/9 {
    areg `yv' z_pnltprs hrrpcond z_pnltprs_X_hrrpcond `sp3' if wk==`t' & hashosp==0, absorb(offid_nu) vce(cluster offid_nu)

    sum `yv' if e(sample)
    loc mdv: display %9.2f `r(mean)'
    loc ar2: display %9.2f `e(r2_a)'

    `out' ctitle(Week `t') keep(z_pnltprs hrrpcond z_pnltprs_X_hrrpcond) label addtext(Adjusted R-squared, `ar2', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y)
  }
}

*----------------------
*analysis of efforts per week in a single model by controlling for week index FE
loc l_mvl "Mean visit length"
loc l_mvlsn "Mean nurse visit length"
loc l_nvall "# All visits"
loc l_nvsn "# Nurse visits"
loc l_pnvall "% All visits"
loc l_pnvsn "% Nurse visits"

loc sp1 `riskhosp' i.wk
loc sp2 `sp1' `demog'
loc sp3 `sp2' `comorbid' i.fy

loc file HHeffort2_allpat
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append"

foreach yv of varlist mvl* nv* pnv* {
  areg `yv' z_pnltprs hrrpcond z_pnltprs_X_hrrpcond `sp3' i.wkidx , absorb(offid_nu) vce(cluster offid_nu)
  *if hashosp==0
  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  `out' ctitle(`l_`yv'') keep(z_pnltprs hrrpcond z_pnltprs_X_hrrpcond) label addtext(Adjusted R-squared, `ar2', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Home health week FE, Y)
}

*non-readmitted pats
loc file HHeffort2_nonrehosp_pat
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append"

foreach yv of varlist mvl* nv* pnv* {
  areg `yv' z_pnltprs hrrpcond z_pnltprs_X_hrrpcond `sp3' i.wkidx if hashosp==0, absorb(offid_nu) vce(cluster offid_nu)

  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'
  loc ar2: display %9.2f `e(r2_a)'

  `out' ctitle(`l_`yv'') keep(z_pnltprs hrrpcond z_pnltprs_X_hrrpcond) label addtext(Adjusted R-squared, `ar2', Mean dep. var., `mdv', Office FE, Y, Fiscal Year FE, Y, Home health week FE, Y)
}
