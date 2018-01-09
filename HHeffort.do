* examine the impact of the referring hospitals' HRRP penalty pressure on HHAs' efforts on patients

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

tempfile an
save `an'

*-----------------------------
*for each hospital that referred the patient, compute the penalty pressure

*first, standardize the penalty pressure
/*use penalty_pressure_oh, clear
egen std_pnlt = std(pnltprssure)

tempfile std_pnlt
save `std_pnlt'*/

use `an', clear
rename provider_hosp prvdr_num
destring prvdr_num, replace
merge m:1 offid_nu prvdr_num using penalty_pressure_oh, keep(3) nogen
*24K have _m=1; 127K have _m=3

drop icd2
duplicates drop

tempfile an2
save `an2'
*-----------------------------
* construct effort measures

*first get epi ID by merging with admission ID - episode ID xwalk
use `an2', clear
keep admissionclientsocid
duplicates drop
merge 1:m admissionclientsocid using epiid_admitID, keep(1 3) nogen
*all matched

*drop if matched to multiple episodes
duplicates tag admissionclientsocid, gen(dup)
drop if dup > 0

*merge with visit-level data for the episodes
merge 1:m epiid using staff_visit_office, keep(1 3) nogen
*all matched
drop if offid_nu ==.

tempfile visit
save `visit'

*total time spent (min), # visits, share of nurse visits in the first 30 days
use `visit', clear
keep epiid visitdate socdate visittime tot_time jobcode
gen elapsed = visitdate - socdate + 1
sort epiid visitd
keep if elapsed <= 30
duplicates drop

* tag visits
gen i = 1

collapse (sum) tot_time nv = i, by(epiid jobcode)
bys epiid: egen tnv = sum(nv)
bys epiid: egen ttot_time = sum(tot_time)

drop if jobcode==""
*6 obs

reshape wide tot_time nv, i(epiid ttot_time tnv) j(jobcode) string

keep epiid tnv ttot_time *LPN* *RN* *SN*

*sum RN & SN
foreach x in "nv" "tot_time" {
  egen `x'_SN2 = rowtotal(`x'RN `x'SN)
  drop `x'RN `x'SN
  rename `x'_SN2 `x'SN
}

*share of nurse visits in first 30 days
gen snvSN = nvSN/tnv
gen snvLPN = nvLPN/tnv

tempfile effort
save `effort'

*--------------------------
*create episode-level data and merge with effort measure data
use `visit', clear
keep admissionclie epiid
duplicates drop

merge 1:1 epiid using `effort', keep(3) nogen
*2 had _m=1

merge 1:m admissionclie using `an2', keep(3) nogen
*17K have _m=2 b/c I dropped later multiple-episode admissions

tempfile an3
save `an3'
*--------------------------
*reg
use `an3', clear

*convert time to minutes
foreach v of varlist ttot_time tot_timeSN {
  replace `v' = `v'*60
}

*set penalty pressure = 0 if conditions have non-HRRP conditions
replace pnltprssure = 0 if ami==0 & pneu==0 & hf==0 & copd==0

egen std_pnlt = std(pnltprssure)

xi i.fy

lab var std_pnlt "Standardized penalty pressure"
lab var riskhosp_ge5med "Taking 6+ meds"
lab var overallst_bad "Overall state likely to be fragile"
lab var livealone "Living alone"
lab var noassist "Having no assistance available"

loc file HHeffort
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append"

loc sp std_pnlt charlindex riskhosp_ge5med overallst_bad livealone noassist _Ify*

*drop outlier value for time
sum ttot_time, de
drop if ttot_time > `r(99)'

*all patients
foreach yv of varlist ttot_time tnv tot_timeSN snvSN snvLPN {
  areg `yv' `sp', absorb(offid_nu) vce(cluster offid_nu)

  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'

  loc rsq: display %9.2f e(r2)
  loc ar2: display %9.2f `e(r2_a)'

  `out' keep(`sp') label addtext(Adjusted R-squared, `ar2', Mean dep. var., `mdv', Referring hospital FE, N)

  *add hospital FE
  areg `yv' `sp' i.offid_nu, absorb(prvdr_num) vce(cluster offid_nu)

  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'

  loc rsq: display %9.2f e(r2)
  loc ar2: display %9.2f `e(r2_a)'

  `out' keep(`sp') label addtext(Adjusted R-squared, `ar2', Mean dep. var., `mdv', Referring hospital FE, Y)
}

foreach d in "ami" "hf" "pneu" "copd" {
  loc file HHeffort_`d'
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append"

  foreach yv of varlist ttot_time tnv tot_timeSN snvSN snvLPN {
    areg `yv' `sp' if `d'==1, absorb(offid_nu) vce(cluster offid_nu)

    sum `yv' if e(sample)
    loc mdv: display %9.2f `r(mean)'

    loc rsq: display %9.2f e(r2)
    loc ar2: display %9.2f `e(r2_a)'

    `out' keep(`sp') label addtext(Adjusted R-squared, `ar2', Mean dep. var., `mdv', Referring hospital FE, N)

    *add hospital FE
    areg `yv' `sp' i.offid_nu if `d'==1, absorb(prvdr_num) vce(cluster offid_nu)

    sum `yv' if e(sample)
    loc mdv: display %9.2f `r(mean)'

    loc rsq: display %9.2f e(r2)
    loc ar2: display %9.2f `e(r2_a)'

    `out' keep(`sp') label addtext(Adjusted R-squared, `ar2', Mean dep. var., `mdv', Referring hospital FE, Y)
  }
}
