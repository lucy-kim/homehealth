* create episode-level data starting for fy 2013-2015 where fy is a year ending June

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults

cd `path'

*use the single-episode patients and measure the efforts
*restrict to AMI, HF, PNEU, COBG conditions
use pats_hospreferred, clear
merge 1:m clientid socdate_e using inpat_dx, keep(3) nogen
*dropped 3522 obs (2%)

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
rename icd2 inpat_dx

*restrict to fy 2013-2015 data

* create fiscal year
capture drop fy
gen fy = .
forval y=2012/2016 {
  loc yl1 = `y'-1
  replace fy = `y' if socdate >= mdy(7,1,`yl1') & socdate <= mdy(6,30,`y')
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

* create visit-level data

*first get epi ID by merging with admission ID - episode ID xwalk
use `an', clear
keep admissionclientsocid fy provider_hosp
duplicates drop
merge 1:m admissionclientsocid using epiid_admitID, keep(1 3) nogen
*all matched

*drop if matched to multiple episodes
duplicates tag admissionclientsocid, gen(dup)
drop if dup > 0
drop dup

*merge with visit-level data for the episodes
destring admissionclie, replace
merge 1:m epiid using epi_visit, keep(1 3) nogen
*all matched
drop if offid_nu ==.

list epiid visitdate hashos firsthospdate  hadvisit_onra if  epiid==550247

*drop episodes that had a visit on the day of readmission
sort epiid visitdate
gen hadvisit_onra = firsthospdate == visitdate & hashosp==1
bys epiid: egen max = max(hadvisit_onra)
bys epiid: gen i = _n==1
tab max if i==1
*5% episodes (26K) have visit on the day of readmission
drop hadvisit_onra i
rename max hadvisit_onra
*drop if max==1

* duration of episode: make sure we have single episode cases
capture drop epilength
egen llvd = rowmax(firsthospdate lvd)
format llvd %d
gen epilength = llvd - fvd + 1

sort epiid visitdate
bys epiid: gen i =_n==1
count if i==1
*54K episodes
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

gen firstwk = dayidx <= 7
gen first2wk = dayidx <= 14
gen second_lastwk = wkidx!=1

count if epilength > 14 & i==1
count if i==1
*78% of episodes had duration of > 2 weeks
drop i

duplicates drop

compress
save HHeffort_visit, replace

*-----------------------------
*construct effort measures using the visit-level data
use HHeffort_visit, clear

assert discipline!=""

*instead of dropping, recode to missing if visit length < p1 or > p99
drop if lov==0
replace lov = lov *60
sum lov, de
loc p1 = `r(p1)'
loc p99 = `r(p99)'
*replace lov = . if lov < `p1' | lov > `p99'
gen x = lov < `p1' | lov > `p99'
bys epiid: egen outlierLOV = max(x)
drop if outlierLOV==1
drop x outlierLOV

*drop visits if visittime is missing
drop if visittime==.

duplicates tag epiid visitdate discipline, gen(dup)
tab dup
drop dup

*merge with summary index of costs
merge m:1 epiid visitdate discipline using resource_index, keep(1 3) nogen

*total # visits during episode , visit length
gen tnv = 1
gen tnvsn = discipline=="SN"
gen lovsn = lov if discipline=="SN"

*during the entire episode
preserve
collapse (sum) tnv tnvsn vtc_tr_pay (mean) lov lovsn, by(epiid epilength)
tempfile effort1
save `effort1'
restore

*by whether the episode is first week or the rest
preserve
collapse (sum) tnv tnvsn vtc_tr_pay (mean) lov lovsn, by(epiid firstwk)
foreach v of varlist tnv tnvsn vtc_tr_pay lov lovsn {
  renam `v' `v'_1stwk
}
reshape wide *_1stwk, i(epiid) j(firstwk)
tempfile effort2
save `effort2'
restore

*--------------------------
*restrict to episodes without a visit on the day of readmission, who had a hospital stay before HH, who didn't have AIDS or blooad anemia, < age 65
use HHeffort_visit, clear

keep if facility=="Hosp"

*drop AIDS & blood anemia
drop if ynel16==1 | ynel25==1

drop if age < 65

*5-year age bins
egen age5yr = cut(age), at(65,70,75,80,85,90,95)
replace age5yr = 95 if age > 94
assert age5yr!=.

*create episode level 30-day hospital readmission indicator
*get 30-day readmission indicator
gen days2hosp = firsthospdate - inpat_dcd
gen hashosp30 = hashosp * (days2hosp <= 30)

lab var hashosp "Any hospital readmission indicator"
lab var hashosp30 "30-day hospital readmission indicator"

loc riskhosp riskhosp_fall riskhosp_manyhos riskhosp_mental riskhosp_ge5 riskhosp_oth
loc priorcond priorcond_dis priorcond_impd priorcond_cath priorcond_pain priorcond_mem priorcond_inc
loc hrfactor hrfactor_alco hrfactor_drug hrfactor_smoke hrfactor_obese
loc overallst overallst_vbad overallst_bad overallst_tem
loc ins ma_visit ma_epi dual
loc ages age5yr
loc demog `ages' female white noassist livealone `ins'
loc comorbid ynch* `overallst' `hrfactor' `priorcond'

loc chars `comorbid' `riskhosp' `demog'
loc vars epiid provider_hosp socdate2 offid_nu clientid fy inpat_dcd ami hf pneu copd hashosp* days2hosp age hadvisit_onra
keep `chars' `vars'
duplicates drop

rename pneu pn
*create indicator for whether episode had 4 HRRP conditions
gen hrrpcond = ami==1 | hf==1 | copd==1 | pn==1

*Indicator for whether the HH episode started within 1 day from hospital discharge
gen time2hh = socdate2 - inpat_dcdate_e
drop if time2hh < 0 | time2hh > 30
tab time2hh
*all the episodes started HH within 14 days from hospital discharge
gen startHH_1day = time2hh <=1 if time2hh!=.

tab hashosp
tab hashosp30

*merge with efforts data at the episode level
merge 1:1 epiid using `effort1', keep(3) nogen
merge 1:1 epiid using `effort2', keep(3) nogen

*create frequency of visits = # visits / episode length
foreach v of varlist tnv tnvsn {
  gen freq_`v' = `v'/epilength
}
gen epilength_1stwk0 = epilength - 7
replace epilength_1stwk0 = . if epilength <= 7
foreach v of varlist tnv_1stwk0 tnvsn_1stwk0 {
  gen freq_`v' = `v'/epilength_1stwk0
}
gen epilength_1stwk1 = epilength if epilength <= 7
replace epilength_1stwk1 = 7 if epilength > 7
assert epilength_1stwk1!=.
foreach v of varlist tnv_1stwk1 tnvsn_1stwk1 {
  gen freq_`v' = `v'/epilength_1stwk1
}

*drop 1 patient who had 158 visits (the next largest value = 56)
drop if tnvsn==158

*merge by patient's referring hospital and office, the penalty pressure data
rename provider_hosp prvdr_num
destring prvdr_num, replace
merge m:1 offid_nu prvdr_num using HRRPpnlty_pressure_hj_2012, keep(3) nogen keepusing(pnltprs* z_pnltprs*)

gen ma = ma_visit==1 | ma_epi==1
gen tm = 1-ma

*penalty pressure in 2012 for each condition
drop if copd==1

gen cond = 4
gen pnltprs_c = .
loc i = 1
foreach d in "ami" "hf" "pn" {
  replace cond = `i' if `d'==1
  loc i = `i'+1
  replace pnltprs_c = pnltprs_`d' if `d'==1
}
replace pnltprs_c = 0 if hrrpcond==0
assert pnltprs_c!=.
assert cond!=.
assert cond==4 if hrrpcond==0
lab define cl 1 "AMI" 2 "HF" 3 "PN" 4 "Non-target"
lab val cond cl

*drop episodes if the total summary cost ==0
drop if vtc_tr_pay==0

egen hrrpcond_count = rowtotal(ami hf pn)
tab hrrpcond_count
drop if hrrpcond_count > 1
drop hrrpcond_count

foreach v of varlist vtc_tr_pay* epilength* lov* {
  gen ln`v' = ln(`v'+1)
}

loc uami "AMI"
loc uhf "HF"
loc upn "PN"
foreach d in "ami" "hf" "pn" {
  capture drop pnltprs_c_X_`d'
  gen pnltprs_c_X_`d' = pnltprs_c *`d'
  gen pnltprs_X_`d' = pnltprs * `d'

  lab var pnltprs_c_X_`d' "Condition-specific penalty salience X `u`d''"
  lab var `d' "Indicator for `u`d''"
  lab var pnltprs_X_`d' "Aggregate penalty salience X `u`d''"
}

tempfile an2
save `an2'

*-----------
*penalty pressure defined to 0 for non-target conditions
gen pp = pnltprs_c > 0
assert pp!=.
tab pp, summarize(pnltprs_c)
tab hrrpcond pp

drop pp
gen pp = pnltprs > 0
assert pp!=.
tab pp, summarize(pnltprs)
tab hrrpcond pp

*-----------

*get total number of episodes going on, # active workers in the office on each day
use epi_visit, clear
keep offid_nu visitdate epiid payrollno
duplicates drop
keep if visitdate <= mdy(7,1,2015)

preserve
drop payrollno
duplicates drop
assert epiid!=.
gen allepi = 1
collapse (sum) allepi, by(offid_nu visitdate)
*subtract 1 to not count the episode that starts on that day
replace allepi = allepi - 1
gen lnallepi = ln(allepi)
tempfile allepi
save `allepi'
restore

preserve
drop epiid
duplicates drop
assert payrollno!=""
gen nw_active_worker = 1
collapse (sum) nw_active_worker, by(offid_nu visitdate)
gen lnnw_active_worker = ln(nw_active_worker)
tempfile nw_active_worker
save `nw_active_worker'
restore

*get characteristics of the referring hospital
use hosp_chars_cr, clear
keep if fy >=2013 & fy <= 2015
keep provid fy vi_hha teaching urban own_* size beds
rename provid prvdr_num
duplicates drop
tempfile hospchars
save `hospchars'

use `an2', clear
merge m:1 prvdr_num fy using `hospchars', keep(3) nogen

rename socdate visitdate_e
merge m:1 offid_nu visitdate_e using `allepi', keep(1 3) nogen
merge m:1 offid_nu visitdate_e using `nw_active_worker', keep(1 3) nogen

compress
save epilvl_rehosp_smpl, replace
