* create episode-level data for fy 2012 where fy is a year ending June
* to interpret the magnitude of our estimates, want to compare the effort levels on the healthiest and sickest people at baseline (2012); get 2012 effort values

*this dofile contains codes from crHHeffort_visit.do, crresource_index.do, crepilvl_rehosp_smpl.do

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults

cd `path'

*-----------------------------
* adpated from crHHeffort_visit.do
*-----------------------------
*use the single-episode patients and measure the efforts
*restrict to AMI, HF, PNEU, COBG conditions
use pats_hospreferred, clear
merge 1:m clientid socdate_e using inpat_dx, keep(3) nogen
*dropped 3522 obs (2%)

*for ICD-9-CM codes for each condition, see Tables D.X.1  https://www.qualitynet.org/dcs/ContentServer?cid=1228774371008&pagename=QnetPublic%2FPage%2FQnetTier4&c=Page
gen icd2 = inpat_dx_cor

*remove "."
replace icd2 = subinstr(icd2, ".", "",.)
destring icd2, replace

* AMI (from Table D.1.1 in 2016 report)
gen ami = icd2==41000 | icd2==41001 | icd2==41010 | icd2==41011 | icd2==41020 | icd2==41021 | icd2==41030 | icd2==41031 | icd2==41040 | icd2==41041 | icd2==41050 | icd2==41051 | icd2==41060 | icd2==41061 | icd2==41070 | icd2==41071 | icd2==41080 | icd2==41081 | icd2==41090 | icd2==41091

* HF
gen hf = icd2==40201 | icd2==40211 | icd2==40291 | icd2==40401 | icd2==40403 | icd2==40411 | icd2==40413 | icd2==40491 | icd2==40493 | icd2==4280 | icd2==4281 | icd2==42820 | icd2==42821 | icd2==42822 | icd2==42823 | icd2==42830 | icd2==42831 | icd2==42832 | icd2==42833 | icd2==42840 | icd2==42841 | icd2==42842 | icd2==42843 | icd2==4289

*PNEU
gen pn = icd2==4800 | icd2==4801 | icd2==4802 | icd2==4803 | icd2==4808 | icd2==4809 | icd2==481 | icd2==4820 | icd2==4821 | icd2==4822 | icd2==48230 | icd2==48231 | icd2==48232 | icd2==48239 | icd2==48240 | icd2==48241 | icd2==48242 | icd2==48249 | icd2==48281 | icd2==48282 | icd2==48283 | icd2==48284 | icd2==48289 | icd2==4829 | icd2==4830 | icd2==4831 | icd2==4838 | icd2==485 | icd2==486 | icd2==4870 | icd2==48811 | icd2==5070

* COPD
gen copd = inpat_dx_cor=="491.2" | inpat_dx_cor=="491.21" | inpat_dx_cor=="491.22" | inpat_dx_cor=="491.8" | inpat_dx_cor=="491.9" | inpat_dx_cor=="492.8" | inpat_dx_cor=="493.20" | inpat_dx_cor=="493.21" | inpat_dx_cor=="493.22" | inpat_dx_cor=="496"

*Stroke
gen stroke = icd2==43301 | icd2==43311 | icd2==43321 | icd2==43331 | icd2==43381 | icd2==43391 | icd2==43401 | icd2==43411 | icd2==43491 | icd2==436

drop icd2

*identify cardiorespiratory conditions from the control groups
split inpat_dx_cor, p(".")
destring inpat_dx_cor1, replace
gen x = inpat_dx_cor1>=390 & inpat_dx_cor1 <= 519
bys admitID_num: egen cardioresp = max(x)
drop x inpat_dx inpat_dx_cor icd-icddesc inpat_dx_cor1 inpat_dx_cor2
duplicates drop

*restrict to fy 2013-2015 data

* create fiscal year
capture drop fy
gen fy = .
forval y=2012/2016 {
  loc yl1 = `y'-1
  replace fy = `y' if socdate >= mdy(7,1,`yl1') & socdate <= mdy(6,30,`y')
}
assert fy!=.
keep if fy==2012

*create data unique at the admission ID-office ID level
keep admissionclie offid_nu ami hf pn copd stroke cardioresp provider_hosp fy
duplicates drop
collapse (max) ami hf pn copd stroke cardioresp, by(admissionclie offid_nu provider_hosp fy)
duplicates tag admissionclie offid_nu, gen(dup)
assert dup==0
drop dup

tempfile an
save `an'

*-----------------------------

* create visit-level data
use epi_visit, clear
keep if nepi==1
drop if offid_nu ==.
tostring admissionclientsocid, gen(admissionclientsocid_str) format("%11.0f")
drop admissionclientsocid
rename admissionclientsocid_str admissionclientsocid

foreach cc in "ami" "hf" "pn" "pneu" "pneu_new" "copd" {
  capture drop `cc'
}

*merge with sample patients for our project
merge m:1 admissionclientsocid using `an', keep(3) nogen

*drop episodes that had a visit on the day of readmission
sort epiid visitdate
gen hadvisit_onra = firsthospdate == visitdate & hashosp==1
bys epiid: egen max = max(hadvisit_onra)
drop if max==1
drop max hadvisit_onra

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

*-------------
*create 30-day readmission indicator in the first week (turn off if the readmission occurred after first week)

*create episode level 30-day hospital readmission indicator
gen days2hosp = firsthospdate - inpat_dcd
gen start_1stwk = fvd
gen end_1stwk = fvd+6
gen hospoccur_1stwk = firsthospdate >= start_1stwk & firsthospdate <= end_1stwk
assert hospoccur_1stwk==0 if firsthospdate==.

gen hashosp30 = hashosp * (days2hosp <= 30)
gen hashosp30_1stwk1 = hashosp * (days2hosp <= 30) * hospoccur_1stwk
gen hashosp30_1stwk0 = hashosp * (days2hosp <= 30) * (1-hospoccur_1stwk)
assert (hashosp30_1stwk1+ hashosp30_1stwk0==1 ) |  (hashosp30_1stwk1 + hashosp30_1stwk0==0)
assert (hashosp30_1stwk1==1 | hashosp30_1stwk0==1 ) if hashosp30==1

lab var hashosp30 "30-day hospital readmission"
lab var hashosp30_1stwk1 "30-day hospital readmission in the first week"
lab var hashosp30_1stwk0 "30-day hospital readmission beyond the first week"

*-------------
*define readmission within 30 days of the start of the episode
gen days2hosp_fromHHstart = firsthospdate - fvd
gen hashosp30_fromHHstart = hashosp * (days2hosp_fromHHstart <= 30)
lab var hashosp30_fromHHstart "readmission within 30 days of the start of the episode"

tempfile HHeffort_visit
save `HHeffort_visit'

*-----------------------------
* adpated from crresource_index.do
*-----------------------------
*get resource index using 2012 data
use payrate, clear
collapse (mean) payrate, by(payrollno monday)

sort payrollno monday
bys payrollno: replace payrate=payrate[_n-1] if payrate >=.

gsort payrollno -monday
bys payrollno: replace payrate=payrate[_n-1] if payrate >=.

bys payrollno: egen x = mean(payrate)
drop if x==.
drop x

*since the pay rate is only available from 11/26/2012, just use the last value for each worker & just get one value per worker
sort payrollno monday
drop if payrate==1
bys payrollno: keep if _n==1
drop monday
duplicates drop

tempfile payrate
save `payrate'

use `HHeffort_visit', clear

keep epiid visitdate payrollno monday visit_tot_cost visit_travel_cost status jobcode offid_nu
duplicates drop

merge m:1 payrollno using `payrate', keep(1 3)
*since the pay rate is only available from 11/26/2012, just use the last value for each worker

gsort payrollno -monday -payrate
bys payrollno: replace payrate = payrate[_n-1] if payrate >=.

tempfile tmp
save `tmp'

use `tmp', clear

*get average value of cost for each pay discipline
gen paydisc = jobcode
replace paydisc = "OT" if jobcode=="OTA"
replace paydisc = "PT" if jobcode=="PTA"
replace paydisc = "RN" if jobcode=="SN"
tab paydisc

bys offid_nu monday paydisc: egen mean = mean(payrate)
bys offid_nu paydisc: egen mean2 = mean(payrate)
bys paydisc: egen mean3 = mean(payrate)
assert mean3!=.
replace mean = mean2 if mean ==.
replace mean = mean3 if mean ==.
replace payrate = mean if payrate ==.
assert payrate != .

drop mean* status _m

*for each patient, get total cost using only 1) visit total costs; or 2) visit total costs + visit travel costs; or 3) visit total costs + visit travel costs + pays
sort epiid visitdate

*recode costs to 0 if missing
replace visit_travel_ = 0 if visit_travel_==.
replace visit_tot_cost = 0 if visit_tot_cost==.
recast double visit_travel_cost
recast double visit_tot_cost
recast double payrate

set type double

gen vtc_tr = visit_travel_ + visit_tot_cost

*subtract transportation pays to workers to avoid double counting
gen vtc_tr_pay = vtc_tr + payrate

assert vtc_tr_pay==visit_travel_cost+ visit_tot_cost+ payrate

sum visit_tot_cost vtc_tr vtc_tr_pay
gen diff1 = vtc_tr-visit_tot_cost
gen diff2 = vtc_tr_pay-vtc_tr
assert diff1 >=0 & diff2>=0

*get episode-visit level total costs across pay disciplines
sort epiid visitdate

replace paydisc = "SN" if paydisc=="RN" | paydisc=="LPN"
collapse (sum) vtc_tr_pay visit_travel_cost visit_tot_cost payrate, by(epiid visitdate paydisc)
rename paydisc discipline

tempfile resource_index
save `resource_index'

*-----------------------------
* adapted from crepilvl_rehosp_smpl.do
*-----------------------------
*construct effort measures using the visit-level data
use `HHeffort_visit', clear

*drop outliers in terms of visit length < p1 or > p99
drop if lov==0
replace lov = lov *60
sum lov , de
loc p1 = `r(p1)'
loc p99 = `r(p99)'
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
capture drop visit_travel_cost visit_tot_cost payrate
merge m:1 epiid visitdate discipline using `resource_index', keep(1 3) nogen

*total # visits during episode , visit length
gen tnv = 1
gen tnvsn = discipline=="SN"
gen lovsn = lov if discipline=="SN"

*during the entire episode
preserve
collapse (sum) tnv tnvsn vtc_tr_pay visit_travel_cost visit_tot_cost payrate (mean) lov lovsn, by(epiid epilength)
assert vtc_tr_pay >= (visit_travel_cost+ visit_tot_cost+ payrate-0.00001) & vtc_tr_pay <= (visit_travel_cost+ visit_tot_cost+ payrate+0.00001)

tempfile effort1
save `effort1'
restore

*by whether the episode is first week or the rest
preserve
collapse (sum) tnv tnvsn vtc_tr_pay visit_travel_cost visit_tot_cost payrate (mean) lov lovsn, by(epiid firstwk)
foreach v of varlist tnv tnvsn vtc_tr_pay visit_travel_cost visit_tot_cost payrate lov lovsn {
  renam `v' `v'_1stwk
}
reshape wide *_1stwk, i(epiid) j(firstwk)
tempfile effort2
save `effort2'
restore

*--------------------------
*restrict to episodes without a visit on the day of readmission, who had a hospital stay before HH, who didn't have AIDS or blooad anemia, < age 65
use `HHeffort_visit', clear

keep if facility=="Hosp"

*drop AIDS & blood anemia
drop if ynel16==1 | ynel25==1

drop if age < 65

*exclude patients referred hospitals not eligible for HRRP penalties
*exclude patients referred by Maryland hospitals: first 2 characters of CCN is 21, 80 (https://www.cms.gov/medicare/provider-enrollment-and-certification/surveycertificationgeninfo/downloads/survey-and-cert-letter-16-09.pdf)
gen first2 = substr(provider_hosp,1,2)
drop if first2=="21" | first2=="80"

*restric to short-term hospitals; i.e. not Critical Access Hospitals
gen last4 = substr(provider_hosp,3,4)
destring last4, replace
keep if last4 >= 1 & last4 <= 879
capture drop first2 last4

*5-year age bins
egen age5yr = cut(age), at(65,70,75,80,85,90,95)
replace age5yr = 95 if age > 94
assert age5yr!=.

*-----------
*create episode-level sample from visit-level sample
loc riskhosp riskhosp_fall riskhosp_manyhos riskhosp_mental riskhosp_ge5 riskhosp_oth
loc priorcond priorcond_dis priorcond_impd priorcond_cath priorcond_pain priorcond_mem priorcond_inc
loc hrfactor hrfactor_alco hrfactor_drug hrfactor_smoke hrfactor_obese
loc overallst overallst_vbad overallst_bad overallst_tem
loc ins ma_visit ma_epi dual
loc ages age5yr
loc demog `ages' female white noassist livealone `ins'
loc comorbid ynch* `overallst' `hrfactor' `priorcond'

loc chars `comorbid' `riskhosp' `demog'
loc vars epiid provider_hosp socdate2 offid_nu clientid fy inpat_dcd ami hf pn copd stroke cardioresp hashosp* days2hosp firsthospdate age
keep `chars' `vars'
duplicates drop

* create a severity measure: sum of risk of hospitalization categories at baseline
capture drop riskhosp
egen riskhosp = rowtotal(riskhosp_* hrfactor_* priorcond_*)
tab riskhosp

*create indicator for whether episode had 4 HRRP conditions
gen hrrpcond = ami==1 | hf==1 | pn==1

*Are there patients having multiple conditions? ~1000 pats have 2-4 conditions
egen ss = rowtotal(ami hf pn copd stroke)
tab ss
drop ss

*Indicator for whether the HH episode started within 1 day from hospital discharge
gen time2hh = socdate2 - inpat_dcdate_e
drop if time2hh < 0 | time2hh > 30
tab time2hh
*all the episodes started HH within 14 days from hospital discharge
gen startHH_1day = time2hh <=1 if time2hh!=.

*-------------------
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

*drop if patient had 0 nurse visits during the episode
drop if tnvsn==0

*drop episodes if the total summary cost ==0
drop if vtc_tr_pay==0

gen ma = ma_visit==1 | ma_epi==1
gen tm = 1-ma

egen hrrpcond_count = rowtotal(ami hf pn)
tab hrrpcond_count
drop if hrrpcond_count > 1

*------------------
*merge with data on other covariates
rename socdate visitdate_e
merge m:1 offid_nu visitdate_e using allepi, keep(1 3) nogen
merge m:1 offid_nu visitdate_e using nw_active_worker, keep(1 3) nogen

rename provider_hosp prvdr_num
destring prvdr_num, replace
merge m:1 prvdr_num fy using hospchars, keep(3) nogen

*-------------
*merge by patient's referring hospital and office, the penalty pressure data
merge m:1 offid_nu prvdr_num using HRRPpnlty_pressure_hj_2012-v2, keep(3) nogen keepusing(pnltprs* penrate* shref_hj)

*create a single penalty pressure variable containing pressure for each condition
gen pnltprs_c = .
gen pnltprs_hosp_c = .
foreach d in "ami" "hf" "pn" {
  replace pnltprs_c = pnltprs_`d' if `d'==1
  replace pnltprs_hosp_c = pnltprs_hosp_`d' if `d'==1
}
replace pnltprs_c = 0 if hrrpcond==0
replace pnltprs_hosp_c = 0 if hrrpcond==0
assert pnltprs_c!=.

*2 obs have pnltprs_hosp_c = .
drop if pnltprs_hosp_c==.

loc uami "AMI"
loc uhf "HF"
loc upn "PN"
foreach d in "ami" "hf" "pn" {
  capture drop pnltprs_c_X_`d'
  gen pnltprs_c_X_`d' = pnltprs_c *`d'
  gen pnltprs_hosp_c_X_`d' = pnltprs_hosp_c *`d'

  lab var pnltprs_c_X_`d' "Condition-specific penalty salience X `u`d''"
  lab var pnltprs_hosp_c_X_`d' "Alternative penalty salience (HH firm's share of hospital patients) X `u`d''"
  lab var `d' "Indicator for `u`d''"
}

*--------------------
* create log-transformed outcome variables & label them
foreach v of varlist vtc_tr_pay* visit_travel_cost* visit_tot_cost* payrate* epilength* lov* {
  gen ln`v' = ln(`v'+1)
}

loc l_lnlov "Ln Visit length (min)"
loc l_lnlovsn "Ln Nurse visit length (min)"
loc l_freq_tnv "Frequency of visits"
loc l_freq_tnvsn "Frequency of nurse visits"
loc l_lnvtc_tr_pay "Ln Total cost index ($)"
loc l_lnvisit_tot_cost "Ln Visit cost ($)"
loc l_lnpayrate "Ln Personnel cost ($)"
loc l_lnvisit_travel_cost "Ln Travel cost ($)"
loc l_hashosp30 "30-day readmission"

lab var startHH_1day "Prob. start HH within 1 day from hosp discharge"

foreach v in "lnlov" "lnlovsn" "freq_tnv" "freq_tnvsn" "lnvtc_tr_pay" "lnvisit_tot_cost" "lnpayrate" "lnvisit_travel_cost" "hashosp30" {
  lab var `v' "`l_`v''"
  lab var `v'_1stwk1 "`l_`v'' in the first week"
  lab var `v'_1stwk0 "`l_`v'' beyond the first week"
}

loc outcome lnlov lnlov_1stwk1 lnlovsn lnlovsn_1stwk1 freq_tnv freq_tnv_1stwk1 freq_tnvsn freq_tnvsn_1stwk1 startHH_1day lnvtc_tr_pay lnvtc_tr_pay_1stwk1 lnvisit_tot_cost lnvisit_tot_cost_1stwk1 lnpayrate lnpayrate_1stwk1 lnvisit_travel_cost lnvisit_travel_cost_1stwk1 hashosp30 hashosp30_1stwk1
des `outcome'

*create cost per day
gen nl = epilength - 7

loc cc vtc_tr_pay visit_tot_cost payrate visit_travel_cost
foreach v of varlist `cc' {
   gen `v'_pd = `v'/epilength

   loc c2 `v'_1stwk1
   gen `c2'_pd = `c2'/7 if epilength >=7
   replace `c2'_pd = `c2'/epilength if epilength <7
   assert `c2'_pd !=. if `c2'!=.

   loc c3 `v'_1stwk0
   gen `c3'_pd = `c3'/nl if epilength >7
   replace `c3'_pd = 0 if epilength <=7
   assert `c3'_pd !=. if `c3'!=.
}

loc cc2 vtc_tr_pay vtc_tr_pay_1stwk1 vtc_tr_pay_1stwk0 visit_tot_cost visit_tot_cost_1stwk1 visit_tot_cost_1stwk0 payrate payrate_1stwk1 payrate_1stwk0 visit_travel_cost visit_travel_cost_1stwk1 visit_travel_cost_1stwk0
foreach c of varlist `cc2' {
 loc v `c'_pd
 gen ln`v' = ln(`v'+1)
}

foreach v in "lnvtc_tr_pay" "lnvisit_tot_cost" "lnpayrate" "lnvisit_travel_cost" {
  lab var `v'_pd "`l_`v'' per day"
  lab var `v'_1stwk1_pd "`l_`v'' per day in the first week"
  lab var `v'_1stwk0_pd "`l_`v'' per day beyond the first week"
}

compress
save epilvl_rehosp_smpl2012, replace
