*create admission level data on patients referred by hospitals only and restrict to patients whose discharge/hospitalization dates are not right truncated due to the sample period limitation

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
