*link_referralhosp_penalty.do
* link the admission-level hospital referral source data with readmissions penalty data by the Medicare hospital ID

loc path /home/hcmg/kunhee/Labor/Bayada_data/
cd `path'

*first, import the raw hospital-level overall readmission penalty rate data into Stata
insheet using HRRP/readmissions-year-3.csv, comma names clear
gen str6 prvdr_num = string(providerid, "%06.0f")
drop providerid
count
*3383 hospitals in total

tempfile penalty
save `penalty'

*-----------------
* process the raw hospital-level condition-specific readmission penalty rate from Atul

foreach d in "ami" "chf" "pneum" {
  insheet using HRRP/hrrp_penalty_`d'.csv, comma names clear
  keep penaltyrate2012 v16
  drop if _n < 2
  rename penaltyrate2012 prvdr_num
  rename v16 penalty2012_`d'
  destring penalty2012_`d', replace
  compress
  tempfile penalty2012_`d'
  save `penalty2012_`d''
}

use `penalty2012_ami', clear
foreach d in "chf" "pneum" {
  merge 1:1 prvdr_num using `penalty2012_`d'', nogen
}
egen totpenalty2012 = rowtotal(penalty2012_*)

tempfile penalty2
save `penalty2'

*-----------------

use `penalty', clear

*merge with referral source data
merge 1:m prvdr_num using referral_mcrID
*2883 hosp have _m=1 -> 500 hospitals in the readmissions penalty data matched to my data

*_m=2 hospitals include those in Maryland (Medicare ID start with "21") that's excempt from the HRRP
gen st = substr(prvdr_num,1,2)
drop if _merge==2 & st=="21"
tab prvdr_num if _merge==2

* below i look up each individual hospital to see if any IPPS hospital that should be matched are not matched (except ones with <10 referred admissions)
*some hospitals have good reasons for not appearing in the penalty data: http://www.kff.org/medicare/issue-brief/aiming-for-fewer-hospital-u-turns-the-medicare-hospital-readmission-reduction-program/
* 390196 excluded from the CMS IPPS (https://www.cms.gov/Medicare/Medicare-Fee-for-Service-Payment/AcuteInpatientPPS/PPS_Exc_Cancer_Hospasp.html)
* 223032 rehab hosp (RH)
* 471300 critical access hosp (CAH)
* 343026 rehap hosp
* 471302 critical access hosp
* 471301 CAH
* 223027 RH
* 330154 excluded from the CMS IPPS
* 063030 RH
* 223034 RH
* 084002 psychiatric
* 313029 RH
* 062014 long-term care (LTC)
* 082000 LTC
* 032002 LTC
* 084003 psychiatric
* 062012 LTC
* 312022 LTC
* 392050 rehab
* 031301 CAH
* 103031 RH
* 222027 RH
* 062015 LTC
* 471306 CAH
* 471307 CAH
* 393054 RH
* 032004 LTC
* 062009 LTC
* 241361 CAH
* 030137 - short term acute hosp but I can't find it in any penalty data
* 222010 LTC
* 100271 excluded from the CMS IPPS
* 062013 LTC
* 220162 excluded from the CMS IPPS
* 121308 CAH
* 301305 CAH
* 062011 LTC
* 083300 children's
* 393303 children's
* 392039 LTC
* 344014 psychiatric
* 342012 LTC
* 393053 RH
* 312023 LTC
* 413025 RH
* 222043 LTC

* other hospitals have no good reasons
* 030100 is consolidated with 030010 which is matched
gen prvdr_num_new = prvdr_num
replace prvdr_num_new = "030010" if prvdr_num=="030100"
*340055 is consolidated with 340075 which appears in the penalty data
replace prvdr_num_new = "340075" if prvdr_num=="340055"

rename prvdr_num prvdr_num_orig
rename prvdr_num_new prvdr_num

keep if _merge==2 | _merge==3
keep prvdr_num_orig-prvdr_num
drop st _merge

*merge with readmissions penalty data
merge m:1 prvdr_num using `penalty', keep(3) nogen
*2883 hosp have _m=1 -> 500 hospitals in the readmissions penalty data matched to my data

*merge with condition-specific readmissions penalty data
merge m:1 prvdr_num using `penalty2', keep(1 3) nogen

*count how many hospitals in the Bayada data matched to the penalty data
preserve
keep prvdr_num
duplicates drop
count
restore

compress
save referralhosp_penalty, replace
