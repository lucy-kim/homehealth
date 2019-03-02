*compute penalty pressure: use 2012 data, product of share of the office j's patients that come from hosp h and h's penalty rate

// *input data
// - referralhosp_mcrID.dta
// - referral.dta
// - officeID_foradmitID.dta
// - hrrp_penalty.xlsx
// - CMShospdisch_tohh.dta
*output data
// - HRRPpnlty-pressure-hj-2012-v2.dta

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults

cd `path'

*--------------------------
* compute the number of patients referred to Bayada office from each hospital for each FY
use referralhosp_mcrID, clear

* create fiscal year
gen fy = .
forval y=2012/2016 {
  loc yl1 = `y'-1
  replace fy = `y' if socdate >= mdy(7,1,`yl1') & socdate <= mdy(6,30,`y')
}
assert fy!=.

loc tunit fy
gen i = 1
collapse (sum) ref_hj = i, by(prvdr_num offid_nu `tunit')

destring prvdr_num, replace

sort offid_nu prvdr_ fy

tempfile ref_hj
save `ref_hj'

*--------------
*create HH office-hospital pair level data counting all the hospitals that have referred any patients during 2013-2015
use `ref_hj', clear
keep offid_nu prvdr_num
duplicates drop

*restrict to 2012
gen fy = 2012
sort offid_nu prvdr_num
merge 1:1 offid_nu prvdr_num fy using `ref_hj'
*_m=2 obs are unmatched b/c they are for > fy 2012 - not interested
assert fy > 2012 if _merge==2
drop if _merge==2

*_m=1 obs are unmatched b/c these hospitals only referred to the office after fy 2012  -> recode their ref_hj = 0
replace ref_hj = 0 if _m==1 & ref_hj==.
drop _merge

tempfile ref_hj2
save `ref_hj2'

*--------------
* compute share of the office j's patients that come from hosp h
*count the number of all the referrals to each office during each FY

*use patient-level data for referrals from all facilities
use referral, clear
tab type, sort

*merge with client data to get the address of office & ZIP code of patient
duplicates tag clientid socdate, gen(dup)
tab dup
*116 obs: regard these as separate referrals
drop dup

merge m:1 clientid socdate_e using officeID_foradmitID, keep(1 3) nogen
*99 admissions unmatched _m=1
drop epiid
count if offid_nu==.
*99 obs
drop if offid_nu==.

drop if type=="HOSPITAL"

*for hospital-referred patients, use a separate cleaned data with medicare hospital ID
*get patient-level data for referrals from hospitals only
append using referralhosp_mcrID

keep offid_nu socdate

*create FY
gen fy = .
forval y=2012/2016 {
  loc yl1 = `y'-1
  replace fy = `y' if socdate >= mdy(7,1,`yl1') & socdate <= mdy(6,30,`y')
}
assert fy!=.

*aggregate the number of referrals for each office-FY
gen i = 1
collapse (sum) tref_jt = i, by(offid_nu fy)

tempfile tref_jt
save `tref_jt'

*--------------
*create office's share of referrals from each hospital in 2012
use `tref_jt', clear
keep if fy==2012
merge 1:m fy offid_nu using `ref_hj2', keep(3) nogen
*drop 32 offices that had zero referrals in 2012 - can't compute penalty pressure for them - 75 offices remain in the sample

gen shref_hj = ref_hj / tref_jt

sort offid_nu prv

tempfile shref_hj2012
save `shref_hj2012'

*--------------
*Bring in raw 2012 penalty rate data and save as stata file;
*-------------------------------------------------------------------------------
import excel using hrrp_penalty.xlsx, firstrow sheet("ami") clear
ren cpnltrevprop penrate_ami
recode penrate_ami (mis=0)
keep hospital penrate_ami


tempfile hrrp_penalty
save `hrrp_penalty', replace

import excel using hrrp_penalty.xlsx, firstrow sheet("chf") clear
ren cpnltrevprop penrate_hf
recode penrate_hf (mis=0)
keep hospital penrate_hf

tempfile hrrp_hf
save `hrrp_hf', replace

import excel using hrrp_penalty.xlsx, firstrow sheet("pneum") clear
ren cpnltrevprop penrate_pn
recode penrate_pn (mis=0)
keep hospital penrate_pn

tempfile hrrp_pn
save `hrrp_pn', replace

use `hrrp_penalty', clear
merge 1:1 hospital using `hrrp_hf'
drop _m
merge 1:1 hospital using `hrrp_pn'
drop _m
ren hospital prvdr_num
destring prvdr_num, force replace
sort prvdr_num

recode penrate_ami penrate_hf penrate_pn (mis=0)

save `hrrp_penalty', replace

*--------------
* merge the hospital's referral share to office data
use `shref_hj2012', clear

*merge with 2012 HRRP penalty data
merge m:1 prvdr_num using `hrrp_penalty', keep(3) nogen
*2876 obs reduced to 2147 obs after matching

*compute penalty pressure: use 2012 data, product of share of the office j's patients that come from hosp h and h's penalty rate
sort offid_nu prvdr_num

foreach d in "ami" "hf" "pn" {
  capture drop pnltprs_`d'
  gen pnltprs_`d' = shref_hj * penrate_`d'
}

tempfile tmp
save `tmp'

*--------------
*add the share of the hospital’s patients that the HH provider contributes

use CMShospdisch_tohh, clear
keep if monday >= mdy(1,1,2012) & monday <= mdy(6,30,2012)
rename provider_hosp prvdr_num
gen x= real(prvdr)
drop if x==.
destring prvdr, replace
collapse (sum) hdis2hh, by(prvdr_num)
tempfile hdis2hh
save `hdis2hh'

*create an office-hospital level # hospitals' discharges to home health that went to Bayada during 2012
use `ref_hj2', clear
merge m:1 prvdr_num using `hdis2hh', keep(3) nogen

gen sharetoBayada = ref_hj/hdis2hh
count if sharetoBayada>1 & sharetoBayada!=.
*25 office-hospital pair obs
*recode to 1 if the share exceeds 1
replace sharetoBayada = 1 if sharetoBayada > 1 & sharetoBayada!=.
sum sharetoBayada, de
sum sharetoBayada if sharetoBayada >0 , de

tempfile sharetoBayada
save `sharetoBayada'

*--------------
*create the final hospital-office level data by merging data on 2 penalty salience measures: 1) referral share X 2012 condition-specific penalty rate; 2) measure 1 X hospital's patient share going to each Bayada office

use `tmp', clear

merge 1:1 prvdr_num offid_nu using `sharetoBayada', keep(3) nogen keepusing(sharetoBayada)

*penalty salience = 2012 penalty rate X office's referral share from each hospital X hospital's discharge share that went to Bayada
foreach d in "ami" "hf" "pn" {
  capture drop pnltprs_hosp_`d'
  gen pnltprs_hosp_`d' = pnltprs_`d' * sharetoBayada
}

compress
save HRRPpnlty_pressure_hj_2012-v2, replace
