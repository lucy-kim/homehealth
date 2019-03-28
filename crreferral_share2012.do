*Create hospital-office-FY level # referrals and share of any referrals from a hospital

loc path /home/hcmg/kunhee/hrrp-home/data/
cd `path'

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
merge 1:m fy offid_nu using `ref_hj2', keep(1 3) nogen

gen shref_hj = ref_hj / tref_jt

sort offid_nu prvdr_num

lab var ref_hj "# referrals for each office from each hospital"
lab var tref_jt "total # referrals for each office-FY"
lab var shref_hj "share of hospital referrals for each office-FY from each hospital"

compress
save referral_share2012, replace
