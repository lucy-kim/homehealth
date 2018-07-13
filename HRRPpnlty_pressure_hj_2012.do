*compute penalty pressure: use 2012 data, product of share of the office j's patients that come from hosp h and h's penalty rate

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
*get 2012 penalty rate to calculate penalty pressure for each office j - hospital h pair later
use hrrp_penalty, clear
keep totpenalty2012 prvdr_num penalty2012_*
duplicates drop

*rename fyear fy
destring prvdr_num, replace

foreach v of varlist *penalty* {
  replace `v' = 0 if `v'==.
}

tempfile hrrp_penalty
save `hrrp_penalty'

*--------------
* merge the hospital's referral share to office data
use `shref_hj2012', clear

*merge with 2012 HRRP penalty data
merge m:1 prvdr_num using `hrrp_penalty', keep(3) nogen
*2876 obs reduced to 2147 obs after matching

*compute penalty pressure: use 2012 data, product of share of the office j's patients that come from hosp h and h's penalty rate
sort offid_nu prvdr_num

gen pnltprs = shref_hj * totpenalty2012

*create the z-score penalty pressure
egen z_pnltprs = std(pnltprs)
sum z_pnltprs, de

rename penalty2012_chf penalty2012_hf
rename penalty2012_pneum penalty2012_pn

foreach d in "ami" "hf" "pn" {
  capture drop pnltprs_`d'
  gen pnltprs_`d' = shref_hj * penalty2012_`d'

  capture drop z_pnltprs_`d'
  egen z_pnltprs_`d' = std(pnltprs_`d')
  sum z_pnltprs_`d'
}

*--------------
*use actual penatly rate in 2013 & reconstruct the penalty pressure
preserve
use hrrp_penalty, clear
keep if fy==2013
keep prvdr_num penalty
duplicates drop
destring prvdr_num, replace
tempfile penalty13
save `penalty13'
restore

merge m:1 prvdr_num using `penalty13', keep(1 3) nogen
rename penalty penalty13

*create the z-score penalty pressure
gen pnltprs13 = shref_hj * penalty13
egen z_pnltprs13 = std(pnltprs13)
sum z_pnltprs13, de

*create 2012 penalty rate for [AMI & HF] X share of office's volume from the hospital
gen penalty2012_heart = penalty2012_ami + penalty2012_hf
gen pnltprs_heart = penalty2012_heart * shref_hj

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

tempfile sharetoBayada
save `sharetoBayada'

*--------------
*add the predicted probability of penalty
use `tmp', clear
merge m:1 prvdr_num using pred_pprob, keep(3) nogen

merge 1:1 prvdr_num offid_nu using `sharetoBayada', keep(3) nogen keepusing(sharetoBayada)

foreach d in "ami" "hf" "pn" {
  *penalty salience = 2012 predicted probability of penalty X office's referral share from each hospital
  capture drop pnltprs_pred_`d'
  gen pnltprs_pred_`d' = shref_hj * pnltprob_`d'

  *penalty salience = 2012 predicted probability of penalty X office's referral share from each hospital X hospital's discharge share that went to Bayada
  capture drop pnltprs_pred_hosp_`d'
  gen pnltprs_pred_hosp_`d' = pnltprs_pred_`d' * sharetoBayada

  *penalty salience = 2012 penalty rate X office's referral share from each hospital X hospital's discharge share that went to Bayada
  capture drop pnltprs_hosp_`d'
  gen pnltprs_hosp_`d' = pnltprs_`d' * sharetoBayada
}

compress
save HRRPpnlty_pressure_hj_2012, replace




*--------------

*plot the penalty for 2012 vs actual penalty for 2013
use hrrp_penalty, clear
keep if fy==2013
keep penalty prvdr_num
destring prvdr_num, replace
merge 1:m prvdr_num using HRRPpnlty_pressure_hj_2012, keep(2 3) nogen

scatter penalty totpenalty2012 if totpenalty2012 <=1 & & pnltprs < 0.1, yti(Actual penalty rate in 2013) xti(Penalty rate in 2012) ti(Actual penalty rate in 2013 vs Penalty rate in 2012)
graph export `gph'/pnltr_1213.eps, replace

binscatter penalty totpenalty2012 if totpenalty2012<=1, yti(Actual penalty rate in 2013) xti(Penalty rate in 2012) ti(Actual penalty rate in 2013 vs Penalty rate in 2012)
graph export `gph'/pnltr_1213.eps, replace


*plot the penalty rate for 2012 vs penalty pressure (2012 penalty rate X share of referrals)
binscatter pnltprs totpenalty2012 if totpenalty2012 <=1, yti("2012 penalty rate X 2012 Share of referrals from the hospital", size(small)) xti(Penalty rate in 2012) ti(Penalty pressure vs Penalty rate in 2012)
graph export `gph'/pnltr12_pnltprs.eps, replace


*--------------
*plot the distribution of penalty pressure across office-hospitals
use HRRPpnlty_pressure_hj_2012, clear

tab pnltprs
sum pnltprs, de
*4 largest obs= 1.5, .36, .27, .25, and the next obs are .06
*drop if > .1

drop if pnltprs > 0.1
sum pnltprs, de
twoway histogram pnltprs, color(*.5) frac width(0.001) ti("Penalty Pressure (%)") xti("2012 Office's share of referrals from hospital X Hospital penalty rate (%)", size(medium))
*|| kdensity pnltprs
* ||, by(offid_nu)
graph export `gph'/density_pnltprs.pdf, replace


twoway histogram z_pnltprs, color(*.5) frac ti("Z-Score of Penalty Pressure (%)") xti("2012 Office's share of referrals from hospital X Hospital penalty rate (%)", size(medium))
graph export `gph'/density_z_pnltprs.pdf, replace

/*reg z_pnltprs shref_hj totpenalty2012, nocons vce(cluster offid_nu)
set matsize 11000
reg z_pnltprs i.prvdr_num, vce(cluster offid_nu)
predict resid, residual*/
