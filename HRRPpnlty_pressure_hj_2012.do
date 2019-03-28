*compute penalty salience: use 2012 data, product of share of the office j's patients that come from hosp h and h's penalty rate

loc path /home/hcmg/kunhee/hrrp-home/data/
cd `path'

* merge the office's hospital referral share data with hospital's HRRP penalty data
use referral_share2012, clear

*merge with 2012 HRRP penalty data
merge m:1 prvdr_num using hrrp_penalty, keep(3) nogen
*2876 obs reduced to 2147 obs after matching

*compute penalty pressure: use 2012 data, product of share of the office j's patients that come from hosp h and h's penalty rate
sort offid_nu prvdr_num

foreach d in "ami" "hf" "pn" {
  capture drop pnltprs_`d'
  gen pnltprs_`d' = shref_hj * penrate_`d'
}

tempfile pnltprs
save `pnltprs'

*--------------
*Create an alternative measure of penalty salience, by multiplying the original penalty salience with the share of the hospital’s patients that the HH provider contributes

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
use referral_share2012, clear
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

use `pnltprs', clear

merge 1:1 prvdr_num offid_nu using `sharetoBayada', keep(3) nogen keepusing(sharetoBayada)

*penalty salience = 2012 penalty rate X office's referral share from each hospital X hospital's discharge share that went to Bayada
foreach d in "ami" "hf" "pn" {
  capture drop pnltprs_hosp_`d'
  gen pnltprs_hosp_`d' = pnltprs_`d' * sharetoBayada
}

compress
save HRRPpnlty_pressure_hj_2012, replace
