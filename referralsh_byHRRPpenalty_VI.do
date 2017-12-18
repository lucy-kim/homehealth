* test if the referral share has declined because hospital facing high penalty shifting their referrals to in-house home health care provider

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults

cd `path'

* construct outcome = referral share for each office j from hospital h in FY t := # home health referral from h to office j / # home health referrals from h during t

*compute the total number of patients referred to home health from each hospital for each FY
use CMShospdisch_tohh, clear
rename provider_hosp prvdr_num

*construct the share of patients referred by each hospital for each FY
loc tunit fy

* create fiscal year
gen fy = .
forval y=2012/2016 {
  loc yl1 = `y'-1
  replace fy = `y' if monday >= mdy(10,1,`yl1') & monday <= mdy(9,30,`y')
}
assert fy!=.

*gen ym = ym(year(monday), month(monday))
collapse (sum) hdis2hh, by(prvdr_num `tunit')

tempfile totref_fromhosp
save `totref_fromhosp'

*--------------------------
* compute the number of patients referred to Bayada office from each hospital for each FY
use referralhosp_mcrID, clear

* create fiscal year
gen fy = .
forval y=2012/2016 {
  loc yl1 = `y'-1
  replace fy = `y' if socdate >= mdy(10,1,`yl1') & socdate <= mdy(9,30,`y')
}
assert fy!=.

gen i = 1
collapse (sum) hospref_to_office = i, by(prvdr_num offid_nu `tunit')

*merge with hospital-FY level total # referrals to home health to compute the share of pats to each Bayada office from hospital-FY
merge m:1 prvdr_num `tunit' using `totref_fromhosp'
keep if _m==3
drop _m

gen shref_jht = hospref_to_office/hdis2hh
sum shref_jht, de
*may have to drop the share is > 99th percentile (3.1)

destring prvdr_num, replace

tempfile shref_jht
save `shref_jht'

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
  replace fy = `y' if socdate >= mdy(10,1,`yl1') & socdate <= mdy(9,30,`y')
}
assert fy!=.

*aggregate the number of referrals for each office-FY
gen i = 1
collapse (sum) totref = i, by(offid_nu fy)

tempfile totref_oy
save `totref_oy'
*--------------
*get 2012 penalty rate to calculate penalty pressure for each office j - hospital h pair later
use hrrp_penalty, clear
keep totpenalty2012 prvdr_num
duplicates drop

*rename fyear fy
destring prvdr_num, replace

tempfile hrrp_penalty
save `hrrp_penalty'

*--------------
* merge the hospital's referral share to office data
use `shref_jht', clear

*merge with office's total # referrals during each FY
merge m:1 offid_nu fy using `totref_oy', keep(1 3) nogen

*construct office's share of referrals from each hospital during each FY
gen hospwgt = hospref_to_office/totref
sum hospwgt

*merge with 2012 HRRP penalty data
merge m:1 prvdr_num using `hrrp_penalty', keep(3) nogen
*1K obs have _m=1; 5K have _m=3

*merge with 2012 AHA data by hospital ID
*first extract only 2012 data from AHA data
preserve
use `path'/Hospital/AHA2005_2015/aha, clear
keep if year==2012
drop year
tempfile aha12
save `aha12'
restore

rename prvdr_num mcrnum
rename fy year
merge m:1 mcrnum using `aha12', keep(3) nogen
*0 obs have _m=1; 5246 have _m=3

rename mcrnum prvdr_num
rename year fy

*compute penalty pressure: use 2012 data, product of share of the office j's patients that come from hosp h and h's penalty rate
sort offid_nu prvdr_num fy
gen hospwgt2012 = hospwgt if fy==2012

bys offid_nu prvdr_num: replace hospwgt2012 = hospwgt2012[_n-1] if hospwgt2012 >= .
*2K/5K have missing values in shref_jh2012 b/c no referrals in 2012 for the pair

*recode hospwgt2012 = 0 if the hospital didn't refer any patients to the office during FY 2012
replace hospwgt2012 = 0 if hospwgt2012==.

gen pnltprssure = hospwgt2012 * totpenalty2012
*101 missing value b/c missing value in totpenalty2012

*create above median, 90th, 75th, and 67th percentiles of penalty pressure
_pctile pnltprssure, p(50 67 75 90)

loc init = 0
foreach x of numlist 50 90 75 67 {
  loc init = `init' + 1
  loc pctl = `r(r`init')'
  di "`x'th = `pctl'"

  gen penalty_gt_p`x' = pnltprssure > `pctl'

  replace penalty_gt_p`x' = . if pnltprssure==.
  lab var penalty_gt_p`x' "Penalty > `x'th pctile"
}
/*
50th = 0
90th = .0000926100910874
75th = .0003353855863679
67th = .0024553760886192
*/

tempfile tmp
save `tmp'

*--------------
*save separate file for penalty pressure for each office-hospital-FY
use `tmp', clear
keep offid_nu prvdr_num pnltprssure penalty_gt* totpenalty2012
duplicates drop

compress
save penalty_pressure_oh, replace
*--------------
*regression
use `tmp', clear

drop if pnltprssure==.

*create post period indicator = 1 for FY 2013-2015
gen post = fy > 2012

loc vi1 homehhos
loc vi2 homehsys
loc vi3 homehnet
loc vi4 homehven
loc vl1 Own
loc vl2 System
loc vl3 Network
loc vl4 Joint venture

*interaction terms for triple diff-in-diff
foreach x of numlist 50 90 75 67 {
  gen highpnlt_p`x'_X_post = penalty_gt_p`x'*post
  lab var highpnlt_p`x'_X_post "Penalty pressure > `x'th pctile X Post"

  forval k=1/4 {
    gen highpnlt_p`x'_X_vi`k' = penalty_gt_p`x'*`vi`k''
    lab var highpnlt_p`x'_X_vi`k' "Penalty pressure > `x'th pctile X Vertical integration"

    gen highpnlt_p`x'_X_vi`k'_X_post = penalty_gt_p`x'*post*`vi`k''
    lab var highpnlt_p`x'_X_vi`k'_X_post "Penalty pressure > `x'th pctile X Vertical integration X Post"
  }
  lab var penalty_gt_p`x' "Penalty pressure > `x'th pctile"
}

lab var post "FY > 2012"

forval k=1/4 {
  capture drop vi`k'_X_post
  gen vi`k'_X_post = post*`vi`k''
  lab var vi`k'_X_post "Vertical integration X Post"
}

xi i.fy

loc yv shref_jht
*
foreach x of numlist 50 90 75 67  {
  loc file refsh_p`x'
  capture erase `reg'/`file'.xls
  capture erase `reg'/`file'.txt
  capture erase `reg'/`file'.tex
  loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append"

  forval k=1/4 {
    loc sp penalty_gt_p`x' highpnlt_p`x'_X_vi`k' highpnlt_p`x'_X_post vi`k'_X_post highpnlt_p`x'_X_vi`k'_X_post _Ify*

    areg `yv' `sp' i.offid_nu, absorb(prvdr_num) vce(cluster offid_nu)

    sum `yv' if e(sample)
    loc mdv: display %9.2f `r(mean)'

    loc rsq: display %9.2f e(r2)
    loc ar2: display %9.2f `e(r2_a)'

    `out' ctitle("`vl`k'' HHA") keep(`sp') label addtext(Adjusted R-squared, `ar2', Mean dep. var., `mdv')
  }
}

*--------------------------
* test if the result on referral share is driven by a few hospitals vs being more widespread

* divide hospitals into 3 groups: non-penalized (reference group), among penalized, above & below median penalty
_pctile pnltprssure if pnltprssure > 0, p(50)
loc x 50
loc pctl = `r(r1)'
di `pctl'

gen pnlized_high = pnltprssure > `pctl'
gen pnlized_low = pnltprssure > 0 & pnltprssure <= `pctl'
tab pnlized_high
tab pnlized_low
assert pnltprssure==0 if pnlized_high==0 & pnlized_low==0

lab var pnlized_high "Penalty pressure > median | Penalized"
lab var pnlized_low "Penalty pressure <= median | Penalized"

*interaction terms for triple diff-in-diff
gen pnlized_high_X_post = pnlized_high*post
lab var pnlized_high_X_post "(Penalty pressure > median | Penalized) X Post"

gen pnlized_low_X_post = pnlized_low*post
lab var pnlized_low_X_post "(Penalty pressure <= median | Penalized) X Post"

forval k=1/4 {
  gen pnlized_high_X_vi`k' = pnlized_high*`vi`k''
  lab var pnlized_high_X_vi`k' "(Penalty pressure > median | Penalized) X Vertical integration"

  gen pnlized_low_X_vi`k' = pnlized_low*`vi`k''
  lab var pnlized_low_X_vi`k' "(Penalty pressure <= median | Penalized) X Vertical integration"

  gen pnlized_high_X_vi`k'_X_post = pnlized_high*`vi`k''*post
  lab var pnlized_high_X_vi`k'_X_post "(Penalty pressure > median | Penalized) X Vertical integration X Post"

  gen pnlized_low_X_vi`k'_X_post = pnlized_low*`vi`k''*post
  lab var pnlized_low_X_vi`k'_X_post "(Penalty pressure <= median | Penalized) X Vertical integration X Post"
}

loc file refsh_2treat
capture erase `reg'/`file'.xls
capture erase `reg'/`file'.txt
capture erase `reg'/`file'.tex
loc out "outreg2 using `reg'/`file'.xls, tex dec(3) append"

forval k=1/4 {
  loc sp pnlized_high pnlized_low pnlized_high_X_vi`k' pnlized_low_X_vi`k' pnlized_high_X_post pnlized_low_X_post vi`k'_X_post pnlized_high_X_vi`k'_X_post pnlized_low_X_vi`k'_X_post _Ify*

  areg `yv' `sp' i.offid_nu, absorb(prvdr_num) vce(cluster offid_nu)

  sum `yv' if e(sample)
  loc mdv: display %9.2f `r(mean)'

  loc rsq: display %9.2f e(r2)
  loc ar2: display %9.2f `e(r2_a)'

  `out' ctitle("`vl`k'' HHA") keep(`sp') label addtext(Adjusted R-squared, `ar2', Mean dep. var., `mdv')
}
