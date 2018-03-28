* create a measure of resources spent on each patient--i.e. a summary index of care intensity based on spending
*get weekly number of visits & salary for each worker

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults

cd `path'

*aggregate the visit-level data to weekly # visits data for each worker-office-worker-week
use epi_visit, clear

*for visit discipline, jobcode is more detailed (distinguishing LPN from RN) but have to map OTA and PTA to OT & PT, respectively
* paydisc in pay data: HHA, LPN, MSW, OT,PT, RN, ST
gen paydisc = jobcode
replace paydisc = "OT" if jobcode=="OTA"
replace paydisc = "PT" if jobcode=="PTA"
replace paydisc = "RN" if jobcode=="SN"
tab paydisc

*drop duplicate visits
keep payrollno monday offid_nu paydisc visitdate lov visittime epiid offid_nu visit_travel_cost visit_tot_cost
duplicates drop

gen i = 1
collapse (sum) nv = i lov visit_tot_cost visit_travel_cost, by(payrollno monday offid_nu paydisc)

compress
save weekly_nv, replace

tempfile weekly_nv
save `weekly_nv'

*----------------------------------------
/* *get weekly pay for each worker
use pay_bywwod, clear
tab paydisc

*start date is already the monday date of each week
rename startdate monday
egen totpay = rowtotal(pay_BONUS pay_OVERTIME pay_PTO pay_REGULAR pay_SALARY)
rename pay_TRANSPORT pay_transport
keep payrollno offid_nu paydisc monday totpay pay_transport

merge 1:1 payrollno offid_nu paydisc monday using weekly_nv, keep(3) nogen

lab var nv "Total # visits for worker-office-week-discipline"
lab var lov "Sum lengths of visits for worker-office-week-discipline"
lab var visit_tot_cost "Sum visit costs for worker-office-week-discipline"
lab var visit_travel_ "Sum visit travel costs for worker-office-week-discipline"
lab var totpay "Total pay (excl. transport) for worker-office-week-discipline"
lab var pay_transport "Total pay for transportation for worker-office-week-discipline"

foreach v of varlist pay_transport totpay {
  sum `v'
  qui count if `v'<=0 & `v'!=.
  di "`v' has `r(N)' obs with 0 or (-) values"
  qui replace `v'= 0 if `v'<=0 & `v'!=.
}

*create (total pay / # visits ) per week for each worker-office-discipline
capture drop hrcost_pv
gen hrcost_pv = totpay / nv
lab var hrcost_pv "(Total pay (excl. transport cost) / # visits ) per worker-office-week-discipline"

tempfile hrcost
save `hrcost' */

*----------------------------------------
use payrate, clear
collapse (mean) payrate, by(payrollno monday)

sort payrollno monday
bys payrollno: replace payrate=payrate[_n-1] if payrate >=.

gsort payrollno -monday
bys payrollno: replace payrate=payrate[_n-1] if payrate >=.

bys payrollno: egen x = mean(payrate)
drop if x==.
drop x

tempfile payrate
save `payrate'
/*
merge 1:m payrollno monday using weekly_nv, keep(3) nogen
collapse (sum) nv (mean) payrate, by(payrollno monday)
gen hrcost_pv = payrate / nv */
*----------------------------------------
*use pay rate data for each worker-week and merge it with the visit-level data
use HHeffort_visit, clear

keep epiid visitdate payrollno monday visit_tot_cost visit_travel_cost status jobcode offid_nu
duplicates drop

merge m:1 payrollno monday using `payrate', keep(1 3)

gsort payrollno -monday -payrate
bys payrollno: replace payrate = payrate[_n-1] if payrate >=.

tempfile tmp
save `tmp'

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

gen double vtc_tr = visit_travel_ + visit_tot_cost

*subtract transportation pays to workers to avoid double counting
gen double vtc_tr_pay = vtc_tr + payrate

sum visit_tot_cost vtc_tr vtc_tr_pay
gen diff1 = vtc_tr-visit_tot_cost
gen diff2 = vtc_tr_pay-vtc_tr
assert diff1 >=0 & diff2>=0

collapse (sum) vtc = visit_tot_cost vtc_tr vtc_tr_pay, by(epiid paydisc)

foreach v of varlist vtc vtc_tr vtc_tr_pay {
  bys epiid: egen epilvl_`v' = sum(`v')
}

drop if paydisc==""
reshape wide vtc vtc_tr vtc_tr_pay, i(epiid epilvl*) j(paydisc) str

assert epilvl_vtc <= epilvl_vtc_tr
assert epilvl_vtc_tr <= epilvl_vtc_tr_pay

compress
save resource_index, replace

/* *use visit-level costs and merge with pay data
use HHeffort_visit, clear

gen paydisc = jobcode
replace paydisc = "OT" if jobcode=="OTA"
replace paydisc = "PT" if jobcode=="PTA"
replace paydisc = "RN" if jobcode=="SN"
tab paydisc

list epiid admissionclie payrollno offid_nu paydisc monday visit_tot_cost visit_travel_cost in 1/10
*visit_tot_cost doesn't seem to include visit_travel_cost

*merge with pay data by payrollno offid_nu paydisc monday
keep epiid admissionclie visitdate payrollno offid_nu paydisc monday visit_tot_cost visit_travel_cost

drop if paydisc==""

*since I don't have pay data available for RD, separately save the visits by RD
preserve
keep if paydisc=="RD"
tempfile rd
save `rd'
restore

drop if paydisc=="RD"
merge m:1 payrollno offid_nu paydisc monday using `hrcost', keepusing(hrcost_pv nv lov totpay pay_transport) keep(1 3)
*10% visits get unmatched and have _m=1

tempfile tmp
save `tmp'

*for unmatched visits, use the average of the cost for that worker-office-discipline from other weeks
use `tmp', clear
keep if _m==3
collapse (mean) hrcost_pv totpay pay_transport nv lov, by(payrollno offid_nu paydisc)
tempfile avg_hrcost_pv
save `avg_hrcost_pv'

use `tmp', clear
keep if _m==1
drop _m
merge m:1 payrollno offid_nu paydisc using `avg_hrcost_pv', keep(1 3)

preserve
keep if _m==3
drop _m
tempfile matched2
save `matched2'
restore

drop if _m==3
drop _m
tempfile unmatched2
save `unmatched2'

*for still unmatched visits, use the average of the cost for that office-discipline-week
use `tmp', clear
keep if _m==3
collapse (mean) hrcost_pv totpay pay_transport nv lov, by(offid_nu paydisc monday)
tempfile avg_hrcost_pv2
save `avg_hrcost_pv2'

use `unmatched2', clear
merge m:1 offid_nu paydisc monday using `avg_hrcost_pv2', keep(1 3)

preserve
keep if _m==3
drop _m
tempfile matched3
save `matched3'
restore

drop if _m==3
drop _m
tempfile unmatched3
save `unmatched3'

*for still unmatched visits, use the average of the cost for that office-discipline
use `tmp', clear
keep if _m==3
collapse (mean) hrcost_pv totpay pay_transport nv lov, by(offid_nu paydisc)
tempfile avg_hrcost_pv3
save `avg_hrcost_pv3'

use `unmatched3', clear
merge m:1 offid_nu paydisc using `avg_hrcost_pv3', keep(1 3)

preserve
keep if _m==3
drop _m
tempfile matched4
save `matched4'
restore

drop if _m==3
drop _m
tempfile unmatched4
save `unmatched4'

*for still unmatched visits, use the average of the cost for that discipline
use `tmp', clear
keep if _m==3
collapse (mean) hrcost_pv totpay pay_transport nv lov, by(paydisc)
tempfile avg_hrcost_pv4
save `avg_hrcost_pv4'

use `unmatched4', clear
merge m:1 paydisc using `avg_hrcost_pv4', keep(1 3)
assert _m==3
drop _m
tempfile matched5
save `matched5'

use `tmp', clear
keep if _m==3
forval x = 2/5 {
    append using `matched`x''
}
tempfile tmp2
save `tmp2'

*for each patient, get total cost using only 1) visit total costs; or 2) visit total costs + visit travel costs; or 3) visit total costs + visit travel costs + pays
use  `tmp2', clear
sort epiid visitdate

gen vtc_tr = visit_travel_ + visit_tot_cost

*subtract transportation pays to workers to avoid double counting
gen vtc_tr_pay = vtc_tr + hrcost_pv

collapse (sum) vtc = visit_tot_cost vtc_tr vtc_tr_pay, by(epiid paydisc)

foreach v of varlist vtc vtc_tr vtc_tr_pay {
  bys epiid: egen epilvl_`v' = sum(`v')
}

reshape wide vtc vtc_tr vtc_tr_pay, i(epiid epilvl*) j(paydisc) str

assert epilvl_vtc <= epilvl_vtc_tr
assert epilvl_vtc_tr <= epilvl_vtc_tr_pay

compress
save resource_index, replace */
