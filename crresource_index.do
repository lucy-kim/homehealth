* create a measure of resources spent on each patient--i.e. a summary index of care intensity based on spending
*get weekly number of visits & salary for each worker

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults

cd `path'

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

compress
save resource_index, replace
