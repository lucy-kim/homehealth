*create data containing additional covariates: 1) total number of episodes going on, # active workers in the office on each day; 2) characteristics of the referring hospital

loc path /home/hcmg/kunhee/hrrp-home/data/
cd `path'

*1) get total number of episodes going on, # active workers in the office on each day
use epi_visit, clear
keep offid_nu visitdate epiid payrollno
duplicates drop
keep if visitdate <= mdy(7,1,2015)

preserve
drop payrollno
duplicates drop
assert epiid!=.
gen allepi = 1
collapse (sum) allepi, by(offid_nu visitdate)
*subtract 1 to not count the episode that starts on that day
replace allepi = allepi - 1
gen lnallepi = ln(allepi)

compress
save allepi, replace
restore

preserve
drop epiid
duplicates drop
assert payrollno!=""
gen nw_active_worker = 1
collapse (sum) nw_active_worker, by(offid_nu visitdate)
gen lnnw_active_worker = ln(nw_active_worker)
compress
save nw_active_worker, replace
restore

*------------------

*get characteristics of the referring hospital
use hosp_chars_cr, clear
keep if fy >=2012 & fy <= 2015
keep provid fy vi_hha teaching urban own_* size beds
rename provid prvdr_num
duplicates drop
compress
save hospchars, replace
