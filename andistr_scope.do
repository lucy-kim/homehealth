* andistr_scope.do
* analyze the distribution of # workers in different service disciplines (nursing, therapy, home aide) across offices
* create for each office-week, # workers in each discipline (by firm size)

loc path /home/hcmg/kunhee/Labor/Bayada_data/
cd `path'
loc gph /home/hcmg/kunhee/Labor/gph

*--------------
* # workers (visits / episodes) in different service disciplines for each office-week
*--------------
use visit_worker_chars, clear
*this dataset spans 1/1/2012 - 12/31/2015

duplicates tag payrollno visitd visittime, gen(dup)
tab dup
*5K (0.1%) obs are duplicates which differ by visit type (e.g. recert vs subsequent visit), service code, description variables
drop dup

tempfile tmp
save `tmp'

use `tmp', clear

*drop employee ID "-99P" since that might be just a wildcard ID
drop if payrollno=="-99P"

*tag unique worker for each office-week-discipline
sort offid_nu monday discipline payrollno visitdate
bys offid_nu monday discipline payrollno: gen i = _n==1

*tag visit for each office-week-discipline
gen j = 1

*tag unique episode for each office-week-discipline
sort offid_nu monday discipline epiid visitdate
bys offid_nu monday discipline epiid: gen k = _n==1

*tag unique episode for each office-week
sort offid_nu monday epiid visitdate
bys offid_nu monday epiid: gen l = _n==1
bys offid_nu monday: egen totnepi = sum(l)

collapse (sum) nw = i nv = j nepi = k (mean) totnepi, by(offid_nu monday discipline)

* create year
gen yr = year(monday)

*recode yr to 2012 if yr==2011
replace yr = 2012 if yr==2011

tempfile an
save `an'

*---
*create mean # episodes for each office-yr
use `an', clear
keep offid_nu monday yr totnepi
duplicates drop
collapse (mean) totnepi , by(offid_nu yr)
sum totnepi if yr==2012, de
tempfile totnepi
save `totnepi'

*---
use `an', clear

*get mean # workers for each office-year-discipline
collapse (mean) nw nv nepi, by(offid_nu yr discipline)

tempfile unbal
save `unbal'

*to get a balanced panel at least at the discipline level, create 6 category
use `unbal', clear
keep discipline
duplicates drop
egen gp = group(discipline)
tempfile disc
save `disc'

use `unbal', clear
keep offid_nu yr
duplicates drop
expand 8

bys offid_nu yr: gen gp = _n
merge m:1 gp using `disc', nogen
drop gp

merge 1:m offid_nu yr discipline using `unbal'
foreach v of varlist nw nv nepi {
  replace `v' = 0 if _m==1
  assert `v' !=.
}
drop _m

drop if discipline=="RD" | discipline=="FS"

*merge by office-yr the mean total # episodes
merge m:1 offid_nu yr using `totnepi', nogen
rename totnepi totnepi_oy

*reorder discipline categories
gen gp = 1 if disc=="SN"
replace gp = 2 if disc=="PT"
replace gp = 3 if disc=="OT"
replace gp = 4 if disc=="HHA"
replace gp = 5 if disc=="ST"
replace gp = 6 if disc=="MSW"
assert gp!=.
lab define gpn 1 "SN" 2 "PT" 3 "OT" 4 "HHa" 5 "ST" 6 "MSW", replace
lab val gp gpn

*add the senior living office indicator
merge m:1 offid_nu using office_restrict, keepusing(seniorliv) keep(1 3) nogen
*2 offices not matched; manually recode
replace seniorliv = 0 if seniorliv==. & (offid_nu==120 | offid_nu==615)

*create % workers / visits (episodes) in each discipline for each office-yr
foreach v of varlist nw nv {
  bys offid_nu yr: egen tot`v' = sum(`v')
  gen p`v' = 100*`v'/tot`v'
}

compress
outsheet using distr_scope.csv, comma names replace

graph bar (asis) nw if yr==2012, over(gp) over(offid_nu, sort(totnw) descending label(angle(90) labsize(small))) stack asyvars percentages ||
*|| scatter totnw offid_nu if yr==2012, yaxis(2)
graph export `gph'/test.eps, replace

*---
*create office size category based on the total patient census for each office-yr
