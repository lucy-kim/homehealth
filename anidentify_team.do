* identifies the presence of team-based provision of home health care
* worker side: number of disciplines per worker, number of workers and volume of visits in each discipline
* patient side: how many patients are getting different combinations of disciplines (e.g. 3 nurse visits, 3 physical therapy visits, 2 aide visits); how many visits per discipline; restrict to patients getting 5 or more visits
* use visit-level data for 2012-2015
* output contained in initial-overview-identifying-teams.docx

*--------------
* set up
*--------------
set linesize 120
set matsize 11000
local home /home/hcmg/kunhee/Labor
loc reg /home/hcmg/kunhee/Labor/regresults
loc gph /home/hcmg/kunhee/Labor/gph
cd `home'/Bayada_data


*--------------
* worker side: number of disciplines per worker, number of workers and volume of visits in each discipline
*--------------
use visit_worker_chars, clear
*this dataset spans 1/1/2012 - 12/31/2015

duplicates tag payrollno visitd visittime, gen(dup)
tab dup
*5K (0.1%) obs are duplicates which differ by visit type (e.g. recert vs subsequent visit), service code, description variables
drop dup

tempfile tmp
save `tmp'

*number of disciplines per worker
gen i = 1
keep payrollno discipline i visitdate visittime offid_nu
duplicates drop

duplicates tag payrollno visitd visittime, gen(dup)
tab dup
*still there are 600 visits where workers provided multiple visits (usually for different offices or different disciplines) at the same visit day-time - keep these for now
drop dup

*drop employee ID "-99P" since that might be just a wildcard ID
drop if payrollno=="-99P"

tempfile tmp2
save `tmp2'

use `tmp2', clear
collapse (sum) nv = i, by(payrollno discipline)

*number of workers and volume of visits in each discipline
preserve
keep payrollno discipline
duplicates drop
tab discipline, sort
restore

preserve
collapse (sum) nv, by(discipline)
gsort -nv
egen t = sum(nv)
gen pct = 100*nv/t
list
restore

*total number of unique workers, and # disciplines per worker
sort payrollno discipline
bys payrollno discipline: gen j = 1 if _n==1

preserve
collapse (sum) ndisc_pw = j, by(payrollno)
tab ndisc
restore

*what are the combination of disciplines for the 3% workers with 2 disciplines? Are the visits evenly distributed between the 2 disciplines?
bys payrollno: egen ndisc_pw = sum(j)

preserve
keep if ndisc_pw==2
keep payrollno discipline nv

*sort by # visits in the discipline from large to small
gsort payrollno -nv
bys payrollno: gen disc`x'_n = _n

forval x = 1/2 {
  gsort payrollno -nv
  bys payrollno: gen disc`x' = discipline if _n==`x'
  loc v disc`x'
  gsort payrollno -`v'
  bys payrollno: replace `v' = `v'[_n-1] if `v'==""
}

*concatenate the discipline combination
gen disc_comb = disc1 + "-" + disc2
drop disc? discipline

* to answer Are the visits evenly distributed between the 2 disciplines?, get the % change
reshape wide nv, i(payrollno disc_comb) j(disc_n)
gen rat = nv1/nv2
gen pctch = 100*(nv1 - nv2)/nv2

*--------------
* patient side: how many patients are getting different combinations of disciplines (e.g. 3 nurse visits, 3 physical therapy visits, 2 aide visits); how many visits per discipline; restrict to patients getting 5 or more visits
*--------------
use `tmp', clear
duplicates drop payrollno discipline visitdate visittime offid_nu, force

*merge with episode ID-admission ID data to see which episodes belong to the same admission
merge m:1 epiid using epiid_admitID, keep(3) nogen
*193K have _m=1; 7K have _m=2; 5M have _m=3

*drop admissions that started only 60 or fewer days before 12/31/2015 b/c I can't know if these would have been recerts or not after this episode (i.e. right truncated)
gen cutoff = mdy(12,31,2015) - 59
*focus on admissions that started 61 or more days before 12/31/2015 so I know there was no recertification
drop if socdate_e > cutoff

*how many episodes in an admission
sort admissionclie epiid
bys admissionclie epiid: gen i = _n==1
bys admissionclie: egen nepi = sum(i)
bys admissionclie: gen j = _n==1
tab nepi if j==1

*focus on single-episode admissions &
keep if nepi==1

tempfile tmp3
save `tmp3'


*create a concatenated string showing the combination of disciplines used in each admission
gen k = 1
keep admissionclie discipline k

collapse (sum) nv = k , by(admissionclie discipline)

bys admissionclie: egen tnv = sum(nv)

preserve
keep admissionclie tnv
duplicates drop
tab tnv
label var tnv "Total number of visits"

estpost tabulate tnv
esttab using `reg'/tnvdist.csv, cells("b(label(Counts)) pct(label(Percent) fmt(2)) cumpct(label(Cumulative %) fmt(2))") varlabels(, blist(Total "{hline @width}{break}")) nonumber nomtitle noobs replace label
restore

*within a patient, sort discipline in an alphabetical order
forval x = 1/8 {
  sort admissionclie discipline
  bys admissionclie: gen disc`x' = discipline if _n==`x'
  loc v disc`x'
  gsort admissionclie -`v'
  bys admissionclie: replace `v' = `v'[_n-1] if `v'==""
}

*concatenate the discipline combination
gen disc_comb = disc1 + "-" + disc2 + "-" + disc3 + "-" + disc4 + "-" + disc5 + "-" + disc6 + "-" + disc7 + "-" + disc8

preserve
keep admissionclie disc_comb tnv
duplicates drop

bys disc_comb: egen cnt = sum(1)

tab disc_comb, sort
label var disc_comb "Discipline combination"

estpost tabulate disc_comb, sort
esttab using `reg'/disc_comb_dist.csv, cells("b(label(Counts)) pct(label(Percent) fmt(2)) cumpct(label(Cumulative %) fmt(2))") varlabels(, blist(Total "{hline @width}{break}")) nonumber nomtitle noobs replace label
restore

drop disc?

tempfile tmp4
save `tmp4'

*---------
*simple exercise: focus on the admissions with PT-SN combination.
*---------

*how many total visits?
keep if disc_comb=="PT-SN------"
sort admissionclie discipline
bys admissionclie: gen i = _n==1
sum tnv if i==1, de

hist tnv if i==1, frac subtitle("Admissions with PT & SN visits") width(1) xti("Total number of visits") xlab(0(10)80) note(Notes. The median visit count is 11.) fcolor(none)
graph export `gph'/tnvdist_ptsn.eps, replace

* group admissions by total # visits: 1) 2 (min) -6 visits ; 2) 7 (25th pctile) - 10 visits; 3) 11 (50th pctile) - 13 visits; 4) 14 (75th) - 18 visits; 5) 19 (90th) or more
sum tnv if i==1 , de
loc p25 = `r(p25)'
loc p50 = `r(p50)'
loc p75 = `r(p75)'
loc p90 = `r(p90)'

gen gp = .
replace gp = 1 if tnv < `p25'
replace gp = 2 if tnv >= `p25' & tnv < `p50'
replace gp = 3 if tnv >= `p50' & tnv < `p75'
replace gp = 4 if tnv >= `p75' & tnv < `p90'
replace gp = 5 if tnv >= `p90'
assert gp!=.

drop i disc_comb
reshape wide nv, i(admissionclie gp tnv) j(discipline) string
lab define tnvcat 1 "2-6 visits" 2 "7-10 visits" 3 "11-13 visits" 4 "14-18 visits" 5 "19+ visits"
lab val gp tnvcat
tab gp, summarize(nvSN)
tab gp, summarize(nvPT)

*---------
*is it usually the same set of PT and SN that visit patients?
*---------
keep admissionclie
duplicates drop
merge 1:m admissionclie using `tmp3', keep(3) nogen

*create a simpler employee ID
egen emplid = group(payrollno)

*count the number of unique workers visiting a patient
sort admissionclie payrollno
capture drop i
bys admissionclie payrollno : gen i = _n==1
bys admissionclie: egen nw = sum(i)

capture drop j
bys admissionclie: gen j = _n==1
tab nw if j==1

tempfile tmp5
save `tmp5'

*check whether any two workers like to work together first
use `tmp5', clear
keep emplid payrollno admissionclie discipline
gen i = 1
collapse (sum) nv = i , by(admissionclie emplid)

*within a patient, sort employee ID
forval x = 1/13 {
  sort admissionclie emplid
  bys admissionclie: gen empl`x' = emplid if _n==`x'
  loc v empl`x'
  gsort admissionclie -`v'
  bys admissionclie: replace `v' = `v'[_n-1] if `v' >= .
}

*convert employee IDs to string variables
foreach v of varlist empl? empl?? {
  tostring `v', replace
}

*concatenate any two employee IDs to make an employee ID combination (only up to patients seeing 4 different workers because "n choose 2" becomes too large as n > 4)
bys admissionclie: gen N = _N
sum N
gen empl_pair = ""
bys admissionclie: replace empl_pair = empl1 + "-" + empl2 if _n==1
bys admissionclie: replace empl_pair = empl1 + "-" + empl3 if _n==2 & N > 2
bys admissionclie: replace empl_pair = empl2 + "-" + empl3 if _n==3 & N > 2
bys admissionclie: replace empl_pair = empl1 + "-" + empl4 if _n==4 & N > 3
bys admissionclie: replace empl_pair = empl2 + "-" + empl4 if _n==5 & N > 3
bys admissionclie: replace empl_pair = empl3 + "-" + empl4 if _n==6 & N > 3

preserve
keep if N <= 4

bys empl_pair: egen nadm = sum(1)
keep empl_pair nadm
duplicates drop
gsort -nadm
drop if empl_pair==""

hist nadm, frac subtitle("Number of admissions visited by the same pair of workers") xti("Number of admissions") note("Notes. Among 90% of patients having visits in SN and PT only seeing 4 different workers at most." "The mean is 4, median 2.") width(1)
graph export `gph'/empl_pairdist_ptsn.eps, replace
