*crCMShospdisch_tohh.do
*use hospital-week level counts of Medicare hospital discharges to home health destinations for 2011-2014 (Source: Elena's CMS data)
loc path /home/hcmg/kunhee/Labor/Bayada_data
cd `path'/Hospital/CMSclaims
loc lag 4

use HHA2011_merge_kunhee_weekly.dta, clear

forval yr=2012/2014 {
    append using HHA`yr'_merge_kunhee_weekly.dta
}
duplicates tag provider monday, gen(dup)
tab dup
*duplicates only for weeks starting on last December date of the previous year
collapse (sum) total_dischar, by(provider monday prstate_hosp)

tempfile appended
save `appended'

keep monday
duplicates drop
drop if year(monday) < 2012
sort monday
assert monday==monday[_n-1] + 7 if _n > 1
egen tot = sum(1)
sum tot, meanonly
loc nwk = `r(mean)'
tempfile weekly
save `weekly'

use `appended', clear
keep provider_hosp
duplicates drop
expand `nwk'
sort provider
bys provider: gen monday = mdy(1,2,2012) if _n==1
bys provider: replace monday = monday[_n-1] + 7 if _n > 1
assert monday!=.

*merge with unbalanced panel at the hospital-week level
merge 1:1 provider monday using `appended'
*_m==1 means no discharges to HH on those weeks from that hospital
assert year(monday) < 2012 if _m==2
drop if year(monday) < 2012
sort provider monday
format monday %d

*recode # discharges to HH to 0 if _m==1
replace total_dischar = 0 if _m==1
drop _merge

sort provider monday
rename total_dischar hdis2hh

compress
save `path'/CMShospdisch_tohh, replace
