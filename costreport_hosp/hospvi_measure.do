* for hospital-CY, get vertical integration measure
* dummy for VI, mix of PACs if VI=1, # providers per PAC type

loc reg /home/hcmg/kunhee/Labor/regresults

cd /home/hcmg/kunhee/Labor/Bayada_data

use hospcr_panel_2010_2016, clear
append using hospcr_panel_1996_2011

*restrict to short-term general hospitals by looking at the last 4 digits fo provider number
*0001-0879 from https://www.resdac.org/sites/resdac.umn.edu/files/Provider%20Number%20Table.txt
gen last4 = substr(prov_num,3,4)
gen nlast4 = real(last4)
gen stg = nlast4 >= 0 & nlast4 <= 879
tab fyear if stg==1
keep if stg==1
drop stg *last4
drop if cah==1

foreach v of varlist cah ltch {
  assert `v'==0 if `v'!=.
  drop `v'
}

duplicates tag prov_num fyear , gen(dup)
tab dup

gen totsnfdays = snfdays + swbsnfdays

* dummy for VI with each type of PAC
foreach f in "ipf" "swbsnf" "swbnf" "rhc" "snf" "hha" "hospice" "nf" "irf" "renal" "cmhc" "fqhc" "asc" {
  gen vi_`f' = `f'_ccn!=""
  destring `f'_ccn, replace
}
sum vi*

tempfile an
save `an'

use `an', clear

*use fyear as the year basis
collapse (max) vi_* teaching urban own_* uncomp* dissh (mean) beds dischrg totsnfdays snfdays swbsnfdays totepi_st totepi_out , by(prov_num state fyear)

*tag outlier bed size hospitalss
tab beds
tab prov if beds > 3000 & bed!=.
gen outlierbed = beds > 3000 & bed!=.
bys prov: egen mm = max(outlierbed)

preserve
keep if mm==1
keep prov
duplicates drop
merge 1:m prov using `an', keepusing(beds fy) keep(3) nogen
sort prov fy
restore

*recode the outlier # beds as missing and use the previous FY's value
use `an', clear
sum beds, de
replace beds = . if beds > 3000 & beds!=.
sort prov fy
bys prov: replace beds = beds[_n-1] if beds >=.

*use fyear as the year basis
collapse (max) vi_* teaching urban own_* uncomp* dissh (mean) beds dischrg totsnfdays snfdays swbsnfdays totepi_st totepi_out , by(prov_num state fyear)

*for years before 2002, recode dummy for VI with irf & ipf because the IRF/IPF dummies were mostly missing
tab fyear, summarize(vi_irf)
tab fyear, summarize(vi_ipf)
replace vi_irf = . if fyear < 2002
replace vi_ipf = . if fyear < 2003

*create hospital size category
gen size = 1 if beds <= 100
replace size = 2 if beds >100 & beds <= 500
replace size = 3 if beds > 500
assert size!=.
replace size = . if beds==.

tempfile an2
save `an2'

tab fyear
drop if fyear==2016
*2016 has significantly fewer hospitals

*for each year, % hospitals with each PAC type
bys fy: sum vi_* teaching own* beds

*across years, the trend in VI for each PAC type

*reshape long
reshape long vi_, i(prov_num fyear state) j(pac) string
rename vi_ vi
tab pac, summarize(vi)

tab fyear if pac=="renal", summarize(vi)

drop if pac=="asc" | pac=="cmhc" | pac=="fqhc" | pac=="rhc" | pac=="renal"

compress

preserve
outsheet using hosp_vi.csv, replace names comma
restore

save hospvi, replace

*---------
*Q. Does a hospital change a vertical integration status with a PAC type during the sample period?

use hospvi, clear
*4858 general short-term hospitals
*restrict to 4 PAC types
keep if pac=="hha" | pac=="hospice" | pac=="irf" | pac=="snf"

sort prov_num pac cy
list prov_num cy pac vi in 1/30

collapse (mean) vi, by(prov_num pac)
*--------
*Q. Does the same hospital typically vertically integrate with more than one PAC?

use hospvi, clear
*4858 general short-term hospitals
*restrict to 4 PAC types
keep if pac=="hha" | pac=="hospice" | pac=="irf" | pac=="snf"

keep if vi==1

*throughout the sample period, what are the integrated PACs for each hospital?
keep prov_num pac
duplicates drop

*within a hospital, sort PAC type in an alphabetical order
forval x = 1/4 {
  sort prov_num pac
  bys prov_num: gen pac`x' = pac if _n==`x'
  loc v pac`x'
  gsort prov_num -`v'
  bys prov_num: replace `v' = `v'[_n-1] if `v'==""
}

*concatenate the discipline combination
gen pac_comb = pac1 + "-" + pac2 + "-" + pac3 + "-" + pac4

keep prov_num pac_comb
duplicates drop

tab pac_comb, sort

label var pac_comb "Vertically integrated post-acute care combination"

estpost tabulate pac_comb, sort
esttab using `reg'/pac_comb_dist.csv, cells("b(label(Counts)) pct(label(Percent) fmt(2)) cumpct(label(Cumulative %) fmt(2))") varlabels(, blist(Total "{hline @width}{break}")) nonumber nomtitle noobs replace label
