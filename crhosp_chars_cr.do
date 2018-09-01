*create hospital characteristics data from the cost report using 2010-2017 files

cd /home/hcmg/kunhee/Labor/Bayada_data

*append all years of Cost report data
use hospcr_panel_2010_2016, clear
append using hospcr_panel_2000_2011

*restrict to short-term general hospitals by looking at the last 4 digits fo provider number
*0001-0879 from https://www.resdac.org/sites/resdac.umn.edu/files/Provider%20Number%20Table.txt
gen last4 = substr(prov_num,3,4)
gen nlast4 = real(last4)
gen stg = nlast4 >= 0 & nlast4 <= 879
tab fyear if stg==1
keep if stg==1
drop stg *last4
drop if cah=="Y"

* dummy for VI with each type of PAC
foreach f in "ipf" "swbsnf" "swbnf" "rhc" "snf" "hha" "hospice" "nf" "irf" "renal" "cmhc" "fqhc" "asc" {
  gen vi_`f' = `f'_ccn!=""
  destring `f'_ccn, replace
}
sum vi*

*for # beds, recode to missing if > 3000 (only 4 hosps)
replace beds = . if beds > 3000

drop uncomp2

*urban = 1 for urban ; 2 for rural
gen x_urban = urban==1
replace x_urban = . if urban==.
drop urban
rename x_urban urban

*use fyear as the year basis
collapse (max) vi_* teaching urban own_* uncomp* dissh (mean) *rev* *inc* beds *dischrg snfdays swbsnfdays totepi_st totepi_out SSIratio Medicaid_ratio DSHratio DSHadjust pionACO, by(prov_num fyear)

destring prov_num, gen(provid)
drop prov_num
rename fyear fy

*fill in missing values downstream & upstream
sort provid fy
order provid fy
foreach v of varlist vi_ipf-totepi_out {
  bys provid: replace `v' = `v'[_n-1] if `v'>=.
}
gsort provid -fy
foreach v of varlist vi_ipf-totepi_out {
  bys provid: replace `v' = `v'[_n-1] if `v'>=.
}
sort provid fy
count if beds==.

*create hospital size category
gen size = 1 if beds <= 100
replace size = 2 if beds >100 & beds <= 300
replace size = 3 if beds > 300
assert size!=.
replace size = . if beds==.

*variable labels
lab var beds "# beds"
lab var dischrg "# discharges"
lab var mcre_dischrg "# Medicare discharges"
lab var mcaid_dischrg "# Medicaid discharges"
lab var ti5_dischrg "# Title V discharges"
lab var totepi_st "total number of episodes (standard/nonoutlier) in hosp-based HHA"
lab var totepi_out "otal number of outlier episodes in hosp-based HHA"
lab var dissh "dummy for disproportionate share hospital adjustment"
lab var SSIratio "% SSI recipient pat days to Medicare Part A pat days"
lab var Medicaid_ratio "% Medicaid pat days to total pat days"
lab var DSHratio "Allowable DSH %"
lab var DSHadjust "DSH adjustment"
lab var pionACO "=1 if participate in Pioneer ACO"

compress
saveold hosp_chars_cr, replace

outsheet using hosp_chars_cr.csv, comma names replace
