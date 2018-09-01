*create hospital panel data for each calendar year

cd /home/hcmg/kunhee/Labor/Bayada_data

*append all years of Cost report data
clear
forval y = 2000/2009 {
    append using hospcr`y'
}
append using hospcr2010_2552_96
append using hospcr2011_2552_96

replace prov_num = prvdr_num if prov_num==""
assert prov_num==prvdr_num
drop prvdr_num
assert fyear!=.

*recode some vars as 0/1 dummies
lab define provtype 1 "general short-term" 2 "general long-term" 3 "cancer" 4 "psychiatric" 5 "rehab" 6 "religious non-medical" 7 "children" 8 "alcohol/drug" 9 "other", replace

foreach f in "hosp" "ipf" "swbsnf" "swbnf" "rhc" "snf" "hha" "hospice" "nf" "irf" "renal" "cmhc" "fqhc" "asc" "subpr" {
  *convert date variable to Stata date
  loc v `f'_dtcert
  capture split `v', destring parse(/)
  capture gen a = mdy(`v'1, `v'2, `v'3)
  capture drop `v'*
  capture rename a `v'
  capture format `v' %d

  capture lab var `f'_name "`f' name"
  capture lab var `f'_ccn "`f' CMS provider ID"
  capture lab var `f'_cbsa "`f' CBSA code"
  capture lab var `f'_dtcert "`f' date of certification"
  capture lab var `f'_provtype "`f' provider type"
  capture lab val `f'_provtype provtype
}

foreach v of varlist cr_start cr_end {
  split `v', destring parse(/)
  gen a = mdy(`v'1, `v'2, `v'3)
  drop `v'*
  rename a `v'
  format `v' %d
}

foreach v of varlist teaching ltch cah dissh {
  tab `v'
  assert `v'=="Y" | `v'=="N" | `v'==""

  gen a = `v'=="Y"
  replace a = . if `v'==""
  drop `v'
  rename a `v'
  lab var `v' "=1 if hosp is `v'"
}

loc v urban
tab `v'
gen a = `v'==1
replace a = . if `v'==.
drop `v'
rename a `v'
lab var `v' "=1 if hosp is `v'"

*variable labels
lab var beds "# beds"
lab var dischrg "# discharges"
lab var malprins_premium "amount of malpractice insurance premiums"
lab var malprins_paidloss "amount of malpractice insurance paid losses"
lab var malprins_selfins "amount of malpractice insurance self insurance"
lab var cr_start "cost reporting period start"
lab var cr_end "cost reporting period end"
lab var dissh "dummy for disproportionate share hospital adjustment"

*drop variables with mostly missing values
capture drop fy_end_dt fy_bgn_dt

*the old format CR data don't specify whether the hospital's subprovider is IRF or IPF -> rely on the dummies for having IRF or IPF; but these variables started to have a "Y" response in 2002, so restrict to 2002-2011 data for IRF / IPF CCN variables

*create IRF CCN & IPF CCN variables
gen irf_ccn = subpr_ccn if irf=="Y"
gen ipf_ccn = subpr_ccn if ipf=="Y"

*drop unnecessary vars
drop adr_vndr_cd fi_num trnsmtl_num rpt_stus_cd

*unstandardized state names; get the first 2 digits from provider number and link with SSA county code-state name xwalk data
gen ssastate = substr(prov_num,1,2)
rename state st0
merge m:1 ssastate using xwalk/ssa_fips_state_crosswalk, keep(1 3) nogen keepusing(state)
tab ssastate if state==""
*48, 65, 66, 67
* 48 = Virgin Islands (http://www.nber.org/ssa-fips-state-county-crosswalk/ssa_statecodes.pdf)
/* 65 = Guam
66 = Saipan */
replace state = "TX" if ssastate=="67" & state==""

drop if state=="PR" | state=="MP" | state=="GU" | state=="VI" | state==""

*even if there are duplicates per prov_num-FY, it seems to cover different CR periods (see cr_start, cr_end vars), so don't worry
drop ssastate st0

*if a hospital-FY appears more than once, keep the record whose fiscal intermediary receipt date is later
duplicates tag prov_num fyear, gen(dup)
tab dup
bys prov_num fyear: egen md = max(fi_rcpt_dt)
drop if md!=fi_rcpt_dt & dup > 0
drop dup md
*list prov_num fy *dt in 1/30

duplicates tag prov_num fyear, gen(dup)
tab dup
*if a hospital-FY still appears more than once, keep the record whose fiscal intermediary creation date is later
bys prov_num fyear: egen md = max(fi_creat_dt)
drop if md!=fi_creat_dt & dup > 0
drop dup

duplicates tag prov_num fyear, gen(dup)
tab dup
assert dup==0
drop dup

keep prov_num *ccn teaching urban own_* dissh *rev* *inc* beds *dischrg fyear SSIratio Medicaid_ratio DSHratio DSHadjust

compress
save hospcr_panel_2000_2011, replace
