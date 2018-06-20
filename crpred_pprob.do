*predicted penalty probability for 2012 and for 2013

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults
cd `path'

foreach d in "ami" "chf" "pneum" {
  loc f probpnlt_`d'_v3.xlsx
  import excel using `f', clear sheet("y2012") firstrow

  sum pnltpr
  replace pnltpr = 1 if pnltpr > 1

  *destring provider ID
  gen x = real(provider)
  list provider if x==.
  drop if x==.
  destring provider, replace
  keep provider pnltpr
  duplicates drop

  renvars provider pnltpr \ prvdr_num pnltprob

  gen cond = "`d'"
  replace cond = "hf" if cond=="chf"
  replace cond = "pn" if cond=="pneum"
  tempfile `d'
  save ``d''
}

clear
foreach d in "ami" "chf" "pneum" {
  append using ``d''
}

rename pnltprob pnltprob_
reshape wide pnltprob_, i(prvdr) j(cond) st

compress
save pred_pprob, replace

*--------------
use pred_pprob, clear
rename pnltprob_base pnltpr_
reshape wide pnltpr_, i(prvdr) j(cond) st
*merge with 2012 HRRP penalty data
merge 1:m prvdr_num using `shref_hj2012', keep(3) nogen
*2876 obs reduced to 2147 obs after matching

*compute penalty pressure: use 2012 data, product of share of the office j's patients that come from hosp h and h's penalty rate
sort offid_nu prvdr_num

foreach d in "ami" "hf" "pn" {
  capture drop pnltprs_`d'
  gen pnltpr_`d' = shref_hj * pnltpr_`d'
}
