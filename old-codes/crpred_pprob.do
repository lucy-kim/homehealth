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
