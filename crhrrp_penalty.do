*Create 2012 penalty rate data constructed from Medicare claims data

loc path /home/hcmg/kunhee/hrrp-home/data/
cd `path'

import excel using hrrp_penalty.xlsx, firstrow sheet("ami") clear
ren cpnltrevprop penrate_ami
recode penrate_ami (mis=0)
keep hospital penrate_ami

tempfile hrrp_ami
save `hrrp_ami', replace

import excel using hrrp_penalty.xlsx, firstrow sheet("chf") clear
ren cpnltrevprop penrate_hf
recode penrate_hf (mis=0)
keep hospital penrate_hf

tempfile hrrp_hf
save `hrrp_hf', replace

import excel using hrrp_penalty.xlsx, firstrow sheet("pneum") clear
ren cpnltrevprop penrate_pn
recode penrate_pn (mis=0)
keep hospital penrate_pn

tempfile hrrp_pn
save `hrrp_pn', replace

use `hrrp_ami', clear
merge 1:1 hospital using `hrrp_hf'
drop _m
merge 1:1 hospital using `hrrp_pn'
drop _m
ren hospital prvdr_num
destring prvdr_num, force replace
sort prvdr_num

recode penrate_ami penrate_hf penrate_pn (mis=0)

compress
save hrrp_penalty, replace
