*create visit-level data before measuring home health agencies' effort level for each patient 

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults

cd `path'

*tag AMI, HF, PNEU, COBG conditions
use pats_hospreferred, clear
merge 1:m clientid socdate_e using inpat_dx, keep(3) nogen
*dropped 3522 obs (2%)

*for ICD-9-CM codes for each condition, see Tables D.X.1  https://www.qualitynet.org/dcs/ContentServer?cid=1228774371008&pagename=QnetPublic%2FPage%2FQnetTier4&c=Page
gen icd2 = inpat_dx_cor

*remove "."
replace icd2 = subinstr(icd2, ".", "",.)
destring icd2, replace

* AMI (from Table D.1.1 in 2016 report)
tab inpat_dx_cor if substr(inpat_dx_cor,1,3)=="410"
gen ami = icd2==41000 | icd2==41001 | icd2==41010 | icd2==41011 | icd2==41020 | icd2==41021 | icd2==41030 | icd2==41031 | icd2==41040 | icd2==41041 | icd2==41050 | icd2==41051 | icd2==41060 | icd2==41061 | icd2==41070 | icd2==41071 | icd2==41080 | icd2==41081 | icd2==41090 | icd2==41091
tab inpat_dx_cor if ami

* HF
tab inpat_dx_cor if substr(inpat_dx_cor,1,3)=="402" | substr(inpat_dx_cor,1,3)=="404" | substr(inpat_dx_cor,1,3)=="428"

gen hf = icd2==40201 | icd2==40211 | icd2==40291 | icd2==40401 | icd2==40403 | icd2==40411 | icd2==40413 | icd2==40491 | icd2==40493 | icd2==4280 | icd2==4281 | icd2==42820 | icd2==42821 | icd2==42822 | icd2==42823 | icd2==42830 | icd2==42831 | icd2==42832 | icd2==42833 | icd2==42840 | icd2==42841 | icd2==42842 | icd2==42843 | icd2==4289
tab icd2 if hf
tab inpat_dx_cor if hf

*PNEU
gen pneu = icd2==4800 | icd2==4801 | icd2==4802 | icd2==4803 | icd2==4808 | icd2==4809 | icd2==481 | icd2==4820 | icd2==4821 | icd2==4822 | icd2==48230 | icd2==48231 | icd2==48232 | icd2==48239 | icd2==48240 | icd2==48241 | icd2==48242 | icd2==48249 | icd2==48281 | icd2==48282 | icd2==48283 | icd2==48284 | icd2==48289 | icd2==4829 | icd2==4830 | icd2==4831 | icd2==4838 | icd2==485 | icd2==486 | icd2==4870 | icd2==48811 | icd2==5070
tab inpat_dx_cor if pneu

* COPD
tab inpat_dx_cor if substr(inpat_dx_cor,1,3)=="491" | substr(inpat_dx_cor,1,3)=="492" | substr(inpat_dx_cor,1,3)=="493" | substr(inpat_dx_cor,1,3)=="496" | substr(inpat_dx_cor,1,3)=="518" | substr(inpat_dx_cor,1,3)=="799"
gen copd = inpat_dx_cor=="491.2" | inpat_dx_cor=="491.21" | inpat_dx_cor=="491.22" | inpat_dx_cor=="491.8" | inpat_dx_cor=="491.9" | inpat_dx_cor=="492.8" | inpat_dx_cor=="493.20" | inpat_dx_cor=="493.21" | inpat_dx_cor=="493.22" | inpat_dx_cor=="496"
tab inpat_dx_cor if copd

*Stroke
gen stroke = icd2==43301 | icd2==43311 | icd2==43321 | icd2==43331 | icd2==43381 | icd2==43391 | icd2==43401 | icd2==43411 | icd2==43491 | icd2==436
tab inpat_dx_cor if stroke

drop icd2

*restrict to fy 2013-2015 data

* create fiscal year
capture drop fy
gen fy = .
forval y=2012/2016 {
  loc yl1 = `y'-1
  replace fy = `y' if socdate >= mdy(7,1,`yl1') & socdate <= mdy(6,30,`y')
}
assert fy!=.
drop if fy==2012

*create data unique at the admission ID-office ID level
keep admissionclie offid_nu ami hf pneu copd stroke provider_hosp fy
duplicates drop
collapse (max) ami hf pneu copd stroke, by(admissionclie offid_nu provider_hosp fy)
duplicates tag admissionclie offid_nu, gen(dup)
assert dup==0
drop dup

tempfile an
save `an'

*-----------------------------

* create visit-level data

*first get epi ID by merging with admission ID - episode ID xwalk
use `an', clear
keep admissionclientsocid fy provider_hosp
duplicates drop
merge 1:m admissionclientsocid using epiid_admitID, keep(1 3) nogen
*all matched

*drop if matched to multiple episodes
duplicates tag admissionclientsocid, gen(dup)
drop if dup > 0
drop dup

*merge with visit-level data for the episodes
rename admissionclie admissionclie_str
merge 1:m epiid using epi_visit, keep(1 3) nogen
*all matched
drop if offid_nu ==.
drop admissionclientsocid
rename admissionclie_str admissionclientsocid

foreach cc in "ami" "hf" "pneu" "copd" {
  tab `cc'
  rename `cc' `cc'_epi
}

*get updated target condition indicators from the above data
merge m:1 admissionclientsocid using `an', keepusing(ami hf pneu copd stroke) keep(1 3) nogen
foreach cc in "ami" "hf" "pneu" "copd" {
  drop `cc'_epi
}
drop pneu_new

*drop episodes that had a visit on the day of readmission
sort epiid visitdate
gen hadvisit_onra = firsthospdate == visitdate & hashosp==1
bys epiid: egen max = max(hadvisit_onra)
drop if max==1
drop max hadvisit_onra

* duration of episode: make sure we have single episode cases
capture drop epilength
egen llvd = rowmax(firsthospdate lvd)
format llvd %d
gen epilength = llvd - fvd + 1

sort epiid visitdate
bys epiid: gen i =_n==1
count if i==1
*54K episodes
tab epilength if i==1
drop if epilength > 60

*create indicators for the 1st week, 2nd week, 1st 2 weeks
assert fvd==visitd if i==1
gen dayidx = visitd - fvd + 1
gen wkidx = 1 if dayidx <= 7
forval x=2/9 {
  replace wkidx = `x' if dayidx > 7*(`x'-1) & dayidx <= 7*`x'
}
assert wkidx!=.
assert wkidx ==2 if dayidx > 7 & dayidx <= 14

gen firstwk = dayidx <= 7
gen first2wk = dayidx <= 14
gen second_lastwk = wkidx!=1

count if epilength > 14 & i==1
count if i==1
*78% of episodes had duration of > 2 weeks
drop i

duplicates drop

compress
save HHeffort_visit, replace
