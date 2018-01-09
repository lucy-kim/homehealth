* anvar_hrrppenalty.do
* how much pressure from hospitals does each office get?
* within-office over-time variation in incentive/pressure for hospitals to decrease readmissions

loc path /home/hcmg/kunhee/Labor/Bayada_data/
cd `path'
loc gph /home/hcmg/kunhee/Labor/gph

use referralhosp_penalty, clear

* Fix an office. Unit of year = fiscal year (Oct 1 - Sept 30)

* create fiscal year
gen fy = .
forval y=2012/2016 {
  loc yl1 = `y'-1
  replace fy = `y' if socdate >= mdy(10,1,`yl1') & socdate <= mdy(9,30,`y')
}
assert fy!=.

* recode "not assessed" values in the penalty rate to missing value before destringing
tab fy2013readmis if regexm(fy2013readmis, "[^0-9 .]")
foreach v of varlist fy201?readmis* {
  replace `v' = "" if `v'=="Not Assessed"
}
destring fy201?readmis* , replace ignore("%")

* drop hospitals not subject to penalty, i.e. penalty = missing value
forval y=2013/2015 {
  drop if fy`y'readmis==.
}

* drop offices that had < 100 admissions referred by hospitals
bys offid_nu: gen obs = _N
drop if obs < 100
* 29 offices dropped; 77 offices kept

* how many offices operated in each FY
preserve
keep offid_nu fy
duplicates drop
keep if fy >= 2012 & fy <= 2015
bys offid: gen nn = _N
restore

*---------------
* crude measure of penalty pressure for each office-FY = weighted average of penalty across hosp's using the share of referrals from each hosp-FY as weights

* create weights for each office-hosp-FY
bys offid_nu fy prvdr_num : gen adm_ijt = _N
bys offid_nu fy: gen adm_it = _N
gen wgt = adm_ijt/adm_it

tempfile admit_lvl
save `admit_lvl'

*-------------
* change to office-fy-hosp level data
use `admit_lvl', clear
keep offid_nu fy prvdr_num wgt fy201?readmiss* adm_ijt adm_it
duplicates drop
reshape wide wgt adm_ijt adm_it, i(offid_nu prvdr_num fy201?readmiss*) j(fy)

* fill in missing values for the total # referrals per office-FY
forval y = 2012/2016 {
  gsort offid_nu -adm_it`y'
  bys offid_nu: replace adm_it`y' = adm_it`y'[_n-1] if adm_it`y' >=.

  *if still missing for the office-FY, then no admissions in that year
  replace adm_it`y' = 0 if adm_it`y'==.
}

* fill in missing values for the number of admissions per office-hospital-FY with zero's
forval y = 2012/2016 {
  replace adm_ijt`y' = 0 if adm_ijt`y'==.
}

*recode wgt to missing if the total admission in that office-FY is zero; otherwise shouldn't be zero - just didn't receive referrals from the hospital in that FY, so fill in zero's
forval y = 2012/2016 {
  replace wgt`y' = 0 if wgt`y'==. & adm_it`y'!=0
  assert adm_it`y'==0 if wgt`y'==.
}

* create a weighted average of penalty rates across hospitals
forval y = 2013/2015 {
  gen prod`y' = fy`y'readmiss * wgt`y'
}

* create a weighted average of penalty using the 2012 weight
forval y = 2013/2015 {
  gen prod_fixwgt`y' = fy`y'readmiss * wgt2012
}

tempfile ijt
save `ijt'

*----------------
* cross-sectional variation in the overall penalty exposure across home health offices in each FY
use `ijt', clear

* create a operation indicator = 1 if the office had any hospital referral (i.e. adm_it > 0)
forval y = 2013/2015 {
  gen operate`y' = prod`y'!=.
}

collapse (sum) prod* (mean) operate*, by(offid_nu)

*reshape long to panel data for each office
reshape long prod prod_fixwgt operate, i(offid_nu) j(fy)

keep if operate==1

* drop offices that span only 1 FY
bys offid_nu: gen nn = _N
drop if nn == 1
drop nn

* within-FY , distribution of penalty shock across offices
kdensity prod if operate==1, nograph generate(x fx)
forval y = 2013/2015 {
  kdensity prod if fy==`y' & operate==1, nograph generate(fx`y') at(x)
  label var fx`y' "FY `y'"
}
line fx2013 fx2014 fx2015 x, sort ytitle(Density) xtitle("Weighted average of penalty rate across hospitals (%)") legend(cols(3)) title("Cross-sectional variation in overall penalty exposure" "across home health offices", size(medium))
graph export `gph'/csv_penalty_withinFY.eps, replace

* but the penalty exposure could increase because of just getting more referrals from the hospitals with mild penalty rather than hospitals getting more penalty -> hold constant the referral share weight at the 2012 level
* within-FY , distribution of penalty shock across offices
capture drop x fx*
kdensity prod_fixwgt if operate==1, nograph generate(x fx)
forval y = 2013/2015 {
  kdensity prod_fixwgt if fy==`y' & operate==1, nograph generate(fx`y') at(x)
  label var fx`y' "FY `y'"
}
line fx2013 fx2014 fx2015 x, sort ytitle(Density) xtitle("Weighted average of penalty rate across hospitals (%)") legend(cols(3)) title("Cross-sectional variation in overall penalty exposure" "across home health offices", size(medium)) note("I hold constant the referral share weights at the 2012 level.", size(small))
graph export `gph'/csv_penalty_withinFY2.eps, replace


*---------------

* cross-time variation within office

* Coefficient of variation in the penalty
capture drop x fx*
bys offid_nu: egen mean = mean(prod)
bys offid_nu: egen sd = sd(prod)
gen cov = sd / mean

preserve
keep offid_nu cov
duplicates drop
sum cov

histogram cov, freq title("Histogram of the coefficient of variation" "in overall penalty exposure across FY within office", size(medium)) yti(Number of offices) xtitle("Coefficient of variation")
graph export `gph'/tsv_penalty_withinoffice.eps, replace
restore

drop operate
outsheet using wgtpenalty.csv, replace comma names

*---------------
* for each office, how many hospitals on average does it get referrals from per year?
use `ijt', clear

*reshape long
forv y=2013/2015 {
  rename fy`y'readmissionpenalty penalty`y'
}

reshape long penalty adm_ijt adm_it wgt prod prod_fixwgt, i(offid_nu prvdr_num) j(fy)

*drop office-FY pairs where the office had zero admissions during the FY
drop if adm_it==0

*count only hospitals that gave >0 referrals per FY
gen cnt = adm_ijt > 0
bys offid_nu fy: egen nhosp = sum(cnt)

* mean # hospitals that referred to each office per FY, across offices
preserve
keep offid_nu fy nhosp
duplicates drop
tab fy, summarize(nhosp)
restore

* total # hospitals that have referred to any office
preserve
keep if cnt==1
keep prvdr_num
duplicates drop
count
restore

* how many patient admissions per office-referral hospital pair?
preserve
collapse (mean) adm_ijt, by(fy)
list
restore

*---------------
*office-referral hospital pair: does a hospital become subject to higher penalty pressure over time?
*restrict to FY 2013 - 2015 for which we have penalty rate data
keep if fy >= 2013 & fy <= 2015
sort offid_nu prvdr_num fy

*fy each office-FY, get IQR of the penalty exposure among hospitals
sort offid_nu fy
foreach l in "prod" "prod_fixwgt" {
  bys offid_nu fy: egen iqr_`l'_withinoy = iqr(`l')
}

*plot for each FY the kernel density of the IQR penalty exposure
preserve
collapse (mean) iqr_* prod prod_fixwgt penalty, by(offid_nu fy)
duplicates drop
list in 1/30
foreach l in "prod" "prod_fixwgt" {
  forval y = 2013/2015 {
    qui sum `l' if fy==`y', de
    loc med_`l'_`y': display %9.2f `r(p50)'
  }
}

loc l prod
kdensity iqr_`l'_withinoy, nograph generate(x fx)
forval y = 2013/2015 {
  kdensity iqr_`l'_withinoy if fy==`y', nograph generate(fx`y') at(x)
  label var fx`y' "FY `y'"
}
line fx2013 fx2014 fx2015 x, sort ytitle(Density) xtitle("IQR of penalty exposure among hospitals (%)") legend(cols(3)) title("Within-office variation in overall penalty exposure by FY", size(medium)) note("Note: To put things in perspective, the median penalty exposure among hospitals is" "`med_`l'_2013'% in FY 2013, `med_`l'_2014'% in FY 2014, `med_`l'_2015'% in FY 2015.", size(small))
graph export `gph'/csv_penalty_withinoffice.eps, replace

sum iqr*
capture drop x fx*
loc l prod_fixwgt
kdensity iqr_`l'_withinoy, nograph generate(x fx)
forval y = 2013/2015 {
  kdensity iqr_`l'_withinoy if fy==`y', nograph generate(fx`y') at(x)
  label var fx`y' "FY `y'"
}
line fx2013 fx2014 fx2015 x, sort ytitle(Density) xtitle("IQR of penalty exposure among hospitals (%)") legend(cols(3)) title("Within-office variation in overall penalty exposure by FY", size(medium)) note("Note: I hold constant the referral share weights at the 2012 level." "To put things in perspective, the median penalty exposure among hospitals is" "`med_`l'_2013'% in FY 2013, `med_`l'_2014'% in FY 2014, `med_`l'_2015'% in FY 2015.", size(small)) xscale(r(0(0.05)0.25)) xlab(0(0.05)0.25)
graph export `gph'/csv_penalty_withinoffice2.eps, replace

restore


* over time, the pressure from penalty from each hospital change for 2 reasons: the number of referrals from each hospital change & the penalty amount for the hosp change -> to control for the endogenous change in # referrals, use the first FY referrals


* take the maximum penalty or the sum across conditions?
