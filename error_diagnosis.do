*code to figure out the difference in sample composition before and after correcting the error in importing the 2012 penalty data

*for analysis sample for 2013-2015
*correct patient-level data: epilvl_rehosp_smpl.dta
*wrong patient-level data: epilvl_rehosp_smpl2.dta
*restrict to TM patients and run regression to drop missing values for the final analysis data used

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc reg /home/hcmg/kunhee/Labor/regresults
cd `path'

loc riskhosp riskhosp_fall riskhosp_manyhos riskhosp_mental riskhosp_ge5 riskhosp_oth
loc priorcond priorcond_dis priorcond_impd priorcond_cath priorcond_pain priorcond_mem priorcond_inc
loc hrfactor hrfactor_alco hrfactor_drug hrfactor_smoke hrfactor_obese
loc overallst overallst_vbad overallst_bad overallst_tem
loc ins dual
loc ages i.age5yr
loc demog `ages' female white noassist livealone `ins'
loc comorbid ynch* `overallst' `hrfactor' `priorcond'
loc hospchars vi_hha teaching urban own_* i.size
loc officechars lnallepi lnnw_active_w

loc sp `riskhosp' `demog' `comorbid' i.fy `officechars' `hospchars'
loc pp1 ami hf pn pnltprs_c_X_ami pnltprs_c_X_hf pnltprs_c_X_pn

*correct data
loc x 1
use epilvl_rehosp_smpl, clear
keep if tm==`x'

drop if copd | stroke | (cardioresp & hrrpcond==0)
tab hrrpcond
assert hrrpcond_count <2

loc n 1
loc yv lnlov
areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)
keep if e(sample)

keep epiid prvdr_num
duplicates drop

tempfile correct_tm
save `correct_tm'

*wrong data
loc x 1
use epilvl_rehosp_smpl2, clear
keep if tm==`x'

drop if copd | stroke | (cardioresp & hrrpcond==0)
tab hrrpcond
assert hrrpcond_count <2

loc n 1
loc yv lnlov
areg `yv' `pp`n'' `sp', absorb(offid_nu) vce(cluster offid_nu)
keep if e(sample)

keep epiid prvdr_num hrrpcond
duplicates drop

tempfile wrong_tm
save `wrong_tm'

*--------------------------------
*compare intersection and non-intersection
use `correct_tm', clear
merge 1:1 epiid prvdr_num using `wrong_tm'

*--------------------------------
*for the baseline sample for 2012

*correct data
