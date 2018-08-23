# README for the home health projects
This project explores topics around hospitals and home health care providers using proprietary data on the home health operations from a large multi-state home health care company operating 100+ offices. We are currently investigating the impact of hospital readmissions penalty program on downstream care providers' behaviors using these home health data.

The codes below are in chronological order.

## Create Stata data files from raw data files
**data step (Create a do-file of do files)**

`crofficeID_foradmitID.do`
- create office ID for each admission ID (i.e. clientID-SOC date pair)

`crreferral.do`
- create referral source data for each admission from the raw CSV file

`crpost_hncorrect.do`
- manually change the hospital name in the referral source data

`CMShospdisch_tohh.do`
- use hospital-week level counts of Medicare hospital discharges to home health destinations for 2011-2014 (Source: Elena's CMS data on inpatient discharge claims)

`crreferralhosp_mcrID.do`
- get Medicare ID for each hospital that appears in the patient referral source data

`crhrrp_penalty.do`
- create readmissions penalty data for each hospital, by condition & overall

`pred_pprob.do`
- create data from Atul's predicted penalty probability for 2012 and for 2013

## Create intermediary data files with useful variables and analysis samples
`HRRPpnlty_pressure_hj_2012.do`
- compute penalty pressure: use 2012 data, product of share of the office j's patients that come from hosp h and h's penalty rate

`anchng_chars_pat2HH.do`
- investigate the potential changes over time in the characteristics of patients discharged to home health

`crresource_index.do`
- create a measure of resources spent on each patient--i.e. a summary index of care intensity based on spending

`crepilvl_rehosp_smpl.do`
- create episode-level data starting for fy 2013-2015 where fy is a year ending June

## Analysis files
`anHHeffort_readmit.do`
- Main regression analysis with counterfactuals: examine the impact of the referring hospitals' HRRP penalty pressure on HHAs' efforts on patients

`desc_stats.do`
- produce descriptive stats

`crepilvl_rehosp_smpl2012.do`
- create 2012 (baseline) patient sample to interpret the magnitude of our estimates, want to compare the effort levels on the healthiest and sickest people at baseline (2012)

------ old files below







`anvar_hrrppenalty.do`
- how much pressure from hospitals does each office get?
- within-office over-time variation in incentive/pressure for hospitals to decrease readmissions

`anchng_chars_pat2HH.do`
- investigate the potential changes over time in the characteristics of patients discharged to home health

`craha.do`
- construct AHA data for each hospital-year that contain strict and loose integration measures, system ID

`referralsh_byHRRPpenalty_VI.do`
- test if the referral share has declined because hospital facing high penalty shifting their referrals to in-house home health care provider

`HHeffort.do`
- examine the impact of the referring hospitals' HRRP penalty pressure on HHAs' efforts on patients

`HHeffort2.do`
- compare HH efforts by whether patient was before or after 30 days from hospital discharge, penalty pressure, or HRRP or non-HRRP condition

`HHeffort_exper_visitorder.do`
- add more measures of home health office's efforts at the patient episode level: mean experience of nurses serving patients, order of nurse's visit during the day

`crHHeffort_week.do`
- create patient-week level data containing referring hospital's penalty pressure, some initial set of effort measures and covariates

`HHeffort3.do`
- Run regression analysis using the patient-week level data to compare HH efforts by (whether patient was before or after 30 days from hospital discharge), penalty pressure, or HRRP or non-HRRP condition

`HHeffort5.do`
- Compare HH efforts, readmissions (Reduced form model) by penalty pressure & TA vs MA patients (don't differentiate by penalty vs non-penalty condition and use the overall penalty pressure)

`crresource_index.do`
- create a measure of resources spent on each patient--i.e. a summary index of care intensity based on spending

`crepi_lvl_rehosp_smpl.do`
- create episode-level data starting for fy 2013-2015 where fy is a year ending June

`HHeffort4.do`
- Compare HH efforts, readmissions (Reduced form model) by penalty pressure & HRRP or non-HRRP condition & TA vs MA patients

`HHeffort6.do`
- Remove cardiorespiratory conditions from the control group; run regs for P(Readmission) in the first week as outcome; and replace the 2012 penalty rate with the 2012 predicted probability of penalty

`desc_stats.do`
- Run descriptive stats

`inpat_firstdx.do`
*keep only the first recorded inpat diagnosis code per patient (reference: crinpat_dx.do)

`crepilvl_rehosp_smpl_firstdx.do`
- reconstruct episode level data after classifying the target and non-target (excl. cardiorespiratory from non-target) conditions using only the first inpat diagnosis code

*---------------------
## absolutely needed files
`crCMShospdisch_tohh.do`
  - use hospital-week level counts of Medicare hospital discharges to home health destinations for 2011-2014 (Source: Elena's CMS data)
`crpred_pprob.do`
`HRRPpnlty_pressure_hj_2012.do`
`anchng_chars_pat2HH.do`
`crresource_index.do`
`crepilvl_rehosp_smpl.do`
`anHHeffort_readmit.do`
`inpat_firstdx.do`
`crepilvl_rehosp_smpl_firstdx.do`
`anHHeffort_readmit_recode_targetcond.do`
`desc_stats.do`
`crepilvl_rehosp_smpl2012.do`
