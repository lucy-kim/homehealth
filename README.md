# README for the home health projects
This project explores topics around hospitals and home health care providers using rich proprietary data on the home health operations from a large multi-state company of 100+ offices. We are currently investigating the impact of hospital readmissions penalty program on downstream care providers' behaviors using home health data.

The codes below are in chronological order.

`crreferralhosp_mcrID.do`
- get Medicare ID for each hospital that appears in the patient referral source data

`crhrrp_penalty.do`
- create readmissions penalty data for each hospital, by condition & overall

`link_referralhosp_penalty.do`
- link the admission-level hospital referral source data with readmissions penalty data by the Medicare hospital ID

`anvar_hrrppenalty.do`
- how much pressure from hospitals does each office get?
- within-office over-time variation in incentive/pressure for hospitals to decrease readmissions

`anchng_chars_pat2HH.do`
- investigate the potential changes over time in the characteristics of patients discharged to home health

`craha.do`
- construct AHA data for each hospital-year that contain strict and loose integration measures, system ID

`referralsh_byHRRPpenalty_VI.do`
- test if the referral share has declined because hospital facing high penalty shifting their referrals to in-house home health care provider

`HRRPpnlty_pressure_hj_2012.do`
- compute penalty pressure: use 2012 data, product of share of the office j's patients that come from hosp h and h's penalty rate

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

`HHeffort4.do`
- Compare HH efforts, readmissions (Reduced form model) by penalty pressure & HRRP or non-HRRP condition & TA vs MA patients

`HHeffort5.do`
- Compare HH efforts, readmissions (Reduced form model) by penalty pressure & TA vs MA patients (don't differentiate by penalty vs non-penalty condition and use the overall penalty pressure)

`crresource_index.do`
- create a measure of resources spent on each patient--i.e. a summary index of care intensity based on spending
