# README for the home health projects
This project explores topics around hospitals and home health care providers using rich proprietary data on the home health operations from a large multi-state company of 100+ offices. We are currently investigating the impact of hospital readmissions penalty program on downstream care providers' behaviors using home health data.

The codes below are in chronological order.

**crreferralhosp_mcrID.do**
- get Medicare ID for each hospital that appears in the patient referral source data

**crhrrp_penalty.do**
- create readmissions penalty data for each hospital, by condition & overall

**link_referralhosp_penalty.do**
- link the admission-level hospital referral source data with readmissions penalty data by the Medicare hospital ID

**anvar_hrrppenalty.do**
- how much pressure from hospitals does each office get?
- within-office over-time variation in incentive/pressure for hospitals to decrease readmissions

**anchng_chars_pat2HH.do**
- investigate the potential changes over time in the characteristics of patients discharged to home health

**craha.do**
- construct AHA data for each hospital-year that contain strict and loose integration measures, system ID

**referralsh_byHRRPpenalty_VI.do**
- test if the referral share has declined because hospital facing high penalty shifting their referrals to in-house home health care provider

**HRRPpnlty_pressure_hj_2012.do**
- compute penalty pressure: use 2012 data, product of share of the office j's patients that come from hosp h and h's penalty rate

**HHeffort.do**
- examine the impact of the referring hospitals' HRRP penalty pressure on HHAs' efforts on patients

**HHeffort2.do**
- compare HH efforts by whether patient was before or after 30 days from hospital discharge, penalty pressure, or HRRP or non-HRRP condition
