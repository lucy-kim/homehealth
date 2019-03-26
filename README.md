# README for the home health projects
This project explores topics around hospitals and home health care providers using proprietary data on the home health operations from a large multi-state home health care company operating 100+ offices. We are currently investigating the impact of hospital readmissions penalty program on downstream care providers' behaviors using these home health data.

Files used for this project (data, codes) are stored on the [Wharton's HPC Cluster](https://research-it.wharton.upenn.edu/documentation/), and specifically in the directory: `~/Labor`.

## Data Sources

All data files are in the directory `~/Labor/Bayada_data`. Data file names are meant to be self-descriptive. I provide definitions for major variables in the data in a [data dictionary](https://drive.google.com/open?id=1xDtpwWqtwapdqyQj0SIBkwQtD0Cmm3G6fD6C07T91Y8).

### Home health data from Bayada
1. Admission-level data
  - Master patient admission data on basic characteristics for 2012-2015: `pats_hospreferred.dta`
  - Referral source data: `referralhosp_mcrID.dta`
2. Episode-level data (an admission can span multiple episodes)
  - `epi_visit.dta`
3. Visit-level data

### Hospital penalties data
Atul constructed the 2012 HRRP penalties data for each hospital-condition (AMI, HF, Pneumonia) using the Medicare claims data.

### Medicare Cost Report data
I used

### Hospitals' discharges to home health from Medicare inpatient claims data
I construct hospitals'  from CMS inpatient claims data provided by Elena.
`CMShospdisch_tohh.dta`

hospital-week level counts of Medicare hospital discharges to home health destinations for 2011-2014 (Source: Elena's CMS data)

## Project codes

The codes below are in chronological order.

### Create Stata data files from raw home health data files

`initial_setup.sh`
- Skip this step because they have all been run; this is just for reference to show all the raw data files that can be potentially used (Not all these data were used for this project.)
- Code files listed here are not shared on this repository because of no need to run again

### Load data from other sources used for the project

`CMShospdisch_tohh.do`
- use hospital-week level counts of Medicare hospital discharges to home health destinations for 2011-2014 (Source: Elena's CMS data on inpatient discharge claims)

`costreport_hosp > format2552-10 > hospcr.sh` and then `crhosp_chars_cr.do`
- Create cost report data for hospital characteristics

### Create intermediary data files with useful variables and analysis samples

`crpats_hospreferred.do`
- create admission level data on patients referred by hospitals only and restrict to patients whose discharge/hospitalization dates are not right truncated due to the sample period limitation

`HRRPpnlty_pressure_hj_2012.do`
- compute penalty salience: use 2012 data, product of share of the office j's patients that come from hosp h and h's penalty rate
- also compute alternative penalty salience measure used for robustness check

`crHHeffort_visit.do`
- create visit-level data before measuring home health agencies' effort level for each patient

`crresource_index.do`
- create a measure of resources spent on each patient--i.e. a summary index of care intensity based on spending

`crother_covariates.do`
- create data containing additional covariates: 1) total number of episodes going on, # active workers in the office on each day; 2) characteristics of the referring hospital

`crepilvl_rehosp_smpl.do`
- create episode-level data starting for fy 2013-2015 where fy is a year ending June

### Analysis files
`anHHeffort_readmit.do`
- Main regression analysis with heterogeneity and robustness check analyses: examine the impact of the referring hospitals' HRRP penalty pressure on HHAs' efforts on patients

`desc_stats.do`
- produce descriptive stats

`crepilvl_rehosp_smpl2012.do`
- create 2012 (baseline) patient sample to interpret the magnitude of our estimates, want to compare the effort levels on the healthiest and sickest people at baseline (2012)

`anepilvl_rehosp_smpl2012.do`
- analyze the difference in efforts spent between the healthiest (bottom quartile of severity score) and sickest (top quartile) people at baseline (2012) using the 2012 patient sample created above
