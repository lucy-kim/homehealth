# README for the home health projects
This project explores topics around hospitals and home health care providers using proprietary data on the home health operations from a large multi-state home health care company operating 100+ offices. We are currently investigating the impact of hospital readmissions penalty program on downstream care providers' behaviors using these home health data.

## Location of data files
Files used for this project are stored in the [Wharton's HPC Cluster](https://research-it.wharton.upenn.edu/documentation/), and specifically in the directory: `~/Labor`

All data files are in the directory `~/Labor/Bayada_data`. All the processed data and raw files contained in subdirectories with descriptive names (e.g. all the raw CSV files containing information on home health patients are in the subdirectory `client_CSV`)

## Project codes

The codes below are in chronological order.

### Create Stata data files from raw home health data files

`initial_setup.sh`
- All the codes used in this shell script available in `/home/hcmg/kunhee/Labor`.
- Skip this step because they have all been run; this is just for reference to show all the raw data files that can be potentially used. (Not all these data were used for this project.)

### Load data from other sources used for the project

`CMShospdisch_tohh.do`
- use hospital-week level counts of Medicare hospital discharges to home health destinations for 2011-2014 (Source: Elena's CMS data on inpatient discharge claims)

`crhrrp_penalty.do`
- create readmissions penalty data for each hospital, by condition & overall

`pred_pprob.do`
- create data from Atul's predicted penalty probability for 2012 and for 2013

`costreport_hosp > format2552-10 > hospcr.sh` and then `crhosp_chars_cr.do`
- Create cost report data for hospital characteristics

### Create intermediary data files with useful variables and analysis samples

`crpats_hospreferred.do`
- create admission level data on patients referred by hospitals only and restrict to patients whose discharge/hospitalization dates are not right truncated due to the sample period limitation

`HRRPpnlty_pressure_hj_2012.do`
- compute penalty pressure: use 2012 data, product of share of the office j's patients that come from hosp h and h's penalty rate

`crHHeffort_visit.do`
- create visit-level data before measuring home health agencies' effort level for each patient

`crresource_index.do`
- create a measure of resources spent on each patient--i.e. a summary index of care intensity based on spending

`crepilvl_rehosp_smpl.do`
- create episode-level data starting for fy 2013-2015 where fy is a year ending June

### Analysis files
`anHHeffort_readmit.do`
- Main regression analysis with counterfactuals: examine the impact of the referring hospitals' HRRP penalty pressure on HHAs' efforts on patients

`desc_stats.do`
- produce descriptive stats

`crepilvl_rehosp_smpl2012.do`
- create 2012 (baseline) patient sample to interpret the magnitude of our estimates, want to compare the effort levels on the healthiest and sickest people at baseline (2012)
