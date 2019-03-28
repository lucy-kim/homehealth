# README for the home health effort project
This project examines the impact of hospital readmissions reduction program (HRRP) on downstream care providers' behaviors using proprietary home health data.

Files used for this project (data, codes, output) are stored on the [Wharton's HPC Cluster](https://research-it.wharton.upenn.edu/documentation/) in the directory: `/home/hcmg/kunhee/hrrp-home`.
  - Data are in `~/data`
  - Codes are in `~/codes`
  - Regression tables/other output files in `~/output`

## Data Sources

### Input data
All data files are in the directory `/home/hcmg/kunhee/hrrp-home/data`. Data file names are meant to be self-descriptive. I provide definitions for major variables in the data in a [data dictionary](https://drive.google.com/open?id=1xDtpwWqtwapdqyQj0SIBkwQtD0Cmm3G6fD6C07T91Y8).

#### Home health data from Bayada
1. Admission/episode-level data
  - `pats_hospreferred.dta`: Base set of patient admissions during 2012-2015 for which the end of episode is not truncated
  - `referralhosp_mcrID.dta`: Hospital referral source (with Medicare provider ID for the hospital) for each patient admission
  - `referral.dta`: Referral source (from any place, including hospital) for each patient admission (this file doesn't have Medicare provider IDs)
  - `inpat_dx.dta`: Inpatient diagnosis codes (ICD 9/10) for each admission; an admission can have multiple diagnosis codes associated and thus appear in multiple rows
  - `officeID_foradmitID.dta`: Crosswalk showing office ID for each admission ID
3. Visit-level data
  - `epi_visit.dta`: All visit-level information provided to each patient in each visit
4. Worker-level data
  - `payrate.dta`: Pay rate per visit for each worker-week
5. Office-level data
  - `office.dta`: Office information, e.g. location

#### Hospital penalties data
1. `hrrp_penalty.xlsx`: Atul constructed the 2012 HRRP penalties data for each hospital-condition (AMI, HF, Pneumonia) using the Medicare claims data.

#### Medicare Cost Report data
- `hospchars.dta`
- I used Medicare hospital cost report data for FY 2012-2015 available on the [CMS website](https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/Cost-Reports/Cost-Reports-by-Fiscal-Year.html).
- I modified the SAS programs shared on the [NBER HCRIS Data](https://www.nber.org/data/hcris.html) to extract the selected hospital characteristics of our interest.
- For the full SAS programs I used to construct hospital-level panel data on key hospital characteristics for FY 2000-2016, see [codes](https://www.dropbox.com/sh/tf79t9rf2onzrkf/AAAWxi2A9wS_WC8t8cbia7Ora?dl=0).

#### Hospitals' discharges to home health from Medicare inpatient claims data
- `CMShospdisch_tohh.dta`
- I aggregated hospital-week level counts of discharges to home health to get a total number of discharges to home health for each hospital in the baseline (FY 2012).
- The hospital-week level data came from CMS inpatient claims data offered by [Elena Andreyeva](https://sites.google.com/site/elenaandreyevaecon/).

### Intermediate data constructed by the below codes
- `referral_share2012.dta`: Office-hospital level data on the share of hospital-originating referrals from each hospital to each office during 2012
- `HRRPpnlty_pressure_hj_2012.dta`: Office-hospital level data on penalty salience (both main and alternative measures)
- `HHeffort_visit.dta`: Patient visit-level data before applying the sample restriction rules
- `resource_index.dta`: Data on home health costs (total and by subcategory) measuring resources spent for each episode-visit
- `allepi.dta`: Office-day level number of ongoing episodes
- `nw_active_worker.dta`: Office-day level number of active workers
- `hosp_chars_cr.dta`: Hospital-level data on hospital characteristics for FY 2012-2015
- `epilvl_rehosp_smpl.dta`: Patient episode-level data for FY 2012-2015 to be used for analysis
  - This is a **key analysis sample file** *(warning: drop 2012 before running the main regression analysis)*.
  - If one makes changes only in the regression analysis step but nothing in the data step, one can use this file and apply the change.

## Codes

Run the codes below in chronological order.

To run all the codes and replicate the work at once, one can submit a Shell script `run.sh` (located in `~/codes`). Running this will produce a single log file `filename_<JOB_ID>.log` (to be created in `~/output`) showing output of all the codes (i.e. do-files) run.

To submit the job, locate to `/home/hcmg/kunhee/hrrp-home/codes` and enter `qsub run.sh` (for detailed advice, see the [Wharton HPC website](https://research-it.wharton.upenn.edu/tools/stata/))

### Construct penalty salience data
1. `crreferral_share2012.do`
  - Create hospital-office-FY level number of referrals and share of hospital-originating referrals out of all referrals
2. `crhrrp_penalty.do`
  - Create 2012 penalty rate data constructed from Medicare claims data
3. `HRRPpnlty_pressure_hj_2012.do`
  - Compute penalty salience := product of share of the office j's patients that come from hosp h and h's penalty rate in 2012
  - Also compute alternative penalty salience measure used for robustness check

### Construct other office or hospital characteristics data
1. `crother_covariates.do`
  - Create data containing additional covariates: 1) total number of episodes going on, # active workers in the office on each day; 2) characteristics of the referring hospital

### Construct patient samples for analysis
1. `crHHeffort_visit.do`
  - Create visit-level data before measuring home health agencies' effort level for each patient
2. `crresource_index.do`
  - Create a measure of resources spent on each patient--i.e. a summary index of care intensity based on spending
3. `crepilvl_rehosp_smpl.do`
  - Create episode-level data for FY 2012-2015 to be used for main analysis

### Regression and descriptive analysis
1. `anHHeffort_readmit.do`
  - Main regression analysis with heterogeneity and robustness check analyses: examine the impact of the referring hospitals' HRRP penalty pressure on HHAs' efforts on patients
  - Produce regression results shown in **Table 2-6 and A3**
2. `desc_stats.do`
  - Produce descriptive statistics referenced in the script and shown in **Table 1, A1 Panel B**
3. `anepilvl_rehosp_smpl2012.do`
  - Analyze the difference in efforts spent between the healthiest (bottom quartile of severity score) and sickest (top quartile) people at baseline (2012) using the 2012 patient sample
  - Produce results shown in **Table A2**
