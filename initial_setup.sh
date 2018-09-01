# Create Stata data files from raw data files

cd /home/hcmg/kunhee/Labor/

#------------------------------------------------------------------------
# To build patient data using all available information from raw files
#------------------------------------------------------------------------

## convert client admission files files in CSV to Stata files
stata-se -q -b do crclientadmiss.do

## clean each individual data file from the client admission DB to make it ready for analysis
stata-se -q -b do clean_admiss.do

## convert additional client admission files (DB4)in CSV to Stata files
stata-se -q -b do crclientadmiss2.do

## clean each individual data file from the client admission DB4
stata-se -q -b do clean_admiss2.do

## convert additional client admission files (DB5)in CSV to Stata files
stata-se -q -b do crclientadmiss3.do

## clean each individual data file from the client admission DB5
stata-se -q -b do clean_admiss3.do

## convert client discharge OASIS files (DB6-DB8) in CSV to Stata files
stata-se -q -b do crclientdisch.do

## *clean each individual data file from the client discharge DB (A LITTLE DONE; TO BE COMPLETED LATER)
stata-se -q -b do clean_dc.do

## Create client recertification data showing episode counts in recertification for each client admission observation before merging with client admission base data
stata-se -q -b do crclientrecert.do

## create data on date of transfer to another facility or death before merging with client admission base data
stata-se -q -b do crdeaths.do

## create discharge date data before merging with client admission base data
stata-se -q -b do crdischrg.do

## create hospitalization data before merging with client admission base data
stata-se -q -b do crhosp.do

## create data on patient's ZIP code
stata-se -q -b do crclient_zip.do

## create data on recertifications and ROCs that each client had
stata-se -q -b do crrecert_roc.do

## Create a Stata file containing the inpatient discharge date for each admission from the raw CSV file
stata-se -q -b do crinpat_dcdate.do

## Create Stata data containing all inpatient diagnosis codes
stata-se -q -b do crinpat_dx.do

## use M1010 client's first inpatient diagnosis code (ICD-9) & tag conditions subject to the Hospital Readmissions Reduction Program
stata-se -q -b do crHRRPdiag.do

## create Charlson Index and Elixhauser index by looking at the ICD-9-CM codes for home health care diagnosis & inpatient diagnosis (for patients with prior inpatient stays) in the patient data (referred to crcharlson_comorbid.do)
stata-se -q -b do crcomorbidity.do

## Create an episode-level data showing the office ID and total visit counts that occurred for each episode from the raw CSV file
stata-se -q -b do crfirst_last_vd.do

## create a Stata data on payer source (i.e. insurance plan IDs) from the CSV files
stata-se -q -b do crplanid.do

## create referral source data for each admission
stata-se -q -b do crreferral.do

## manually change the hospital name in the referral source data
stata-se -q -b do crpost_hncorrect.do

## get Medicare ID for each hospital that appears in the patient referral source data
stata-se -q -b do crreferralhosp_mcrID.do

#-------- Now merging patient data -----

## Create base patient episode-admission level data from the raw data
stata-se -q -b do crmasterclientadmiss2.do

## link the episode-admission level data with hospitalization date data
qsub -m e link_epi_hosp.sh

## create episode-admission-level data containing patient information e.g. race, for each admission
stata-se -q -b do crclient_chars.do

## add OASIS patient characteristics / comorbidity variables & other outcome variables
stata-se -q -b do crclient_chars2.do

# ---------------------------------------
# prepare patient episode-visit-level data
# ---------------------------------------

## merge the patient episode-hopsitalization date-admission-level data with the visit-level data -> beforehand, will have to collapse the patient data to episode-admission level data
stata-se -q -b do crepi_visit.do

# ---------------------------------------
# prepare worker-day-level data containing visit information, employment information
# ---------------------------------------

## Create daily panel of each worker who ever works in our sample from 2012 - Q3 2015
stata-se -q -b do crdaily_workerpanel.do

# ---------------------------------------
# prepare office-day-level data
# ---------------------------------------

## create office-day level data showing employment: # new hires, # quitters (voluntary, involuntary), # total workers on each day; demand: # visits in each discipline, # new episodes, # existing episodes, # discharged episodes on each day
stata-se -q -b do crdaily_officepanel.do

# ---------------------------------------
# prepare xwalk data on office ID for each admission ID & episode ID
# ---------------------------------------
## create episode-level data that contain office ID and visit counts
stata-se -q -b do crepisode_visitcnt_offid.do

##create office ID for each admission ID (i.e.clientID-SOC date pair)
crofficeID_foradmitID.do

# ---------------------------------------
# prepare data all episode IDs contained in each admission ID
# ---------------------------------------
## create xwalk data all episode IDs contained in each admission ID
crepiid_admitID.do

# ---------------------------------------
# prepare visit-level data
# ---------------------------------------

cd /home/hcmg/kunhee/Labor/

## create visit data from CSV files
stata-se -q -b do crvisit.do

## create visit service code & time data for each visit from CSV files
stata-se -q -b do crvisit_svccode_time.do

## merge the visit-level data with visit service code-level data that contain visit code & time
stata-se -q -b do crvisit_codelevel.do

## link the visit-level data with episode-level data that contain visit counts and office ID by episode ID (old file name = link_staff_visit_office.do)
stata-se -q -b do crstaff_visit_office.do


# ---------------------------------------
# prepare worker-day-level data
# ---------------------------------------

## create clinical staff data for all workers, all services, time periods from a CSV file
stata-se -q -b do crstaff.do

## create a stata dataset using the HCHB payroll data which contains the various information about workers (this may be a cross-sectional data)
stata-se -q -b do crhchb_worker.do

## create a list of unique workers that appear in the visit-level data
stata-se -q -b do cruniqworker.do

## create cross-sectional staff characteristics data with (one obs per worker) using the HCHB worker-level data and staff.dta
stata-se -q -b do crstaff_chars.do

## create xwalk data from payrollno to worker ID in the historical employment status / pay rate / quota data
stata-se -q -b do crpayrollno_workerIDes_xwalk.do

## create daily data showing each worker's employment arrangement on that day
stata-se -q -b do crempl_status_weekly.do
# # this do file contains the following do files:
# crproductivity_quota_daily.do
# crjobtitle_chng_daily.do

## create weekly pay data for each worker-week for salaried workers (later merge these data with the worker-week-office-discipline-level pay data by payrollno)
stata-se -q -b do crpay_byww_sa.do

## create daily data showing the productivity quota for each worker & keep only the workers I can match with the workers appearing in the visit-level data
crproductivity_weekly.do

## create data for each worker's historical pay rate
stata-se -q -b do crpayrate.do


## create weekly pay data for each worker-week-office-discipline (previously named crpay_byworker_byweek.do)
stata-se -q -b do crpay_bywwod.do


# ---------------------------------------
# prepare visit-level data containing employment information of the worker who provides each visit
# ---------------------------------------

## create visit-level data with worker characteristics (employment status, job title as of the visit date) attached
stata-se -q -b do crvisit_worker_chars.do

## create a visit-level data containing workers weekly pay, weekly number of visit points worked, employmenet status, productivity quota, job title for each worker-visit
stata-se -q -b do crvisit_pay.do (not updated after the new visit_worker_chars.dta created)

# ---------------------------------------
# prepare office-level data
# ---------------------------------------
## create office-level data (note that this is snapshot data) that contains all the offices that have been in business and contain xwalk between alphabet office ID (offid0, offid) and numeric office ID (offid_nu)
croffice.do


*create worker pay rate data
do crpayrate

## create admission-level data on patients referred by hospitals
stata-se -q -b do crpats_hospreferred.do
