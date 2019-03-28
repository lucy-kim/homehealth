#!/bin/bash
#$ -j y

cd /home/hcmg/kunhee/hrrp-home/codes

LOGFILE="/home/hcmg/kunhee/hrrp-home/output/filename_${JOB_ID}.log"

# Construct penalty salience data
stata-se do crreferral_share2012.do &>> "${LOGFILE}"
stata-se do crhrrp_penalty.do &>> "${LOGFILE}"
stata-se do HRRPpnlty_pressure_hj_2012.do &>> "${LOGFILE}"

# Construct other office or hospital characteristics data
stata-se do crother_covariates.do &>> "${LOGFILE}"

# Construct patient samples for analysis
stata-se do crHHeffort_visit.do &>> "${LOGFILE}"
stata-se do crresource_index.do &>> "${LOGFILE}"
stata-se do crepilvl_rehosp_smpl.do &>> "${LOGFILE}"

# Regression and descriptive analysis
stata-se do anHHeffort_readmit.do &>> "${LOGFILE}"
stata-se do desc_stats.do &>> "${LOGFILE}"
stata-se do anepilvl_rehosp_smpl2012.do &>> "${LOGFILE}"
