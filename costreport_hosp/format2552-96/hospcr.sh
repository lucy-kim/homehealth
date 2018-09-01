#!/bin/bash
#$ -N sasjob01
#$ -j y
#$ -m e
# extract from the CMS hospital cost report data on hospital-based PACs and other hospital characteristics

cd /home/hcmg/kunhee/Labor

# Download raw data
# sas -nodms -noterminal download_hospcr.sas

# Read Hospital cost report main, numeric, alphanumeric files
# sas -nodms -noterminal read_hosp_rpt.sas
# sas -nodms -noterminal read_hosp_rpt_nmrc.sas
# sas -nodms -noterminal read_hosp_rpt_alphnmrc.sas

# Extract some variables from the numeric, alpha file
sas -nodms -noterminal extract_hosp_nmrc.sas
# sas -nodms -noterminal extract_hosp_alpha.sas

# Transpose to wide shape (multiple variables per report number) & merge alphanumeric & numeric data to create Hospital-level data
sas -nodms -noterminal crhospcr.sas

# # Create hospital panel data for each calendar year
stata-se -q -b do crhospcr_panel_1996_2011.do

# # # For hospital-CY, get vertical integration measures: dummy for VI, mix of PACs if VI=1, # providers per PAC type
# stata-se -q -b do hospvi_measure.do
# #
# # # Delete intermediate data files
# cd /home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport
# rm -f *.csv
# rm -f *.sas7bdat
#
# cd /home/hcmg/kunhee/Labor/Bayada_data
# rm -f alpha*.sas7bdat
# rm -f nmrc*.sas7bdat
# rm -f hospcr201?.sas7bdat
# gzip hospcr201?.dta
