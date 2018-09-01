options nocenter ;
*Read hospital cost report RPT file ;
*Reference: http://www.nber.org/docs/hcris/read_hosp_rpt.sas ;

libname library "/home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport";
x "cd /home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport";

PROC FORMAT CNTLOUT=library.fhosp_rpt;
**************************************************;
** The PROC FORMAT statements will store the formats in a sas data set;
** Use the stored formats in subsequent programs like this: ;
** proc format cntlin=library.fhosp_rpt;
** PROC freq;
**        tables adr_vndr_cd util_cd ;
**        format adr_vndr_cd adr_vndr_cd.;
**        format util_cd $util_cd.;
** Consult the SAS Procedures Guide PROC FORMAT section;
**************************************************;
VALUE prvdr_ctrl_type_cd
       1     =  "Voluntary Nonprofit, Church"
       2     =  "Voluntary Nonprofit, Other"
       3     =  "Proprietary, Individual"
       4     =  "Proprietary, Corporation"
       5     =  "Proprietary, Partnership"
       6     =  "Proprietary, Other"
       7     =  "Governmental, Federal"
       8     =  "Governmental, City-County"
       9     =  "Governmental, County"
       10    =  "Governmental, State"
       11    =  "Governmental Hospital District"
       12    =  "Governmental, City"
       13    =  "Governmental, Other"
;
VALUE rpt_stus_cd
       1     =  "As Submitted"
       2     =  "Settled w/o Audit"
       3     =  "Settled with Audit"
       4     =  "Reopened"
;
VALUE $initl_rpt_sw
       "Y"   =  "first cost report filed for this provider"
       "N"   =  "2nd+ report for this provider"
;
VALUE $last_rpt_sw
       "Y"   =  "last cost report filed for this provider"
       "N"   =  "not last report for this provider"
;
VALUE adr_vndr_cd
       2     =  "E & Y"
       3     =  "KPMG"
       4     =  " HFS"
;
VALUE $util_cd
       "L"   =  "Low Medicare Util"
       "N"   =  "No Medicare Util"
       "F"   =  "Full Medicare Util"
;

*  The following line should contain
   the complete path and name of the raw data file.
   On a PC, use backslashes in paths as in C:\  ;

%macro loop;
%do year=2010 %to 2017;
/* proc printto log  ="/home/hcmg/kunhee/Labor/read_hosp_rpt&year..log";
proc printto print="/home/hcmg/kunhee/Labor/read_hosp_rpt&year..lst"; */

FILENAME datafile "/home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport/hosp_rpt2552_10_&year..csv";

*  The following line should contain the name of the SAS dataset ;

%let dataset=library.hosp_rpt2552_10_&year.;

data &dataset.;

*hosp_dm.* files report lengths;
*Using a length of 4 bytes retains 6 significant digits;
*Largest integer represented exactly is 2,097,152;
*Maximum values apply to 2002-09-30 data file;
*max date is around 16000, do length of 4 should be fine for dates;
*Variable           Maximum
---------           -------
RPT_REC_NUM         64331     Primary Key / Unique ID
PRVDR_CTRL_TYPE_CD  "13"
PRVDR_NUM           "660001"
RPT_STUS_CD         "4"
INITL_RPT_SW        "Y"
LAST_RPT_SW         "Y"
TRNSMTL_NUM         "8"
FI_NUM              "77002"
ADR_VNDR_CD         "4"
UTIL_CD             "F"
SPEC_IND            "Y"
;
LENGTH
  rpt_rec_num 5 prvdr_ctrl_type_cd 3 prvdr_num $7
  rpt_stus_cd 3 initl_rpt_sw $1 last_rpt_sw $1
  trnsmtl_num $1 fi_num $5 adr_vndr_cd 3 util_cd $1 spec_ind $1
default = 4;
* '2C' is hexadecimal for decimal 44 which represents ',' ;
* '0D' is hexadecimal for decimal 13 which represents '\r', which is the carriage return character;
infile datafile dsd delimiter='2C0D'x  ;
**  the ":" is a format modifier that reads data values
    that need additional instructions from an informat;
INPUT
rpt_rec_num
prvdr_ctrl_type_cd $
prvdr_num $
npi $
rpt_stus_cd
fy_bgn_dt : mmddyy10.
fy_end_dt : mmddyy10.
proc_dt : mmddyy10.
initl_rpt_sw $
last_rpt_sw $
trnsmtl_num $
fi_num $
adr_vndr_cd
fi_creat_dt : mmddyy10.
util_cd $
npr_dt : mmddyy10.
spec_ind $
fi_rcpt_dt : mmddyy10.
;
LABEL
rpt_rec_num="Report Record Number"
prvdr_ctrl_type_cd ="Provider Control Type Code"
prvdr_num ="Provider Number"
npi="National Provider Identifier"
rpt_stus_cd="Report Status Code"
fy_bgn_dt="Fiscal Year Begin Date"
fy_end_dt="Fiscal Year End Date"
proc_dt  ="HCRIS Process Date"
initl_rpt_sw="Initial Report Switch"
last_rpt_sw="Last Report Switch"
trnsmtl_num="Transmittal Number"
fi_num ="Fiscal Intermediary Number"
adr_vndr_cd="Automated Desk Review Vendor Code"
fi_creat_dt="Fiscal Intermediary Create Date"
util_cd    ="Utilization Code"
npr_dt     ="Notice of Program Reimbursement Date"
spec_ind="Special Indicator"
fi_rcpt_dt="Fiscal Intermediary Receipt Date"
fy="Fiscal year (NBER-computed from fy_bgn_dt)"
;
FORMAT
fy_bgn_dt   MMDDYYS10.
fy_end_dt   MMDDYYS10.
proc_dt     MMDDYYS10.
fi_creat_dt MMDDYYS10.
npr_dt      MMDDYYS10.
fi_rcpt_dt  MMDDYYS10.
;
proc sort data=&dataset.;
by rpt_rec_num;

proc means DATA=&dataset. max n;
title "Inspect maximums:  Using a length of 4 bytes (default) retains 6 significant digits" ;
proc freq DATA=&dataset. ;
title "" ;
tables prvdr_ctrl_type_cd rpt_stus_cd initl_rpt_sw last_rpt_sw trnsmtl_num fi_num adr_vndr_cd util_cd spec_ind ;

FORMAT
prvdr_ctrl_type_cd prvdr_ctrl_type_cd.
rpt_stus_cd rpt_stus_cd.
initl_rpt_sw $initl_rpt_sw.
last_rpt_sw $last_rpt_sw.
adr_vndr_cd adr_vndr_cd.
util_cd $util_cd.
;
proc print DATA=&dataset. (obs=6);
proc contents DATA=&dataset.;

%end;
%mend;
%loop;
