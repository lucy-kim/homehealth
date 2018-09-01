*options obs=55;
options nocenter ;
*Read hospital cost report alphanumeric file ;
*Reference: http://www.nber.org/docs/hcris/read_hosp_rpt_nmrc.sas ;

*  The following line should contain the directory
   where the SAS file is to be stored  ;

libname library "/home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport";
x "cd /home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport";

*  The following line should contain
   the complete path and name of the raw data file.
   On a PC, use backslashes in paths as in C:\  ;

%macro loop;
%do year=2010 %to 2017;
proc printto log  ="/home/hcmg/kunhee/Labor/read_hosp_rpt_nmrc&year..log";
proc printto print="/home/hcmg/kunhee/Labor/read_hosp_rpt_nmrc&year..lst";
FILENAME datafile "/home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport/hosp_nmrc2552_10_&year._long.csv";

*  The following line should contain the name of the SAS dataset ;

%let dataset = hosp_nmrc_rpt&year._long ;


data library.&dataset.;
*hosp_dm.* files report lengths;
LENGTH rpt_rec_num 4. wksht_cd $7. line_num $5. clmn_num $4. itm_val_num 7. ;
* '2C' is hexadecimal for decimal 44 which represents ',' ;
* '0D' is hexadecimal for decimal 13 which represents '\r', which is the carriage return character;
infile datafile dsd delimiter='2C0D'x  ;
INPUT

rpt_rec_num
wksht_cd  $
line_num  $
clmn_num  $
itm_val_num

;

proc means data=library.&dataset. n max;
title "Inspect maximums:  Four bytes retain six significant digits" ;
run;
proc freq data=library.&dataset.;
title "" ;
tables wksht_cd clmn_num ;
proc sort data=library.&dataset. nodupkey out = &dataset. ;
by wksht_cd line_num clmn_num  ;
proc print data=&dataset. noobs ;
var wksht_cd clmn_num line_num;

%end;
%mend;
%loop;
