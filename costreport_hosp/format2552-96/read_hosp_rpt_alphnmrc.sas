*options obs=55;
options nocenter;
*Read hospital cost report alphanumeric file ;
*Reference: http://www.nber.org/docs/hcris/read_hosp_rpt_alphnmrc.sas ;

*  The following line should contain the directory
   where the SAS file is to be stored  ;

libname library "/home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport";
x "cd /home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport";

*  The following line should contain
   the complete path and name of the raw data file.
   On a PC, use backslashes in paths as in C:\  ;

%macro loop;
%do year=2000 %to 2011;
proc printto log  ="/home/hcmg/kunhee/Labor/read_hosp_rpt_alphnmrc&year..log";
proc printto print="/home/hcmg/kunhee/Labor/read_hosp_rpt_alphnmrc&year..lst";
FILENAME datafile "/home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport/hosp_alpha2552_96_&year._long.csv";

*  The following line should contain the name of the SAS dataset ;
%let dataset=hosp_alpha&year._long ;

data library.&dataset.;

*Variable           Maximum
---------           -------
RPT_REC_NUM         64331
WKSHT_CD            "S89R000"
LINE_NUM            "09919"  (min="00000". "00100" I think means "1". "00101" can mean "101" or "1.01", depending on the worksheet)
CLMN_NUM            "0600" (means "6", I think.   Values are 0000, 0100, 0200, 0300, 0400, 0500, and 0600)
;
length wksht_cd $7. line_num $5. clmn_num $4. alphnmrc_itm_txt $45. default = 4  ;
* '2C' is hexadecimal for decimal 44 which represents ',' ;
* '0D' is hexadecimal for decimal 13 which represents '\r', which is the carriage return character;
infile datafile dsd delimiter='2C0D'x  ;
INPUT
rpt_rec_num
wksht_cd $
line_num $
clmn_num $
alphnmrc_itm_txt $
;

proc means data=library.&dataset. max n;
title "Inspect maximums:  Four bytes will retain six significant digits" ;
title2 "&dataset.";
proc freq data=library.&dataset.;
title "";
tables WKSHT_CD LINE_NUM CLMN_NUM ;
proc contents data=library.&dataset.;
data &dataset.;
set library.&dataset.;
proc sort data=&dataset. nodupkey;
by wksht_cd clmn_num line_num  ;
proc print data=&dataset. noobs;
var wksht_cd clmn_num line_num;

%end;
%mend;
%loop;
