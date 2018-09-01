options nocenter;

libname old "/home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport";
libname library "/home/hcmg/kunhee/Labor/Bayada_data";
x "cd /home/hcmg/kunhee/Labor/Bayada_data";

/* proc contents data= library.nmrc2011;
  proc freq data= library.nmrc2011 ;
    table varname;
run; */

%let year = 2011;

proc sort data=library.nmrc&year. out=xx;
    by rpt_rec_num;

proc print data=library.nmrc&year. (where=( WKSHT_CD EQ "E00A18A" and CLMN_NUM="0010" and LINE_NUM="03300"));
/*
proc transpose data=xx out=library.nmrc&year.t (drop = _NAME_);
    by rpt_rec_num;
    var itm_val_num;
    id varname;
run;

proc print data=library.nmrc&year.t (obs=6);
run; */
