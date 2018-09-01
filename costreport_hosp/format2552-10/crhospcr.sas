options nocenter;
*Transpose to wide shape (multiple variables per report number) & merge alphanumeric & numeric data to create data on the ownership of HHAs for each hospital;
*Reference: hosp_chars_formedstat.sas
http://www.nber.org/docs/hcris/transpose_alpha.sas
http://www.nber.org/docs/hcris/transpose_nmrc.sas;

libname old "/home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport";
libname library "/home/hcmg/kunhee/Labor/Bayada_data";
x "cd /home/hcmg/kunhee/Labor/Bayada_data";

%macro loopalpha(fyear=,lyear=);
    %do year=&fyear. %to &lyear.;

        proc sort data=library.alpha&year. out=xx;
            by rpt_rec_num;

        proc print data=xx (obs=6);
            title "Year &year.";

        proc transpose data=xx out=library.alpha&year.t (drop = _NAME_);
            by rpt_rec_num;
            var alphnmrc_itm_txt;
            id varname;
        run;

        proc print data=library.alpha&year.t (obs=6);
        run;

    %end;
%mend;

%macro loopnmrc(fyear=,lyear=);
    %do year=&fyear. %to &lyear.;
        proc sort data=library.nmrc&year. out=xx;
            by rpt_rec_num;

        proc print data=xx (obs=6);

        proc transpose data=xx out=library.nmrc&year.t (drop = _NAME_) LET;
            by rpt_rec_num;
            var itm_val_num;
            id varname;
        run;

        proc print data=library.nmrc&year.t (obs=6);
        run;
    %end;
%mend;

* merge;
%macro loop(fyear=,lyear=);

%do year=&fyear. %to &lyear.;

* Work with the RPT file first;
%let rpt = rpt&year.;

data &rpt.;
    set old.hosp_rpt2552_10_&year.;

    *restrict to hospitals where Bayada operates;
    if prvdr_num eq "" then delete;
    *ownership status;
    length own_np 3.;
    own_np=0;
    length own_fp 3.;
    own_fp=0;
    length own_gv 3.;
    own_gv=0;
    if (prvdr_ctrl_type_cd>=1 and prvdr_ctrl_type_cd<=2) or prvdr_ctrl_type_cd=6 then own_np=1;
    if (prvdr_ctrl_type_cd>=3 and prvdr_ctrl_type_cd<=5) then own_fp=1;
    if (prvdr_ctrl_type_cd>=7 and prvdr_ctrl_type_cd<=13) then own_gv=1;
    if prvdr_ctrl_type_cd=. then delete;
run;

* Get rid of duplicates by report id;
proc sort nodupkey;
    by rpt_rec_num;

data merged;
    merge library.alpha&year.t (sortedby=rpt_rec_num) &rpt. (in=recent);
    by rpt_rec_num;
    fyear=&year.;
    if recent;

    data hcris&year.;
        merge library.nmrc&year.t (sortedby=rpt_rec_num) merged (in=recent);
        by rpt_rec_num;
        if recent;
    run;

    proc sort data=hcris&year. out=library.hospcr&year. ;
        by prvdr_num rpt_rec_num;

    proc print data=library.hospcr&year. (obs=6);
    proc means data=library.hospcr&year. median  ;
    options nolabel;
    proc means data=library.hospcr&year. ;
    options label;
    proc contents data=library.hospcr&year.;

    *stat-transfer;
    x "qrsh -now no st hospcr&year..sas7bdat hospcr&year._2552_10.dta -y";
%end;
%mend;

%loopalpha(fyear=2010,lyear=2017);
%loopnmrc(fyear=2010,lyear=2017);
%loop(fyear=2010,lyear=2017);

x "mv hospcr2012_2552_10.dta hospcr2012.dta";
x "mv hospcr2013_2552_10.dta hospcr2013.dta";
x "mv hospcr2014_2552_10.dta hospcr2014.dta";
x "mv hospcr2015_2552_10.dta hospcr2015.dta";
x "mv hospcr2016_2552_10.dta hospcr2016.dta";
x "mv hospcr2017_2552_10.dta hospcr2017.dta";
