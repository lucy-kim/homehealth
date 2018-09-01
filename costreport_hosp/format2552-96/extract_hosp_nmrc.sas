options nocenter ;
*Extract some variables from the HHA ALPHA file from CMS HHA cost report data;
*Reference: hosp_chars_formedstat.sas
http://www.nber.org/docs/hcris/extract_nmrc2552-10.sas;

libname library "/home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport";
x "cd /home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport";
libname out "/home/hcmg/kunhee/Labor/Bayada_data";

%macro loop (fyear=,lyear=);

%do year=&fyear. %to &lyear.;

    %let dataset=nmrc&year.;

    proc sql;
       **select appropriate worksheets;
       create table &dataset. as
       select *
       from library.hosp_nmrc_rpt&year._long
       where  (
       ( WKSHT_CD EQ "S300001"  )
       or ( WKSHT_CD EQ "S410000" )
       or ( WKSHT_CD EQ "S200000" )
       or ( WKSHT_CD EQ "G300000" )
       or ( WKSHT_CD EQ "E00A18A" )
       )
       ;

       **add varname;
       data out.&dataset.;
           set &dataset;
           length varname $32. ;
           ;                                                                                      *32-character varname maximum;
           *                                                                                      *12345678901234567892123456789312;
           select;
           *hospital-based PAC;
           when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="01900") varname="hosp_provtype"; * hosp provider type;

           when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="02000") varname="subpr_provtype"; * subprovider provider type;

           when (WKSHT_CD EQ "S200000" and CLMN_NUM="0100" and LINE_NUM="02104") varname="urban"; * urban/rural classification;

           *malpractice;
           when (WKSHT_CD EQ "S200000" and CLMN_NUM="0100" and LINE_NUM="05400") varname="malprins_premium"; * malpractice insurance premiums;
           when (WKSHT_CD EQ "S200000" and CLMN_NUM="0200" and LINE_NUM="05400") varname="malprins_paidloss"; * malpractice insurance paid losses;
           when (WKSHT_CD EQ "S200000" and CLMN_NUM="0300" and LINE_NUM="05400") varname="malprins_selfins"; * malpractice insurance self insurance;

           when (WKSHT_CD EQ "S300001" and CLMN_NUM="0100" and LINE_NUM="01200") varname="beds"; *# beds;
           when (WKSHT_CD EQ "S300001" and CLMN_NUM="1500" and LINE_NUM="01200") varname="dischrg"; *# discharges;
           when (WKSHT_CD EQ "S300001" and CLMN_NUM="1200" and LINE_NUM="01200") varname="ti5_dischrg"; *# discharges for Title V;
           when (WKSHT_CD EQ "S300001" and CLMN_NUM="1300" and LINE_NUM="01200") varname="mcre_dischrg"; *# discharges for Title XVIII;
           when (WKSHT_CD EQ "S300001" and CLMN_NUM="1400" and LINE_NUM="01200") varname="mcaid_dischrg"; *# discharges for Title XIX;

           * financial;
           when (WKSHT_CD EQ "G300000" and CLMN_NUM="0100" and LINE_NUM="00100") varname="tot_pat_rev";
           when (WKSHT_CD EQ "G300000" and CLMN_NUM="0100" and LINE_NUM="00300") varname="net_pat_rev";
           when (WKSHT_CD EQ "G300000" and CLMN_NUM="0100" and LINE_NUM="00400") varname="tot_oper_exp";
           when (WKSHT_CD EQ "G300000" and CLMN_NUM="0100" and LINE_NUM="00500") varname="net_pat_inc";
           when (WKSHT_CD EQ "G300000" and CLMN_NUM="0100" and LINE_NUM="02500") varname="tot_oth_inc";
           when (WKSHT_CD EQ "G300000" and CLMN_NUM="0100" and LINE_NUM="03000") varname="tot_net_inc";

           when ( WKSHT_CD EQ "E00A18A" and CLMN_NUM="0100" and LINE_NUM="00400") varname="SSIratio"; /* SSI Ratio */
           when ( WKSHT_CD EQ "E00A18A" and CLMN_NUM="0100" and LINE_NUM="00401") varname="Medicaid_ratio"; /* Medicaid Ratio */
           when ( WKSHT_CD EQ "E00A18A" and CLMN_NUM="0100" and LINE_NUM="00403") varname="DSHratio";/* DSH Ratio */
           when ( WKSHT_CD EQ "E00A18A" and CLMN_NUM="0100" and LINE_NUM="00404") varname="DSHadjust";/* DSH adjust */
           otherwise;
             end;
         run;

         proc contents;
             proc print data=out.&dataset ( obs = 50 );
               proc freq data=out.&dataset;
                 table varname;
               run;
     %end;
 %mend;
 %loop(fyear=2000,lyear=2011);
