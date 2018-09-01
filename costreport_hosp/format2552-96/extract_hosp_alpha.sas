options nocenter ;
*Extract some variables from the HHA ALPHA file from CMS HHA cost report data;
*Reference: hosp_chars_formedstat.sas
http://www.nber.org/docs/hcris/extract_alpha2552-10.sas;

libname library "/home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport";
x "cd /home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport";
libname out "/home/hcmg/kunhee/Labor/Bayada_data";

%macro loop(fyear=,lyear=);

  %do year=&fyear. %to &lyear.;

    %let dataset=alpha&year.;

    proc sql;
      **select appropriate worksheets;
      create table &dataset. as
      select *
      from library.hosp_alpha&year._long
      where  (
      WKSHT_CD EQ "S200000"
      )
      ;

      **add varname;
      data out.&dataset.;
        set &dataset.;
        length varname $32. ;
        ;                                                                                        *32-character varname maximum;
        select;                                                                                  *12345678901234567892123456789312;

        when ( WKSHT_CD EQ "S200000" and CLMN_NUM = "0100" and LINE_NUM = "00200" ) varname = "hospname" ;
        when ( WKSHT_CD EQ "S200000" and CLMN_NUM = "0200" and LINE_NUM = "00200" ) varname = "prov_num" ;
        when ( WKSHT_CD EQ "S200000" and CLMN_NUM = "0100" and LINE_NUM = "00101" ) varname = "city" ;
        when ( WKSHT_CD EQ "S200000" and CLMN_NUM = "0200" and LINE_NUM = "00101" ) varname = "state" ;
        when ( WKSHT_CD EQ "S200000" and CLMN_NUM = "0300" and LINE_NUM = "00101" ) varname = "zip" ;
        when ( WKSHT_CD EQ "S200000" and CLMN_NUM = "0100" and LINE_NUM = "02500" ) varname = "teaching";  ** teaching hospital ;
        when ( WKSHT_CD EQ "S200000" and CLMN_NUM = "0400" and LINE_NUM = "00101" ) varname = "county" ;

        *hospital-based PAC;
        when (WKSHT_CD="S200000" and CLMN_NUM="0500" and LINE_NUM="02103") varname="hosp_cbsa"; * hosp CBSA number;
        when (WKSHT_CD="S200000" and CLMN_NUM="0300" and LINE_NUM="00200") varname="hosp_dtcert"; * hosp date certified;

        when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="00300") varname="subpr_name"; * subprovider name;
        when (WKSHT_CD="S200000" and CLMN_NUM="0200" and LINE_NUM="00300") varname="subpr_ccn"; * subprovider CCN number;
        when (WKSHT_CD="S200000" and CLMN_NUM="0300" and LINE_NUM="00300") varname="subpr_dtcert"; * subprovider date certified;

        when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="05800") varname="irf"; * dummy for IRF;
        when (WKSHT_CD="S200000" and CLMN_NUM="0200" and LINE_NUM="05800") varname="irf_pps"; * dummy for 100% federal PPS reimbursement;

        when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="06000") varname="ipf"; * dummy for IPF;
        when (WKSHT_CD="S200000" and CLMN_NUM="0200" and LINE_NUM="06000") varname="ipf_pps"; * dummy for 100% federal PPS reimbursement;

        when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="00400") varname="swbsnf_name"; * swbsnf name;
        when (WKSHT_CD="S200000" and CLMN_NUM="0200" and LINE_NUM="00400") varname="swbsnf_ccn"; * swbsnf CCN number;
        when (WKSHT_CD="S200000" and CLMN_NUM="0300" and LINE_NUM="00400") varname="swbsnf_dtcert"; * swbsnf date certified;

        when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="00500") varname="swbnf_name"; * swbnf name;
        when (WKSHT_CD="S200000" and CLMN_NUM="0200" and LINE_NUM="00500") varname="swbnf_ccn"; * swbnf CCN number;
        when (WKSHT_CD="S200000" and CLMN_NUM="0300" and LINE_NUM="00500") varname="swbnf_dtcert"; * swbnf date certified;

        when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="00600") varname="snf_name"; * snf name;
        when (WKSHT_CD="S200000" and CLMN_NUM="0200" and LINE_NUM="00600") varname="snf_ccn"; * snf CCN number;
        when (WKSHT_CD="S200000" and CLMN_NUM="0300" and LINE_NUM="00600") varname="snf_dtcert"; * snf date certified;

        when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="00700") varname="nf_name"; * nf name;
        when (WKSHT_CD="S200000" and CLMN_NUM="0200" and LINE_NUM="00700") varname="nf_ccn"; * nf CCN number;
        when (WKSHT_CD="S200000" and CLMN_NUM="0300" and LINE_NUM="00700") varname="nf_dtcert"; * nf date certified;

        when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="00800") varname="oltc_name"; * oltc name;

        when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="00900") varname="hha_name"; * hha name;
        when (WKSHT_CD="S200000" and CLMN_NUM="0200" and LINE_NUM="00900") varname="hha_ccn"; * hha CCN number;
        when (WKSHT_CD="S200000" and CLMN_NUM="0300" and LINE_NUM="00900") varname="hha_dtcert"; * hha date certified;

        when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="01100") varname="asc_name"; * asc name;
        when (WKSHT_CD="S200000" and CLMN_NUM="0200" and LINE_NUM="01100") varname="asc_ccn"; * asc CCN number;
        when (WKSHT_CD="S200000" and CLMN_NUM="0300" and LINE_NUM="01100") varname="asc_dtcert"; * asc date certified;

        when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="01200") varname="hospice_name"; * hospice name;
        when (WKSHT_CD="S200000" and CLMN_NUM="0200" and LINE_NUM="01200") varname="hospice_ccn"; * hospice CCN number;
        when (WKSHT_CD="S200000" and CLMN_NUM="0300" and LINE_NUM="01200") varname="hospice_dtcert"; * hospice date certified;

        when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="01400") varname="rhc_name"; * rhc name;
        when (WKSHT_CD="S200000" and CLMN_NUM="0200" and LINE_NUM="01400") varname="rhc_ccn"; * rhc CCN number;
        when (WKSHT_CD="S200000" and CLMN_NUM="0300" and LINE_NUM="01400") varname="rhc_dtcert"; * rhc date certified;

        when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="01410") varname="fqhc_name"; * fqhc name;
        when (WKSHT_CD="S200000" and CLMN_NUM="0200" and LINE_NUM="01410") varname="fqhc_ccn"; * fqhc CCN number;
        when (WKSHT_CD="S200000" and CLMN_NUM="0300" and LINE_NUM="01410") varname="fqhc_dtcert"; * fqhc date certified;

        when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="01500") varname="cmhc_name"; * cmhc name;
        when (WKSHT_CD="S200000" and CLMN_NUM="0200" and LINE_NUM="01500") varname="cmhc_ccn"; * cmhc CCN number;
        when (WKSHT_CD="S200000" and CLMN_NUM="0300" and LINE_NUM="01500") varname="cmhc_dtcert"; * cmhc date certified;

        when (WKSHT_CD="S200000" and CLMN_NUM="0100" and LINE_NUM="01600") varname="renal_name"; * renal name;
        when (WKSHT_CD="S200000" and CLMN_NUM="0200" and LINE_NUM="01600") varname="renal_ccn"; * renal CCN number;
        when (WKSHT_CD="S200000" and CLMN_NUM="0300" and LINE_NUM="01600") varname="renal_dtcert"; * renal date certified;

        *other hospital characteristics;
        when (WKSHT_CD EQ "S200000" and CLMN_NUM="0100" and LINE_NUM="01700") varname="cr_start"; * cost reporting period start;
        when (WKSHT_CD EQ "S200000" and CLMN_NUM="0200" and LINE_NUM="01700") varname="cr_end"; * cost reporting period end;
        when (WKSHT_CD EQ "S200000" and CLMN_NUM="0100" and LINE_NUM="02101") varname="dissh"; * dummy for disproportionate share hospital adjustment;
        when (WKSHT_CD EQ "S200000" and CLMN_NUM="0100" and LINE_NUM="02104") varname="urban"; * urban/rural classification;
        when (WKSHT_CD EQ "S200000" and CLMN_NUM="0100" and LINE_NUM="02500") varname="teach"; *teaching dummy;
        when (WKSHT_CD EQ "S200000" and CLMN_NUM="0100" and LINE_NUM="05900") varname="ltch"; * dummy for LTCH;
        when (WKSHT_CD EQ "S200000" and CLMN_NUM="0200" and LINE_NUM="05900") varname="ltch_pps"; * dummy for LTCH having 100% federal pps reimbursement;
        when (WKSHT_CD EQ "S200000" and CLMN_NUM="0100" and LINE_NUM="03000") varname="cah"; * dummy for CAH;
        otherwise;
      end;
    run;
    proc contents;
      proc print data=out.&dataset ( obs = 50 );
        *where WKSHT_CD EQ "S200000" ;
        proc freq data=out.&dataset;
          table varname;
        run;

      %end;
    %mend;
    %loop(fyear=2000,lyear=2011);
