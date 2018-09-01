options nocenter ;
*Download & unzip the CMS HRIS hospital cost report data for each year;

%macro downloadhospcr(firstyr, lastyr);
%do year = &firstyr. %to &lastyr.;

* Get files;
x "cd /home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport";
/* x "wget -nv -N http://downloads.cms.gov/FILES/HCRIS/HOSP10FY&year..zip";
x "unzip HOSP10FY&year..zip"; */

*rename files to match names from NBER;
x "mv hosp10_&year._RPT.CSV hosp_rpt2552_10_&year..csv";
x "mv hosp10_&year._ALPHA.CSV hosp_alpha2552_10_&year._long.csv";
x "mv hosp10_&year._NMRC.CSV hosp_nmrc2552_10_&year._long.csv";

/* x "wget -nv -N http://www.nber.org/hcris/2552-10/hosp_rpt2552_10_&year..csv";
x "wget -nv -N http://www.nber.org/hcris/2552-10/hosp_nmrc2552_10_&year._long.csv";
x "wget -nv -N http://www.nber.org/hcris/2552-10/hosp_alpha2552_10_&year._long.csv"; */

%end;
%mend downloadhospcr;

%downloadhospcr(2010,2017);
