options nocenter ;
*Download & unzip the CMS HRIS hospital cost report data for each year;

%macro downloadhospcr(firstyr, lastyr);
%do year = &firstyr. %to &lastyr.;

* Get files;
x "cd /home/hcmg/kunhee/Labor/Bayada_data/Hospital/costreport";
x "wget -nv -N http://www.nber.org/hcris/2552-96/hosp_rpt2552_96_&year..csv";
x "wget -nv -N http://www.nber.org/hcris/2552-96/hosp_nmrc2552_96_&year._long.csv";
x "wget -nv -N http://www.nber.org/hcris/2552-96/hosp_alpha2552_96_&year._long.csv";

%end;
%mend downloadhospcr;

%downloadhospcr(2000,2011);
