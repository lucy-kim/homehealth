*keep only the first recorded inpat diagnosis code per patient (reference: crinpat_dx.do)

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults
cd `path'

use DB4_M1010_v2, clear
*already unique at the admission ID level

*convert a few ICD-10 codes to ICD-9 codes

*are there any ICD-10 codes (those start with alphabet)?
replace inpatdiag = subinstr(inpatdiag," ","",.)
gen x = real(inpat)
*21 obs missing values in x
tab inpat if x==.

*what are the unique ICD-10 codes appearing in the 2015 inpatient DX data?
preserve
keep if x==.
rename inpatdiag i10
keep i10
duplicates drop

*remove dot
gen i10_old = i10
replace i10 = subinstr(i10, ".", "", .)
replace i10 = subinstr(i10, " ", "", .)
duplicates tag i10, gen(dup)
tab dup
drop dup
duplicates drop i10, force

tempfile uniquei10
save `uniquei10'
restore

preserve
import delimit "`path'/ICD/DiagnosisGEMs-2014/2014_I10gem.txt", clear
split v1, p("")
drop v1
rename v11 i10
rename v12 i9
rename v13 flag

*merge with the list of unique ICD-10 codes appearing in the 2015 inpat DX data
merge m:1 i10 using `uniquei10', keep(2 3)
*348 have _m=3; 91 have _m=2

*if _m=2, manually fill in the ICD-9 codes in http://www.icd10data.com/
list i10* if _merge==2
replace i9 = "001" if i10=="A00"
replace i9 = "038" if i10=="A41"
replace i9 = "174.0" if i10=="C5001"
replace i9 = "174.9" if i10=="C5091"
replace i9 = "198.4" if i10=="C793"
replace i9 = "233.0" if i10=="D05"
replace i9 = "282.62" if i10=="D570"
replace i9 = "249.90" if i10=="E08"
replace i9 = "249.70" if i10=="E085"
replace i9 = "249.80" if i10=="E136"
replace i9 = "249.20" if i10=="F039"
replace i9 = "305.00" if i10=="F101"
replace i9 = "291.81" if i10=="F1023"
replace i9 = "295.90" if i10=="F20"
replace i9 = "295.70" if i10=="F25"
replace i9 = "296.21" if i10=="F32"
replace i9 = "331.0" if i10=="G30"
replace i9 = "345.50" if i10=="G40"
replace i9 = "339.29" if i10=="G892"
replace i9 = "386.11" if i10=="H811"
replace i9 = "386.19" if i10=="H813"
replace i9 = "410.01" if i10=="I21"
replace i9 = "414.01" if i10=="I251"
replace i9 = "414.01" if i10=="I2511"
replace i9 = "415.19" if i10=="I26"
replace i9 = "415.12" if i10=="I269"
replace i9 = "427.31" if i10=="I48"
replace i9 = "428.20" if i10=="I50"
replace i9 = "428.20" if i10=="I502"
replace i9 = "428.30" if i10=="I503"
replace i9 = "440.30" if i10=="I7039"
replace i9 = "480.8" if i10=="J128"
replace i9 = "482.0" if i10=="J15"
replace i9 = "485" if i10=="J18"
replace i9 = "491.22" if i10=="J44"
replace i9 = "562.11" if i10=="K574"
replace i9 = "682.3" if i10=="L0311"
replace i9 = "715.09" if i10=="M15"
replace i9 = "715.15" if i10=="M16"
replace i9 = "715.16" if i10=="M17"
replace i9 = "719.90" if i10=="M25"
replace i9 = "719.41" if i10=="M2551"
replace i9 = "721.90" if i10=="M47"
replace i9 = "724.00" if i10=="M480"
replace i9 = "733.16" if i10=="M80071"
replace i9 = "730.20" if i10=="M86"
replace i9 = "730.05" if i10=="M8615"
replace i9 = "719.7" if i10=="R26"
replace i9 = "781.2" if i10=="R268"
replace i9 = "780.60" if i10=="R50"
replace i9 = "873.8" if i10=="S0190"
replace i9 = "852.21" if i10=="S065"
replace i9 = "873.8" if i10=="S098XX"
replace i9 = "952.00" if i10=="S14102"
replace i9 = "879.0" if i10=="S21009"
replace i9 = "807.02" if i10=="S224"
replace i9 = "805.4" if i10=="S32"
replace i9 = "808.41" if i10=="S32301"
replace i9 = "808.0" if i10=="S3246"
replace i9 = "808.2" if i10=="S32599"
replace i9 = "808.49" if i10=="S3289X"
replace i9 = "812.00" if i10=="S42202"
replace i9 = "812.01" if i10=="S42222"
replace i9 = "812.49" if i10=="S4249"
replace i9 = "890.1" if i10=="S71029"
replace i9 = "820.8" if i10=="S720"
replace i9 = "820.8" if i10=="S72009"
replace i9 = "820.09" if i10=="S72092"
replace i9 = "820.20" if i10=="S72109"
replace i9 = "820.21" if i10=="S7214"
replace i9 = "821.00" if i10=="S729"
replace i9 = "821.00" if i10=="S7290X"
replace i9 = "891.0" if i10=="S81"
replace i9 = "891.0" if i10=="S818"
replace i9 = "891.0" if i10=="S81809"
replace i9 = "823.20" if i10=="S82201"
replace i9 = "824.4" if i10=="S82841"
replace i9 = "824.8" if i10=="S82899"
replace i9 = "836.3" if i10=="S83"
replace i9 = "892.0" if i10=="S913"
replace i9 = "825.0" if i10=="S9203"
replace i9 = "825.29" if i10=="S9220"
replace i9 = "988.0" if i10=="T61781"
replace i9 = "998.01" if i10=="T8111"
replace i9 = "998.30" if i10=="T8130X"
replace i9 = "996.47" if i10=="T84093"
replace i9 = "996.2" if i10=="T85"
replace i9 = "E888.1" if i10=="W180"
replace i9 = "E888.9" if i10=="W19XXX"
replace i9 = "162.2" if i10=="C34"
replace i9 = "820.00" if i10=="S72019"
assert i9!=""
drop _merge

bys i10: gen n = _N
*if there are >1 ICD-9 codes for each ICD-10 codes, then pick one randomly
duplicates drop i10*, force
drop n flag

*place a dot
gen l = length(i9)
gen yesdot = regexm(i9, "[.]")
gen x = ""
forval k = 4/5 {
  replace x = substr(i9,1,3) + "." + substr(i9,4,`k') if l==`k' & yesdot==0
}
replace x = i9 if x==""
drop i9 l yesdot
rename x i9
rename i10 i10_new
rename i10_old i10

tempfile i10gem
save `i10gem'
restore

*merge back for observations with ICD-10 inpat diagnosis
rename inpat i10
merge m:1 i10 using `i10gem'
assert _merge==3 if x==.
replace i9 = i10 if i9=="" & i10!=""
assert i9!=""
keep `mvar' clientid socdate i9
replace i9 = "9" if i9=="009.0"

rename i9 inpat_dx

tempfile data2
save `data2'


*the old inpatient DX may be one of the DX codes in the new data, so drop if they're duplicates
duplicates drop

*drop nonsense codes
drop if inpat_dx=="NoD.x"
drop if inpat_dx=="0"

*some ICD codes are prevalent but unmatched to the ICD-9 to CCS xwalk data -> manually fix
gen inpat_dx_cor = inpat_dx
*below adding .9 means coding it as belonging to the diagnosis code but "unspecified"
replace inpat_dx_cor = "599.9" if inpat_dx=="599"
replace inpat_dx_cor = "428.9" if inpat_dx=="428"
replace inpat_dx_cor = "250.9" if inpat_dx=="250"
replace inpat_dx_cor = "414.9" if inpat_dx=="414"
replace inpat_dx_cor = "008.45" if inpat_dx=="8.45"
replace inpat_dx_cor = "453.40" if inpat_dx=="453.4"
replace inpat_dx_cor = "300.9" if inpat_dx=="300"
replace inpat_dx_cor = "332.0" if inpat_dx=="332"
replace inpat_dx_cor = "272.4" if inpat_dx=="78.5"
replace inpat_dx_cor = "250.00" if inpat_dx=="250"
replace inpat_dx_cor = "4660" if inpat_dx=="466"
replace inpat_dx_cor = "507.8" if inpat_dx=="507"
replace inpat_dx_cor = "821.00" if inpat_dx=="821"
replace inpat_dx_cor = "070.70" if inpat_dx=="70.7"
replace inpat_dx_cor = "339.20" if inpat_dx=="339.29"
replace inpat_dx_cor = "015.90" if inpat_dx=="15"

*merge with ICD-9 to CCS xwalk
gen icd = subinstr(inpat_dx_cor, ".","",.)
merge m:1 icd using dxicd2ccsxw.dta, keep(1 3)

*use some rules:

*if XXX, add ".9";
gen l = length(inpat_dx)
split inpat_dx, p(".")
capture drop yesdot
gen yesdot = inpat_dx2!=""
replace inpat_dx_cor = inpat_dx + ".9" if l==3 & yesdot==0 & _m==1

*if XXX.Y, add "0" as the second decimal digit;
gen l1 = length(inpat_dx1)
gen l2 = length(inpat_dx2)
replace inpat_dx_cor = inpat_dx + "0" if l1==3 & l2==1 & _m==1
*tab inpat_dx_cor if l1==3 & l2==1 & _m==1

*if XX.Y or XX.YY, add "0" as the first digit;
replace inpat_dx_cor = "0" + inpat_dx if l1==2 & (l2==1 | l2==2)& _m==1

*if X (single digit integer), add "00" as the first two digits and add ".0" at the end
replace inpat_dx_cor = "00" + inpat_dx + ".0" if l==1 & yesdot==0 & _m==1

*if X.X or X.YY, add "00" as the first two digits
replace inpat_dx_cor = "00" + inpat_dx if l1==1 & (l2==1 | l2==2) & _m==1

*if XX, add "0" as the first digit, and add ".9" at the end
replace inpat_dx_cor = inpat_dx + ".9" if l==2 & _m==1 & yesdot==0

*merge again with ICD-9 to CCS xwalk
drop icd-_merge l l1 l2 inpat_dx1 inpat_dx2 yesdot
gen icd = subinstr(inpat_dx_cor, ".","",.)
merge m:1 icd using dxicd2ccsxw.dta, keep(1 3)
*tab inpat_dx_cor if _m==1, sort

*if XX, add "0" as the first digit, and add ".0" at the end
split inpat_dx, p(".")
gen l1 = length(inpat_dx1)
gen l2 = length(inpat_dx2)
replace inpat_dx_cor = "0" + inpat_dx + ".0" if l1==2 & l2==0 & _m==1

gen yesdot = inpat_dx2!=""
replace inpat_dx_cor = inpat_dx + ".00" if l1==3 & l2==0 & yesdot==0 & _m==1

*merge again with ICD-9 to CCS xwalk
drop icd-_merge l1 l2 inpat_dx1 inpat_dx2 yesdot
gen icd = subinstr(inpat_dx_cor, ".","",.)
merge m:1 icd using dxicd2ccsxw.dta, keep(1 3)
*tab inpat_dx_cor if _m==1, sort
*tab inpat_dx if _m==1, sort

tempfile tmp
save `tmp'


use `tmp', clear
*if XXX, just add ".0" at the end
split inpat_dx, p(".")
gen l1 = length(inpat_dx1)
gen l2 = length(inpat_dx2)
gen yesdot = inpat_dx2!=""
replace inpat_dx_cor = inpat_dx + ".0" if l1==3 & l2==0 & yesdot==0 & _m==1

*if 0XX.Y, add "0" as the second decimal digit;
replace inpat_dx_cor = inpat_dx_cor + "0" if (l1==1 | l1==2) & l2==1 & _m==1

*if XX, add "0" as the first digit
replace inpat_dx_cor = "0" + inpat_dx if l1==2 & l2==0 & _m==1

*merge again with ICD-9 to CCS xwalk
drop icd-_merge l1 l2 inpat_dx1 inpat_dx2 yesdot
gen icd = subinstr(inpat_dx_cor, ".","",.)
merge m:1 icd using dxicd2ccsxw.dta, keep(1 3)
assert _m==3
drop _m optccs-icdvers

*one manual fix
replace inpat_dx_cor = "466" if inpat_dx_cor == "4660"
replace icd = "466" if icd == "4660"

tempfile firstdx
save `firstdx'

*tag cardiorespiratory conditions
split inpat_dx_cor, p(".")
gen xx = real(inpat_dx_cor1)
drop if xx==.
drop xx
destring inpat_dx_cor1, replace

tab inpat_dx_cor1 if inpat_dx_cor1>=390 & inpat_dx_cor1 <= 519
tab ccsdesc if inpat_dx_cor1>=390 & inpat_dx_cor1 <= 519

gen cardioresp = inpat_dx_cor1>=390 & inpat_dx_cor1 <= 519


*for ICD-9-CM codes, see https://www.qualitynet.org/dcs/ContentServer?cid=1228774371008&pagename=QnetPublic%2FPage%2FQnetTier4&c=Page
rename inpat_dx icd2

*remove "."
replace icd2 = subinstr(icd2, ".", "",.)
destring icd2, replace

* AMI (from Table D.1.1 in 2016 report)
gen ami = icd2==41000 | icd2==41001 | icd2==41010 | icd2==41011 | icd2==41020 | icd2==41021 | icd2==41030 | icd2==41031 | icd2==41040 | icd2==41050 | icd2==41051 | icd2==41060 | icd2==41061 | icd2==41070 | icd2==41071 | icd2==41080 | icd2==41081 | icd2==41090 | icd2==41091

* HF
tab icd2 if (icd2 >= 40201 & icd2 <= 42843) | icd2==4289
gen hf = icd2==40201 | icd2==40211 | icd2==40291 | icd2==40401 | icd2==40403 | icd2==40411 | icd2==40413 | icd2==40491 | icd2==40493 | icd2==4280 | icd2==4281 | icd2==4289 | icd2==42820 | icd2==42821 | icd2==42822 | icd2==42823 | icd2==42830 | icd2==42831 | icd2==42832 | icd2==42833 | icd2==42840 | icd2==42841 | icd2==42842 | icd2==42843

*PNEU
gen pneu = icd2==4800 | icd2==4801 | icd2==4802 | icd2==4803 | icd2==4808 | icd2==4809 | icd2==481 | icd2==4820 | icd2==4821 | icd2==4822 | icd2==48230 | icd2==48231 | icd2==48232 | icd2==48239 | icd2==48240 | icd2==48241 | icd2==48242 | icd2==48249 | icd2==48281 | icd2==48282 | icd2==48283 | icd2==48284 | icd2==48289 | icd2==4829 | icd2==4830 | icd2==4831 | icd2==4838 | icd2==485 | icd2==486 | icd2==4870 | icd2==48811 | icd2==5070

* COPD
tab icd2 if icd2 >=49121 & icd2 <= 49600
gen copd = icd2==49121 | icd2==49122 | icd2==4918 | icd2==4919 | icd2==4928 | icd2==49320 | icd2==49321 | icd2==49322 | icd2==496
rename icd2 inpat_dx

compress
save inpat_firstdx, replace
