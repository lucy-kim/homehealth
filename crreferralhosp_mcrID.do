*crreferral_mcrID.do
*get Medicare ID for each hospital that appears in the patient referral source data (referred to crhospcr_referral.do)

loc path /home/hcmg/kunhee/Labor/Bayada_data/
cd `path'

*-------------
* get office ID for each admission
*-------------
*create temp referral hospital files for each YR
use referral, clear

tab type, sort

*merge with client data to get the address of office & ZIP code of patient
duplicates tag clientid socdate, gen(dup)
tab dup
*116 obs: regard these as separate referrals
drop dup

merge m:1 clientid socdate_e using officeID_foradmitID, keep(1 3) nogen
*99 admissions unmatched _m=1
drop epiid
count if offid_nu==.

tempfile reffull
save `reffull'

*restrict to clients that have no missing values in office ID
use `reffull', clear

/* tab socdate if offid_nu==. */
*40% from post 8/24/2015
tab st if offid_nu==.
*91% in NJ

drop if offid_nu==.
*99 clients dropped

*merge with office level data
merge m:1 offid_nu using office_flow, keepusing(addr_st) keep(1 3) nogen

replace state = addr_st if state==""
assert state!=""
assert facility!=""
assert offid_nu!=.

*at least improve the name of facility for hospitals to remove any duplicates because of difference in names
rename facility hospname
gen hosp_orig = hospname

*by removing some string values, standardize the format of hospital names
gen x = regexm(hospname, "'''") | regexm(hospname, "\(") | regexm(hospname, "\)")
*list if x==1
*remove any quotation marks, etc.
replace hospname = subinstr(hospname, "  ", " ", .)
replace hospname = subinstr(hospname, "'", "", .)
replace hospname = subinstr(hospname, "(", "", .)
replace hospname = subinstr(hospname, ")", "", .)
replace hospname = subinstr(hospname, "&", "and", .)
replace hospname = subinstr(hospname, "  ", " ", .)

replace hospname = subinstr(hospname, " inc.", "", .)
replace hospname = subinstr(hospname, ", llc", "", .)
replace hospname = subinstr(hospname, " inc", "", .)
replace hospname = subinstr(hospname, " -", "-", .)
replace hospname = subinstr(hospname, "- ", "-", .)
replace hospname = subinstr(hospname, "st ", "st. ", .) if regexm(hospname, "^st ")
replace hospname = subinstr(hospname, "the ", "", .) if regexm(hospname, "^the ")
replace hospname = subinstr(hospname, "dup- ", "", .)
replace hospname = subinstr(hospname, "-", " ", .)
replace hospname = subinstr(hospname, "dup ", "", .)
replace hospname = subinstr(hospname, "hosp.", "hospital", .)
replace hospname = subinstr(hospname, "inst.", "institution", .)
replace hospname = subinstr(hospname, " inst ", " institution", .)
replace hospname = subinstr(hospname, " reg ", " regional", .)
replace hospname = subinstr(hospname, " ctr", " center", .)
replace hospname = subinstr(hospname, "n.", "northern", .)
replace hospname = subinstr(hospname, "comm.", "community", .)
replace hospname = subinstr(hospname, ".", "", .)

drop nadm
bys state hospname: gen nadm = _N

tempfile office_referral
save `office_referral'

keep hospname state nadm type

duplicates drop
duplicates tag hospname state, gen(dup)
tab dup
list if dup > 0
drop if hospname=="walgreens infusion" & type=="OTHER"
drop if hospname=="meadows at shannondell" & type=="AL/IL/CCRC- SENIOR LIVING"
drop dup

count
tempfile ref
save `ref'

*-------------
* process Medicare Cost Report data to get Medicare ID for each hospital
*-------------
*append all years of Cost report data
use hospcr_panel, clear

replace hospname = lower(hospname)

*keep unique list of state-hospitals
keep state hospname prvdr_num
duplicates drop

assert state!=""
assert hospname!=""
recast str50 hospname

*several hospitals have multiple provider numbers
duplicates tag hospname state, gen(dup)
tab dup
drop dup

replace hospname = subinstr(hospname, "  inc.", "", .)
replace hospname = subinstr(hospname, "  inc", "", .)
replace hospname = subinstr(hospname, ", llc", "", .)
replace hospname = subinstr(hospname, " llc", "", .)
replace hospname = subinstr(hospname, "med ctr", "medical center", .)
replace hospname = subinstr(hospname, "med. ctr.", "medical center", .)
replace hospname = subinstr(hospname, " ctr", " center", .)
replace hospname = subinstr(hospname, " -", "-", .)
replace hospname = subinstr(hospname, "- ", "-", .)
replace hospname = subinstr(hospname, "hosp-", "hospital-", .)
replace hospname = subinstr(hospname, "hosp.", "hospital", .)
replace hospname = subinstr(hospname, "st ", "st. ", .) if regexm(hospname, "^st ")
replace hospname = subinstr(hospname, "the ", "", .) if regexm(hospname, "^the ")
replace hospname = subinstr(hospname, "  ", " ", .)
replace hospname = subinstr(hospname, "-", " ", .)
replace hospname = subinstr(hospname, " mem ", " memorial ", .)
replace hospname = subinstr(hospname, "&", "and", .)
replace hospname = subinstr(hospname, "inst.", "institution", .)
replace hospname = subinstr(hospname, " inst ", " institution", .)
replace hospname = subinstr(hospname, " reg ", " regional", .)
replace hospname = subinstr(hospname, "n.", "northern", .)
replace hospname = subinstr(hospname, "comm.", "community", .)
replace hospname = subinstr(hospname, ".", "", .)

duplicates drop
duplicates drop hospname state, force

compress
/*save hospname`y', replace*/

tempfile cr
save `cr'

*---------------
* merge the state-hospital level data on string hospital names with the Cost Report data by the hospital name
*---------------
*clean up some string values in the hospital name to remove unimportant parts like comma, inc.
use `ref', clear
count

replace state = "NJ" if hospname=="premier orthopedic associates of south jersey elm" & state=="-"

merge 1:1 state hospname using `cr'
*337 _m=3; 714 _m=1; 4737s _m=2

sort _m state hospname

tempfile postmerge
save `postmerge'

preserve
keep if _m==1
saveold onlyref`y', replace v(12)
restore
preserve
keep if _m!=1
saveold onlycr`y', replace v(12)
restore

*merge with the new names looked up by Dave
preserve
insheet using `path'/client_CSV/refhosp_namelookup.csv, comma names clear
rename id newprovnum
keep newprovnum hospname-nadm newname
list if newprovnum==.

*for obs not assigned any hospital numbers by Dave, double-check if they are really not hospitals
replace newname = "roger williams hospital" if hospname=="roger williams medical center" & state=="RI"
replace newname = "st joseph hlth serv of rhode islan" if hospname=="st josephs hospital ri" & state=="RI"
replace newname = "rhode island hospital" if hospname=="lifespan" & state=="RI"
replace newname = "our lady of fatima hospital" if hospname=="southern new england rehabilitation center at our" & state=="RI"
replace newname = "ut md anderson cancer center" if hospname=="md anderson cancer center university of tx" & state=="TX"
replace newname = "vcu health system mcv hospital" if (hospname=="mcv hospital" | hospname=="virginia commonwealth university medical center") & state=="VA"
replace newname = "sentara norfolk general hospital" if (hospname=="sentara heart hospital" | hospname=="sentara") & state=="VA"
replace newname = "sentara careplex hospital" if (hospname=="sentara careplex") & state=="VA"
replace newname = "sentara williamsburg regional medica" if hospname=="sentara williamsburg regional medical center" & state=="VA"
/* replace newname = "sentara princesss anne hospital" if hospname=="sentara princess anne hospital" & state=="VA" */
replace newname = "cheshire medical center" if hospname=="cheshire medical center/dartmouth hitchcock keene" & state=="VT"
replace state = "NH" if hospname=="cheshire medical center/dartmouth hitchcock keene" & state=="VT"
replace newname = "university of vermont medical center" if hospname=="fanny allen hospital" & state=="VT"

compress
save refhosp_namelookup, replace
restore

*---------------
* add the hospitals whose names were manually changed
*---------------
use `postmerge', clear
drop if _merge==2
/* keep if state=="RI" | state=="SC" | state=="TN" | state=="TX" | state=="UT" | state=="VA" | state=="VT" | state=="WA" | state=="WV" | state=="WY" */
merge 1:1 hospname state using refhosp_namelookup, gen(m3) keep(1 3) keepusing(newname)
count if newname!=""
list if m3==3 & newname==""

drop m3

*create indicator for not hospital & hospital but can't find a CMS provider number
gen nothosp = .
gen hosp_noprvn = .

*manually change the hospital name in the referral source data
rename state state_orig
gen state = state_orig

compress
save pre_hncorrect, replace

*---------------
* change the string hospital name values to fit with the Medicare Cost Report data hospital name values
*---------------
do /home/hcmg/kunhee/Labor/crpost_hncorrect

*---------------
* merge the data with revised hosp names with the Medicare Cost Report data to get Medicare ID for each hospital
*---------------
use post_hncorrect, clear

*restrict to hospitals
keep if type=="HOSPITAL"
drop type

rename hospname hospname_orig
drop prvdr_num
rename newname hospname
replace hospname = hospname_orig if hospname==""

*re-match with hospital CR data by state & hospname
merge m:1 state hospname using `cr', gen(m2) keep(1 3)
*240 _m=1
count if prvdr_num==""
*130 obs

*double-check whether these unmatched hospitals are really not hospitals
count if prvdr_num==""
*227 obs have no CMS hospital provider # - are these really not hospitals?
gen bad = regexm(hospname, "va ") | regexm(hospname, " va") | regexm(hospname, "veteran") | regexm(hospname, "miltary") | regexm(hospname, "naval")
list hospname hospname_orig state nadm if prvdr_num=="" & bad==0
count if prvdr_num=="" & bad==0
*69 obs
list if prvdr_num=="" & bad==0
count
*1048 obs

compress
save hosp_string, replace

*restrict to hospital referral obs that have non-missing CMS provider #
use hosp_string, clear
drop if prvdr_num==""
keep hospname_orig state_orig state hospname state prvdr_num
tempfile prvdr_num
save `prvdr_num'

*-------------
* add the hospital ID to each hospital in the admission-level data
*-------------
use `office_referral', clear
keep if type=="HOSPITAL"
rename hospname hospname_orig
rename state state_orig
merge m:1 hospname_orig state_orig using `prvdr_num', keep(3) nogen
*4723 obs _m=1 for VA, etc. ; 112K obs _m=3

keep clientid socdate admissionclie yr prvdr_num state addr_st offid_nu hospname
duplicates drop
*yr is the Year of SOC date

compress
save referralhosp_mcrID.dta, replace

*-------------
* how many hospitals appear in the referral source data?
sort prvdr_num
bys prvdr_num: gen i = _n==1
count if i==1
