**craha.do**
* construct AHA data for each hospital-year that contain strict and loose integration measures, system ID
*raw data source: WRDS
*data dictionary https://wrds-web.wharton.upenn.edu/wrds/tools/variable.cfm?library_id=4040

cd /home/hcmg/kunhee/Labor/Bayada_data/Hospital/AHA2005_2015

*on unix, convert XLSX data file to Stata file: run "qrsh -now no st AHA_2005-2015.xlsx AHA_2005-2015.dta"

use AHA_2005-2015, clear

keep if year > 2011

keep id mcrnum mname mstate year mhsmemb sysid sysname snhos netname netwrk snsys snnet snven homehhos homehsys homehnet homehven

sort id year
*hospitals with medicare ID missing, so impute across years if non-missing for subset of years
gsort id -mcrnum
bys id: replace mcrnum = mcrnum[_n-1] if mcrnum >= .

*drop if still missing Medicare prov ID (4K obs)
drop if mcrnum==.

*unique Medicare ID per AHA hospital ID?
preserve
gen n = 1
collapse (sum) n, by(id mcrnum)
bys id: egen tn = sum(n)

*drop if AHA ID has a unique Medicare ID
drop if n==tn

*leave a Medicare provider ID that appears most if AHA ID does not have a unique Medicare ID
bys id: egen maxn = max(n)

keep if n==maxn

gen ratio = n/tn
drop if maxn!=n & ratio!=0.5

*manually choose a Medicare provider ID by looking up hospital names
gen bad = ratio==0.5

*one AHA id had 3 different medicare IDs
duplicates tag id, gen(dd)
tab dd
replace bad = 1 if dd > 0
*all bad = 0

bys id: drop if bad==1 & _n==1

keep id mcrnum bad
rename mcrnum mcrnum2

tempfile dupmp
save `dupmp'
restore

*for AHA IDs with duplicate IDs that have a dominant Medicare ID, assign that to the AHA ID
merge m:1 id using `dupmp'
replace mcrnum = mcrnum2 if _merge==3 & bad==0
drop _merge bad mcrnum2

duplicates drop

*-----------
*a lot of missing values in the SNF integration measures
gen snmiss = snhos==.
tab year, summarize(snmiss)
gen hhmiss = homehhos==.
tab year, summarize(hhmiss)
* 2007 have no missing values; all other years have ~20% missing; 2014 goes up to 24% for both SNF and HHA

sort mcrnum year

*drop if different IDs assigned to the same Medicare ID-year & everything else is same
drop id mname sysname
duplicates drop

*unique at the hospital-year level?
duplicates tag year mcrnum , gen(dup)
tab dup
*12 obs have duplicates by Medicare ID & year
*mname and sysid seem to have different values for duplicates by Medicare ID-year
tab mcrnum if dup > 0

*manually recode
replace sysid=181 if mcrnum==62015 & dup > 0

*missing values in one of the duplicate rows and non-missing value in another duplicate row ; get max value
drop netname
collapse (max) mhsmemb-homehven, by(mcrnum year mstate)

duplicates tag year mcrnum , gen(dup)
assert dup==0
drop dup

sort mcrnum year

loc d_hos "owned/prov by my hosp/subsidiary"
loc d_sys "prov by my health system (in local community)"
loc d_net "prov by my network (in local community)"
loc d_ven "formal contract/joint venture w/ provider not in sys or net"

foreach p in "sn" "homeh" {
  foreach k in "hos" "sys" "net" "ven" {
      lab var `p'`k' "`p' `d_`k''"
  }
}

lab var netwrk "is the hosp a participant in a network"
/*lab var netname "network name"*/

* trend of hospitals' SNC and HHC integration across time
foreach p in "sn" "homeh" {
  foreach k in "hos" "sys" "net" "ven" {
    tab year, summarize(`p'`k')
  }
}

compress
save aha, replace
