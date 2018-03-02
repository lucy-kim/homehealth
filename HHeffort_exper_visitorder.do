*add more measures of home health office's efforts at the patient episode level: mean experience of nurses serving patients, order of nurse's visit during the day

loc path /home/hcmg/kunhee/Labor/Bayada_data
loc gph /home/hcmg/kunhee/Labor/gph
loc reg /home/hcmg/kunhee/Labor/regresults

cd `path'

*--------------------------------------
*get the nurse's experience

*first get a list of patient episode-nurse-week (i.e. monday date) of visit date cells using the patient-visit level data
use HHeffort_visit, clear
keep if facility=="Hosp"
keep if discipline=="SN"

*collapse at the episode-nurse-week level, counting # visits by each nurse in each week for the patient
gen j = 1
collapse (sum) nv = j, by(epiid payrollno monday)

tempfile epi_nurse_wk
save `epi_nurse_wk'

*merge with the nurse's employment start & (intermediate end) dates for each nurse
use daily_workerpanel, clear

*restrict to nurses
keep if discipline=="SN"

*restrict to episodes that started on or before 8/9/2015 - 59 days, i.e. 8/9/2015 is the 60th day of the episode
gen date = mdy(8,9,2015) - 59
count if visitdate <= date
keep if visitdate <= date

keep payrollno monday etd* esd* tenure

collapse (min) tenure (mean) esd2 etd2, by(payrollno monday esd etd)
lab var tenure "Time-varying tenure (in months) up to the start of week (across offices)"
lab var etd2 "New time-varying empl term date (defining 90-day absence as attrition)"
lab var esd2 "New time-varying empl start date (defining 90-day absence as attrition)"

duplicates tag payrollno monday, gen(dup)
tab dup
*dup > 0 only for 1 worker who has two very different empl start dates -> drop obs where esd!=esd2
drop if dup > 0 & esd!=esd2
drop dup

*calculate the length of tenure up to the start of week for each nurse using 2 different empl start dates, one that is starting entirely new in the firm & another that is starting again after 90-day absence

*create monday of the empl start dates
foreach date of varlist esd esd2 {
    capture drop day
    gen day = dow(`date')
    capture drop monday_`date'
    gen monday_`date' = `date' - 6 if day==0
    forval d = 1/6 {
        loc d2 = `d' - 1
        replace monday_`date' = `date' - `d2' if day==`d'
    }
    format monday_`date' %d
    drop day
}

*could have used modulus for simplification
foreach date of varlist esd esd2 {
    capture drop tenure_`date'
    gen tenure_`date' = (monday - monday_`date')/7 + 1
    assert tenure_`date' >= 1
}
lab var monday_esd "Monday date of the very first empl start date"
lab var monday_esd2 "Monday date of esd2, empl start date (counting 90-day absence as attrition)"

lab var tenure_esd "Tenure (wks) incl & upto each week from the first ever empl start date"
lab var tenure_esd2 "Tenure (wks) incl & upto each week from empl start date (using esd2)"

compress
save nurse_tenure_byweek, replace

*--------------------------------------
* get the order of a patient's visit during the visiting nurse's day

*create nurse-date-visit level data with episode ID attached for each visit
*use the most raw visit-level data
use epi_visit, clear
keep payrollno visitdate visittime epiid monday
duplicates drop
*185 obs dropped

*a worker can't visit > 1 patient at the same time
duplicates tag payrollno visitdate visittime, gen(dup)
tab dup
sort payrollno visitdate visittime
list if dup > 0 & payrollno=="347200"
*dup > 0 makes no sense -> the visit time must be wrong -> since it affects tiny fraction of visits, just say the visits occurred on the same order
drop dup

*create a visit order index
sort payrollno visitdate visittime
preserve
keep payrollno visitdate visittime
duplicates drop
bys payrollno visitdate: gen visitorder = _n
tab visitorder
tempfile vo
save `vo'
restore

merge m:1 payrollno visitdate visittime using `vo', nogen

keep payrollno visitdate visittime monday epiid visitorder

compress
save visitorder, replace

tempfile visitorder
save `visitorder'

*link it with episode-visit level data
use HHeffort_visit, clear

duplicates tag payrollno visitdate_e visittime_e monday epiid, gen(dd)
assert dd==0
drop dd

merge 1:1 payrollno visitdate visittime monday epiid using `visitorder', keep(1 3) nogen
*all merged

keep payrollno visitdate visittime wkidx epiid visitorder

compress
save visitorder, replace
