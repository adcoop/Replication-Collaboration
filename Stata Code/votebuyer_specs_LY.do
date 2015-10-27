*Coding vote-buying parties

set more off
cd "~\Dropbox\Green and Vasudevan (2015) replication"

use "1. Journalist Data\Input Data\journalist_data_nopii.dta", clear

******************************Preparing the data*****************************\

merge m:1 state_name pc_name using "2. Electoral Data\PC_results.dta", keep(matched master) nogen

preserve 
use "3. Sample Data\AC_expt_sample.dta", clear
keep state_name pc_name
bys state_name pc_name: drop if _n >1

tempfile PC_sample
save `PC_sample', replace
restore

cd "3. Sample Data"
merge m:1 state_name pc_name using `PC_sample'
cd ..

drop if _m ==2
gen in_sample = (_merge==3)
drop _m

merge m:1 state_name pc_name using "2. Electoral Data\ECI_sched_clean.dta", keep(matched master) nogen

*Standardizing party/alliance names
foreach var of varlist spend_1-win_3 party_? actual_cand_party? {
replace `var' = "NDA" if inlist(`var',"BJP","SHS","TDP","SWP","RPI(A)","LJP","RSP","AD")
replace `var' = "UPA" if inlist(`var',"INC","NCP","RJD","RLD","JMM")
}

*Some cleaning up
replace win_2 = "" if win_1 == win_2

gen resp_secret = (secret_1!="") //Responding to vote-buyer party question

*****************************************************************************************************

*Label vars 
label var in_sample "Whether PC is in the experiment sample?"
label var actual_cand_name1 "Results: Winner name"
label var actual_cand_name2 "Results: 1st runner-up name"
label var actual_cand_name3 "Results: 2nd runner_up name"

label var actual_cand_party1 "Results: Winner party"
label var actual_cand_party2 "Results: 1st runner-up party"
label var actual_cand_party3 "Results: 2nd runner_up party"

label var actual_cand_voteshare1 "Results: Winner voteshare"
label var actual_cand_voteshare2 "Results: 1st runner-up voteshare"
label var actual_cand_voteshare3 "Results: 2nd runner_up voteshare"

label var resp_secret "Whether responded to vote-buyer question"

label var pc_num "PC number"

save "1. Journalist Data/Output Data/journalist_data_nopii_clean.dta", replace

************************************Coding the vote-buyers************************************************
*Keep valid responses
keep if in_sample == 1 //keeping responses only for PCs in the experimental sample
keep if resp_secret == 1

keep state_name secret_* poll_date pc_name pc_num jcode

*Number of responses
bys state_name pc_num: egen num_resp = count(jcode)

preserve
bys state_name pc_num: drop if _n>1
tab num_resp
restore
***********************Specification 1*********************************
reshape long secret_, i(state_name pc_name jcode)

rename secret_ party_name
drop if party_name == ""

bys state_name pc_name party_name: drop if _n > 1

drop _j jcode

label var party_name "Vote-buying party"

compress
save "1. Journalist Data/Output Data/votebuyers_spec1.dta", replace

***********************Specification 2*********************************
bys state_name poll_date party_name: drop if _n > 1
drop pc_name pc_num

compress
save "1. Journalist Data/Output Data/votebuyers_spec2.dta", replace

***********************Specification 3*********************************
bys state_name party_name: drop if _n > 1
drop poll_date

compress
save "1. Journalist Data/Output Data/votebuyers_spec3.dta", replace
