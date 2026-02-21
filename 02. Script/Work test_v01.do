/*----------------------------------------------------------------------

------------------------------------------------------------------------
Author          : Kanya Anindya 
Project         : Causal treatment effects on survival and complications
Email           : kanyaanin@gmail.com
Version         : v01, 21-Feb-2026
------------------------------------------------------------------------
Data cleaning and management
----------------------------------------------------------------------*/
	
set more 1
cap log close
clear all

program define datetime 
 disp "$S_DATE $S_TIME"
 end

** Set directory
global project 	"/Users/kanyaanindya/Documents/11. Interview/02. KI - MASLD/Work test/"
global source 		 "${project}01. Dataset/"
global dofile 		 "${project}02. Script"
global excel 		 "${project}03. Excel/"
global result 		 "${project}04. Result/"
global logfile 		 "${project}05. Log/"
global temp 		 "${project}06. Temp/"
global figure 		 "${project}07. Figure/"

log using "${logfile} KI Work test_($S_DATE).log", replace 
display "This log file was created on $S_DATE $S_TIME"

**# Excel to stata
	* Lab data 
	import excel "${source}labdata.xlsx", first clear
	save "${source}labdata.dta", replace
	
	* Lab data 
	import excel "${source}tmpdata.xlsx", first clear
	save "${source}tmpdata.dta", replace

/****************************

▀█▀ █▀▄▀█ █▀█ █▀▄ ▄▀█ ▀█▀ ▄▀█
░█░ █░▀░█ █▀▀ █▄▀ █▀█ ░█░ █▀█

*****************************/

**# Load data
	use "${source}tmpdata.dta", clear

	* Label all variables
	la var ID "Patient identification"
	la var kon "The sex of the patient"
	la var fodelsedatum "The patients birth date"
	la var T_grupp "Tumor group"
	la var behandlingsgrupp "Treatment group"
	la var diagnosdatum "Date of diagnosis"
	la var avliddatum "Death date"
	la var komplikationsdatum "Date of complication"

**# Handling duplicates
	* Check duplicates
	sort ID
	egen nmiss = rowmiss(ID-diagnosdatum)
	bysort ID: gen dup = _N
	bysort ID (nmiss): gen keep = (_n == 1)

    list ID-komplikationsdatum  keep if dup > 1, sepby(ID) // list all dups
	count if dup > 1 // how many duplicates?
	
	* Remove duplicates
	duplicates list
	keep if keep==1 // remove dups, 4 obs deleted
	drop nmiss-keep
	
**# Check codings
	* Clean strings
	foreach x in kon behandlingsgrupp  {
		replace `x' = strtrim(lower(`x'))
	}
	
	* Sex
	gen kon1 = . 
	replace kon1 = 0 if kon=="män"
	replace kon1 = 1 if kon=="kvinnor"
	la def kon1_ 0 "Män" 1 "Kvinnor", replace
	la val kon1 kon1_
	la var kon1 "The sex of the patient"
	
	* Treatment
	gen behandlingsgrupp1 = .
	replace behandlingsgrupp1 = 0 if behandlingsgrupp=="obehandlad"
	replace behandlingsgrupp1 = 1 if behandlingsgrupp=="behandlad"
	la def behandlingsgrupp1_ 0 "Obehandlad" 1 "Behandlad", replace
	la val behandlingsgrupp1 behandlingsgrupp1_
	la var behandlingsgrupp1 "Treatment group"
	
	* Tumour group
	gen T_grupp1 = .
	replace T_grupp1 = 0 if T_grupp=="T1-3"
	replace T_grupp1 = 1 if T_grupp=="T4ab"
	la def T_grupp1_ 0 "T1-3" 1 "T4ab", replace
	la val T_grupp1 T_grupp1_
	la var T_grupp1 "Tumor group"

	* Date consistency
	foreach var of varlist fodelsedatum diagnosdatum avliddatum komplikationsdatum {
		format `var' %tdCCYY-NN-DD
	}
	list fodelsedatum diagnosdatum avliddatum komplikationsdatum 
	
**# Follow-up variables
	* End of follow up
	local end_fu = mdy(12,31,2017)
	gen double end_fu = `end_fu'
	format end_fu %tdCCYY-NN-DD
	la var end_fu "End of follow-up (31 Dec 2017)"
	
	* Died
	gen dod = 0
	replace dod = 1 if !missing(avliddatum) & avliddatum <= end_fu
	la def dod_ 0 "Alive" 1 "Died", replace
	la val dod dod_
	la var dod "Death during follow-up"
	
	* Complications
	gen komplikation = 0
	replace komplikation = 1 if !missing(komplikationsdatum) & komplikationsdatum <= end_fu
	label define komplikation_ 0 "No complication" 1 "Complication", replace
	label values komplikation komplikation_
	label var komplikation "Complication during follow-up"
	
	* Follow-up times
	gen last_fu = end_fu
	replace last_fu = avliddatum if dod == 1
	format last_fu %tdCCYY-NN-DD
	lab var last_fu "Last follow-up date"
	
	gen double uppfoljningstid = last_fu - diagnosdatum
	lab var uppfoljningstid "Follow-up time (days)"
	
	* Define competing risks time and type
	gen cr_last = end_fu
	
	replace cr_last = komplikationsdatum if !missing(komplikationsdatum) ///
		& komplikationsdatum < cr_last // Earliest of complication, death, or end of follow-up
	replace cr_last = avliddatum if !missing(avliddatum) ///
		& avliddatum < cr_last
	gen cr_time = cr_last - diagnosdatum
	la var cr_time "Time to first event or censoring (days)"

		* 0=censored, 1=complication first, 2=death first
		gen failtype = 0 //  censored
		replace failtype = 1 if !missing(komplikationsdatum) ///
			& komplikationsdatum == cr_last // complication
		replace failtype = 2 if !missing(avliddatum) /// 
			& avliddatum == cr_last  // death

		la def faillbl 0 "Censored" 1 "Complication" 2 "Death", replace
		la val failtype faillbl
		la var failtype "First event type"
	
	* Age at diagnosis
	gen alder1 = (diagnosdatum - fodelsedatum) / 365.25 // decimal
		
	gen alder = year(diagnosdatum) - year(fodelsedatum) - (mdy(month(diagnosdatum), ///
			day(diagnosdatum), year(diagnosdatum)) < mdy(month(fodelsedatum), ////
			day(fodelsedatum), year(diagnosdatum))) // in years
	
	lab var alder1 "Age at diagnosis (decimal)"
	lab var alder "Age at diagnosis (in years)"

**# Risk group
	* Risk 1: large tumour (T4ab)
	* Risk 2: T1-3 and age > 65
	* Risk 3: T1-3 and age <= 65
	
	gen riskgrupp = .
	replace riskgrupp = 2 if T_grupp1 == 1 // Risk Group 1 - highest risk
	replace riskgrupp = 1 if T_grupp1 == 0 & alder > 65 // Risk Group 2 - middle risk
	replace riskgrupp = 0 if T_grupp1 == 0 & alder <= 65 // Risk Group 3 - lowest risk
	
	la def riskgrupp_  0 "Lag risk" 1"Medelrisk" 2"Hog risk", replace
	la val riskgrupp riskgrupp_
	la var riskgrupp "Risk group"
	
**# Save cleaned tmpdata as temp file
	order ID kon kon1 fodelsedatum alder alder1  T_grupp T_grupp1 ///
		  riskgrupp behandlingsgrupp behandlingsgrupp1 ///
		  diagnosdatum avliddatum komplikationsdatum ///
		  dod komplikation end_fu last_fu uppfoljningstid
	tempfile tmp_clean
	save `tmp_clean', replace
	
/****************************

█░░ ▄▀█ █▄▄ █▀▄ ▄▀█ ▀█▀ ▄▀█
█▄▄ █▀█ █▄█ █▄▀ █▀█ ░█░ █▀█

*****************************/

**# Load data
	use "${source}labdata.dta", clear

**# Check codings
	* Date
	format diagnosdatum %tdCCYY-NN-DD
	
**# Reshape to wide
	reshape wide value, i(ID diagnosdatum) j(variable) string
	
	rename valueht ht
	rename valuecrp crp
	label var ht  "Haematocrit (volume %)"
	label var crp "C-reactive protein"
	
**# Handle missing value
	egen nmiss = rowmiss(_all)
	list if nmiss > 0
	drop if nmiss > 0 // drop 2 observations with missing date
	drop nmiss
	
**# Save cleaned tmpdata as temp file
	tempfile lab_clean
	save `lab_clean', replace

/*****************************

▄▀█ █░░ █░░   █▀▄ ▄▀█ ▀█▀ ▄▀█
█▀█ █▄▄ █▄▄   █▄▀ █▀█ ░█░ █▀█

*****************************/

**# Merge and check missing
	use `tmp_clean', clear
	assert !missing(ID)
	assert !missing(diagnosdatum)

	* Merge, ssc install mmerge if you haven't
	mmerge ID diagnosdatum using `lab_clean'
	tab _merge
	drop _merge
	
	* Distributional assessment
	sum _all
	list if avliddatum <= diagnosdatum // Death < Diagnosed
	drop if avliddatum <= diagnosdatum // 1 person (ID: 457)
	
	list if komplikationsdatum <= diagnosdatum // Complication < Diagnosed
	drop if komplikationsdatum <= diagnosdatum // 1 person (ID: 514)

	* Check missing value
	misschk kon1 alder1 T_grupp1 riskgrupp behandlingsgrupp1 crp ht, gen(miss)
	list  if missnumber>0 // only 2 cases, use complete case analysis
	drop if missnumber>0 
	drop miss* 

**# Save merged data
	save "${result}worktest_v01.dta", replace
	export excel "${excel}worktest_v01.xls", replace firstrow(variables)
