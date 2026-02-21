/*----------------------------------------------------------------------

------------------------------------------------------------------------
Author          : Kanya Anindya 
Project         : Causal treatment effects on survival and complications
Email           : kanyaanin@gmail.com
Version         : v01, 20-Feb-2026
------------------------------------------------------------------------
Data analysis
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

log using "${logfile} KI Work test_analysis_($S_DATE).log", replace 
display "This log file was created on $S_DATE $S_TIME"

**# Load data
	use "${result}worktest_v01.dta", clear
	
**# Descriptive
	gen uppfoljning_manader = uppfoljningstid/30.4375
	la var uppfoljning_manader "Follow-up time (in months)"
	
	dtable alder i.kon1 i.T_grupp1 i.riskgrupp ht crp uppfoljningstid uppfoljning_manader, ///
		by(behandlingsgrupp1) ///
		define(meansd = mean sd, delimiter(" ± ")) ///
        define(myiqr = p25 p75, delimiter("-")) ///
		nformat(%6.0f p25 p75 median) sformat("(%s)" myiqr) ///
		nformat(%6.1f mean sd) sformat("%s" sd) ///
		continuous(crp uppfoljningstid uppfoljning_manader, stat(median myiqr)) ///
		continuous(alder ht , stat(meansd)) ///
		sample(, statistics(freq) place(seplabels)) sformat("(N=%s)" frequency) ///
		export("${excel}worktest_desc_v01.xls", replace)
	
	* Observe the dates
	summ diagnosdatum, detail
	display "Earliest diagnosdatum: " %td r(min) // 2008-01-12
	display "Latest diagnosdatum:   " %td r(max) // 2015-01-26
	
	summ uppfoljningstid, detail
	display "Minimum follow-up (days): " %9.0f r(min) // 2 days
	display "Maximum follow-up (days): " %9.0f r(max) // 3641 days
	
/***********************************************

█▀▄▀█ ▄▀█ █ █▄░█   ▄▀█ █▄░█ ▄▀█ █░░ █▄█ █▀ █ █▀
█░▀░█ █▀█ █ █░▀█   █▀█ █░▀█ █▀█ █▄▄ ░█░ ▄█ █ ▄█

***********************************************/

**# OUTCOME 1: Overall survival
	* Stset
	stset uppfoljningstid, failure(dod==1) id(ID) scale(365.25)
	
	**# 1.1 KM curve
	sts test behandlingsgrupp1
	
	* Overall KM by treatment
	qui sts graph, by(behandlingsgrupp1) ///
		title("Overall survival by treatment group") ///
		xtitle("Years since diagnosis") ytitle("Survival probability") ///
		risktable(, order(1 "Obehandlad" 2 "Behandlad")) ///
		legend(order(1 "Obehandlad" 2 "Behandlad") ring(0) pos(5)) ///
		name(km_overall, replace)

	graph export "${result}km_os_overall.png", replace width(2400)

	* KM by treatment within each risk group
	forvalues r = 0/2 {
		preserve
			keep if riskgrupp == `r'

			qui sts graph, by(behandlingsgrupp1) ///
				title("Overall survival by treatment, risk group: `r'") ///
				xtitle("Years since diagnosis") ytitle("Survival probability") ///
				risktable(, order(1 "Obehandlad" 2 "Behandlad")) ///
				legend(order(1 "Obehandlad" 2 "Behandlad") ring(0) pos(5)) ///
				name(km_r`r', replace)

			graph export "${result}km_os_risk`r'.png", replace width(2400)
		restore
	}
	
	{
	** Additional analysis: export KM number to excel
		* stset
		stset uppfoljningstid, failure(dod==1) id(ID) scale(365.25)

		* Save KM estimates by treatment
		sts list, by(behandlingsgrupp1) ///
			saving("${temp}km_overall.dta", replace)
			
		* Overall
		preserve 
			use "${temp}km_overall.dta", clear
			gen riskgrupp = .
			tempfile km_all
			save `km_all', replace
		restore

		* Loop over risk groups
		forvalues r = 0/2 {
			preserve
				keep if riskgrupp == `r'
				sts list, by(behandlingsgrupp1) ///
					saving("${temp}km_r`r'.dta", replace)
				* Append
				use "${temp}km_r`r'.dta", clear
				gen riskgrupp = `r'
				append using `km_all'
				save `km_all', replace	
			restore
		}
		
		use `km_all', clear
		sort riskgrupp
		export excel using "${excel}/worktest_km_v01.xls", firstrow(variables) replace
	}
	
	**# 2.1 Cox
	* Compare linear vs flexible models
	use "${result}worktest_v01.dta", clear
	stset uppfoljningstid, failure(dod==1) id(ID) scale(365.25)
	
	stcox i.behandlingsgrupp1 c.alder1 i.kon1 i.T_grupp1 c.ht c.crp
	
	* Check non-linearity
	cap drop mart
	predict mart, mgale
	lowess mart alder1 // result: non-linear
	lowess mart ht // result: linear
	lowess mart crp // result: linear
	
	notes: age is non-linear, so use Restricted cubic splines
	
	* Stcox with spline age model
	mkspline age_s = alder1, cubic nknots(4)
	stcox i.behandlingsgrupp1 age_s* i.kon1 i.T_grupp1 c.ht c.crp
	estimates store cox_spline
	
	* Proportional hazards check
	estat phtest, detail
	
**# OUTCOME 2: Complication-free
	**# 2.1 KM curve
	stset cr_time, failure(failtype==1) id(ID) scale(365.25)
	sts test behandlingsgrupp1
	
	qui sts graph, by(behandlingsgrupp1) ///
		title("Complication free survival by treatment group") ///
		xtitle("Years since diagnosis") ytitle("Complication free survival") ///
		risktable(, order(1 "Obehandlad" 2 "Behandlad")) ///
		legend(order(1 "Obehandlad" 2 "Behandlad") ring(0) pos(5)) ///
		name(km_cfs, replace)
	graph export "${result}km_cfs_overall.png", replace width(2400)
	
	{
		** Additional analysis: export KM number to excel
		* stset
		stset cr_time, failure(failtype==1) id(ID) scale(365.25)

		* Save KM estimates by treatment
		sts list, by(behandlingsgrupp1) ///
			saving("${temp}km_cfs_overall.dta", replace)
			
		* Overall
		preserve 
			use "${temp}km_cfs_overall.dta", clear
			gen riskgrupp = .
			tempfile km_cfs_all
			save `km_cfs_all', replace
		restore

		* Loop over risk groups
		forvalues r = 0/2 {
			preserve
				keep if riskgrupp == `r'
				sts list, by(behandlingsgrupp1) ///
					saving("${temp}km_cfs_r`r'.dta", replace)
				* Append
				use "${temp}km_cfs_r`r'.dta", clear
				gen riskgrupp = `r'
				append using `km_cfs_all'
				save `km_cfs_all', replace	
			restore
		}
		
		use `km_cfs_all', clear
		sort riskgrupp
		export excel using "${excel}/worktest_km_cfs_v01.xlsx", firstrow(variables) replace
	}
	
	* Compare linear vs flexible models	
	use "${result}worktest_v01.dta", clear
	stset cr_time, failure(failtype==1) id(ID) scale(365.25)
	stcox i.behandlingsgrupp1 c.alder1 i.kon1 i.T_grupp1 c.ht c.crp
	cap drop mart
	predict mart, mgale
	lowess mart alder1
	lowess mart ht
	lowess mart crp
	
	**# 2.2 Cox
	mkspline age_s = alder1, cubic nknots(4)
	stcox i.behandlingsgrupp1 age_s* i.kon1 i.T_grupp1 c.ht c.crp
		
	* Cause specific Cox for complication with spline age
	stcox i.behandlingsgrupp1 age_s* i.kon1 i.T_grupp1 c.ht c.crp
	estimates store comp_cs_spline
	estat phtest, detail
	
	* Competing risks Fine and Gray for complication with death as competing event
	stcrreg i.behandlingsgrupp1 age_s* i.kon1 i.T_grupp1 c.ht c.crp, compete(failtype==2)
	estimates store comp_fg_spline
	
/***********************************

█▀ █▀▀ █▄░█ █▀ █ ▀█▀ █ █░█ █ ▀█▀ █▄█
▄█ ██▄ █░▀█ ▄█ █ ░█░ █ ▀▄▀ █ ░█░ ░█░

************************************/

**# Sensivity analysis 1: IPTW	
	use "${result}worktest_v01.dta", clear
	
	* Generate the propensity score (Denominator)
	logit behandlingsgrupp1 c.alder i.kon1 i.T_grupp1 c.ht c.crp
	predict double ps, pr

	* Check positivity and overlap
	summ ps if behandlingsgrupp1==0
	summ ps if behandlingsgrupp1==1
	
	scalar p_treat = r(mean)

	gen double sw = .
	replace sw = (p_treat/ps) if behandlingsgrupp1==1
	replace sw = ((1-p_treat)/(1-ps)) if behandlingsgrupp1==0
	label var sw "Stabilised IPTW"
	
	_pctile sw, p(1 99)
	scalar w_lo = r(r1)
	scalar w_hi = r(r2)

	gen double sw_tr = sw
	replace sw_tr = w_lo if sw_tr < w_lo
	replace sw_tr = w_hi if sw_tr > w_hi
	label var sw_tr "Stabilised IPTW truncated at p1 p99"
	
	* Check balance before and after weighting
	pstest c.alder i.kon1 i.T_grupp1 i.riskgrupp ht crp, both mw(sw_tr) treated(behandlingsgrupp1)

	**# OUTCOME 1: Overall survival, IPTW
		* Stcox with spline age model
		stset uppfoljningstid [pweight=sw_tr], failure(dod==1)
		
		mkspline age_s = alder1, cubic nknots(4)
		stcox i.behandlingsgrupp1 age_s* i.kon1 i.T_grupp1 c.ht c.crp
		
	**# OUTCOME 2: Overall survival, IPTW
		* Stcox with spline age model
		stset cr_time [pweight=sw_tr], failure(failtype==1) id(ID) scale(365.25)
		stcrreg i.behandlingsgrupp1 age_s* i.kon1 i.T_grupp1 c.ht c.crp, compete(failtype==2)
		
	
**# Sensivity analysis 2: Effect moderation of risk group
	use "${result}worktest_v01.dta", clear
	
	**# OUTCOME 1: Overall survival, effect modration
		* Stcox with spline age model
		stset uppfoljningstid, failure(dod==1)
		
		mkspline age_s = alder1, cubic nknots(4)
		stcox i.behandlingsgrupp1##i.riskgrupp i.kon1 c.ht c.crp
		
	**# OUTCOME 2: Overall survival, effect modration
		* Stcox with spline age model
		stset cr_time, failure(failtype==1) id(ID) scale(365.25)
		stcrreg i.behandlingsgrupp1##i.riskgrupp i.kon1 c.ht c.crp, compete(failtype==2)
		
	

	