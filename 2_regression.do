*******************************************************************************
* Description

/*
This file runs the regression which generate the values for the price index.
*/

*******************************************************************************
* set up

* close all log files
cap log close

* define all types
global TYPES "WM HK WK"

*******************************************************************************
* loop through all housing types

foreach TYPE of global TYPES {
	***************************************************************************
	* paths
	
	global DATA_PATH "M:/_FDZ/RWI-GEO/RWI-GEO-REDX/output/`TYPE'/data/$month/" 
	global LOG_PATH "M:/_FDZ/RWI-GEO/RWI-GEO-REDX/output/`TYPE'/log/$month/"
	global OUTPUT_PATH "M:/_FDZ/RWI-GEO/RWI-GEO-REDX/output/`TYPE'/output/$month/"

	***************************************************************************
	* set up
	
	* set up log file
	log using "${LOG_PATH}2_regression_`TYPE'_`c(current_date)'.log", append

	/* DELETE?
	global exWM "i.constr first_occupancy  i.balkon i.garten i.einbaukueche i.gaestewc i.keller i.ausstattung zimmeranzahl_full"
	global depWM "ln_rent_sqm"
	global exHK "i.constr first_occupancy i.gaestewc einliegerwohnung i.ausstattung zimmeranzahl_full i.plotarea_size typ_*"
	global depHK "ln_houseprice_sqm"
	global exWK "i.constr first_occupancy  i.balkon i.garten i.einbaukueche i.gaestewc i.aufzug i.keller betreut i.ausstattung declared_wohngeld zimmeranzahl_full nofloors catfloors"
	global depWK "ln_flatprice_sqm"

	global dnamekid2015 "kreis"
	global dnamegid2015 "gemeinde"
	global dnamegrid "grid"
	global dnameAMR2 "AMR2"
	*/

	* read prepared data
	use "${DATA_PATH}1_prepare_RWIGEOREDX_`TYPE'.dta", clear 

	* drop very last month in the sample to prevent look ahead bias
	drop if ejahr == ${maxyear} & emonat == ${maxmonth}


	sum qudate if ejahr! = 2007
	global minqudate = r(min)
	global maxqudate = r(max)

	sum ejahr if ejahr! = 2007
	global minejahr = r(min)
	global maxejahr = r(max)

	***************************************************************************
	* perform regressions
	
	foreach region of varlist kid2019 gid2019 AMR2 {
		foreach time of varlist ejahr qudate modate {

			*Regression 1
			preserve 
			xtreg ${dep`TYPE'} ${ex`TYPE'} i.`time' if ejahr != 2007, fe i(`region') robust
			estimate store m1 
			estout m1 using "${DATA_PATH}`region'_timeeff_`TYPE'_`time'_reg1.csv", deli(";") cells("b ci_l ci_u") replace
			predict pindex_FE, u
			keep `region' `time' m${price`TYPE'}sqm_`region'_`time' pindex_FE
			duplicates drop
			gen pindex = 100*(exp(pindex_FE)-1) //"delogarithmise"
			compress
			save "${DATA_PATH}`region'_`TYPE'_`time'1.dta", replace
			restore

			if "`time'" == "ejahr" | "`time'" == "qudate" {
				*Regession 3, Time and Regional Interaction
				display "regression 3, `time', `region'"
				preserve
				drop if ejahr == 2007
				xtset id_`region'_`time'
				xtreg ${dep`TYPE'} ${ex`TYPE'}, fe i(id_`region'_`time') robust
				keep if e(sample)
				su ${dep`TYPE'}
				predict pindex_FE, u 
				keep `region' `time' m${price`TYPE'}sqm_`region'_`time' pindex_FE
				bysort `region' `time': gen freq_${dname`region'}id = _N //noobs per Kreis
				gen pindex = 100 * (exp(pindex_FE) - 1) //"delogarithmise"
				duplicates drop
				compress
				save "${DATA_PATH}${dname`region'}_pindex_`TYPE'_`time'3.dta", replace
				restore 
			}

			/* DELETE BECAUSE EMPTY?
			else if "`time'" == "modate" {
			}
			*/

			*Regression 2 
			*ejahr ganz
			*qudate last quarter
			display "regression 2, `time', `region'"
			if "`time'" == "ejahr" {
				forval x = 2008/$maxyear{
					preserve //to save data during loop
					drop if `time'!=`x' //only review 1 year
					//FE Kreise regression with mean adjusted flatprice/sqm and its effects
					xtreg ${dep`TYPE'} ${ex`TYPE'}, fe i(`region') robust
					predict pindex_FE if e(sample), u //predict/save residuals from regression, but only if (full) observation is used in the estimation
					keep if e(sample) //keep the observation only if used
					su ${dep`TYPE'}
					keep `region' `time' m${price`TYPE'}sqm_`region'_`time' pindex_FE //keep the info on Kreis, year and price index
					bysort `region': gen freq_${dname`region'}id = _N //noobs per Kreis
					gen pindex = 100 * (exp(pindex_FE) - 1)
					duplicates drop
					save "${DATA_PATH}${dname`region'}_pindex_`TYPE'_reg2_`x'.dta", replace //save data with Info on Kreis, year, price index and Noobs in Kreis
					restore //restore data from the beginnign of the loop
				}
			} 


			*alternative for all quarters and all month: 

			else if "`time'" == "qudate" & ("`region'" == "kid2019" | "`region'" == "AMR2") { 

				* estimation for different quarters back in the past
				local maxqudatemin20 = $maxqudate -20
				local maxqudatemin8 = $maxqudate  -8
				local maxqudatemin4 = $maxqudate  -4
				local maxqudatemin3 = $maxqudate  -3
				local maxqudatemin2 = $maxqudate  -2
				local maxqudatemin1 = $maxqudate  -1
				 
				foreach x in `maxqudatemin20' `maxqudatemin8' `maxqudatemin4' `maxqudatemin3' `maxqudatemin2' `maxqudatemin1' $maxqudate {
					preserve
					* keep if ejahr !=${maxyear2} & emonat!= ${maxmonth}
					keep if qudate == `x'
					//FE Kreise regression with price/sqm 
					xtreg ${dep`TYPE'} ${ex`TYPE'}, fe i(`region') robust
					predict pindex_FE if e(sample), u //predict/save residuals from regression, but only if (full) observation is used in the estimation
					keep if e(sample) //keep the observation only if used
					su ${dep`TYPE'}
					keep `region' `time' m${price`TYPE'}sqm_`region'_`time' pindex_FE //keep the info on Kreis, year and price index
					bysort `region' `time': gen freq_${dname`region'}id = _N //noobs per Kreis
					gen pindex = 100 * (exp(pindex_FE) - 1) //"delogarithmise"
					duplicates drop
					compress //reduce data 
					save "${DATA_PATH}${dname`region'}_pindex_`TYPE'_reg2_`time'_`x'.dta", replace //save data with Info on Kreis, year, price index and Noobs in Kreis
					restore //restore data from the beginnign of the loop
				}
			}

			/* DELETE BECAUSE EMPTY?
			else if "`time'" == "modate" {
			}
			*/
		}
	}
	***************************************************************************
	* finish
	
	log close
	exit, clear
}
