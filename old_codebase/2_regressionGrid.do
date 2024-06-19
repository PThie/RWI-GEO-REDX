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
global TYPES "WK HK WM"

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
	log using "${LOG_PATH}2_regressionGRID_`TYPE'_`c(current_date)'.log", append


	* read prepared data
	use "${DATA_PATH}1_prepare_RWIGEOREDX_`TYPE'.dta", clear 

	* drop very last month in the sample to prevent look ahead bias
	drop if ejahr == ${maxyear} & emonat == ${maxmonth}
keep if kid2019 == 11000 |kid2019 == 2000 |kid2019 == 6412|kid2019 == 5111 |kid2019 == 5112 |kid2019 == 5113 |kid2019 == 5315 |kid2019 == 5913 |kid2019 == 3241 |kid2019 == 4011|kid2019== 8111|kid2019 ==9162|kid2019==9564|kid2019 ==14612|kid2019==14713

	sum qudate if ejahr! = 2007
	global minqudate = r(min)
	global maxqudate = r(max)

	sum ejahr if ejahr! = 2007
	global minejahr = r(min)
	global maxejahr = r(max)

	***************************************************************************
	* perform regressions
	
	foreach region of varlist grid {
		foreach time of varlist ejahr {

			*Regression 2 
			*ejahr ganz
			*qudate last quarter
			display "regression 2, `time', `region'"
			if "`time'" == "ejahr" {
				forval x = 2008/$maxyear{
					preserve //to save data during loop
					drop if `time'!=`x' //only review 1 year
					//FE Kreise regression with mean adjusted flatprice/sqm and its effects
					xtreg ${dep`TYPE'} ${var`TYPE'}, fe i(`region') robust
					predict pindex_FE if e(sample), u //predict/save residuals from regression, but only if (full) observation is used in the estimation
					keep if e(sample) //keep the observation only if used
					su ${dep`TYPE'}
					foreach var of global var2`TYPE' {
						 su `var', d
						replace `var' = r(p50)
						display "`var'"
					}
					predict median
					gen median2 = median + pindex_FE
					keep `region' `time' m${price`TYPE'}sqm_`region'_`time' pindex_FE pindex median median2 ergg_1km kid2019 //keep the info on Kreis, year and price index
					bysort `region': gen freq_${dname`region'}id = _N 
					gen pindex = 100 * (exp(pindex_FE) - 1)
					duplicates drop
					save "${DATA_PATH}${dname`region'}_pindex_`TYPE'_reg2_`x'.dta", replace //save data with Info on Kreis, year, price index and Noobs in Kreis
					restore //restore data from the beginnign of the loop
				}
			} 


		}
	}
	***************************************************************************
	* finish
	
	log close
	exit, clear
}
