*******************************************************************************
* Description

/*
This file plots the generate price index (mainly for checking and spotting 
potential errors).

NOTE: Run aux_prepare_plot.do if shape files do not exist (shape files for:
states, districts, and municipalities (VWG))
*/

*******************************************************************************
* set up

* close all log files
cap log close

* define all types
global TYPES " WM WK HK"

*******************************************************************************
* loop through all housing types

foreach TYPE of global TYPES {

	***************************************************************************
	* paths
	
	global DATA_PATH "DATA_PATH" 
	global LOG_PATH "LOG_PATH"
	global OUTPUT_PATH "OUTPUT_PATH"

	***************************************************************************
	* set up
	
	* set up log file
	log using "${LOG_PATH}3_plot`TYPE'_`c(current_date)'.log", replace

	***************************************************************************
	* plots for cross-sectional development on district level 
	forval x = 2009/$maxyear {
		* load data
		use "${DATA_PATH}grid_pindex_`TYPE'_reg2_`x'.dta", clear 
		
		cap drop obid
		duplicates drop
		rename ergg_1km IDm
		drop if IDm == "-9"
		merge 1:m IDm using "${SHAPE_PATH}Raster\grid_stata\grids_projectionde_1km.dta"
		drop if _merge < 3

		* drop if GF ==2
		drop _merge
		duplicates drop g_id, force
		
		gen mean_price =exp(median2)
		
		* price index with anonymisation threshold
		gen pindex_o50 = pindex
		replace pindex_o50 =. if freq_gridid < 50 

		format pindex_o50 %5.1f
		format pindex %5.1f

		spmap pindex_o50 using "${SHAPE_PATH}Raster\grid_stata\grids_projectionde_1km_coord" ///
		, id(g_id)  clmethod(quantile) clnumber(5) fcolor (YlOrRd) legstyle(1) ocolor(Reds) ///
		title("`TYPE' in `x' reg 2, kreis") osize(none) ndocolor(erose)  ndsize(vvthin) ndlabel("no inhabitants") ///
		polygon(data("${SHAPE_PATH}Bundesland\2015\VG250_LAN_coords.dta") osize(thin) oc(black)) ///
		legend(pos(3) /// position of legend at 11 o'clock ///
		ring(1) /// legend within plot region 
		col(1)  /// legend has 1 column only
		size(vsmall)) 
		
				graph save "${OUTPUT_PATH}grid`TYPE'_`x'", replace
		graph export "${OUTPUT_PATH}grid_`TYPE'_`x'.png", replace
		
		
		foreach z of numlist 11000  2000 6412 5111 5112  5113 5315 5913  3241 4011 8111 9162 9564 14612 14713 {

		spmap mean_price using "${SHAPE_PATH}Raster\grid_stata\grids_projectionde_1km_coord" if freq_gridid >=35 & kid2019 == `z' ///
				, id(g_id)  clmethod(quantile) clnumber(5) fcolor (YlOrRd) legstyle(1) ocolor(Reds) 
				
	

		* export
		graph save "${OUTPUT_PATH}gridM`z'`TYPE'_`x'", replace
		graph export "${OUTPUT_PATH}gridM`z'_`TYPE'_`x'.png", replace
	}	
		
		
	}


	***************************************************************************
	* finish
	log close
}
