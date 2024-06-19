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
	log using "${LOG_PATH}3_plot`TYPE'_`c(current_date)'.log", replace

	***************************************************************************
	* plots for cross-sectional development on district level 
	forval x = 2018/$maxyear {
		* load data
		use "${DATA_PATH}kreis_pindex_`TYPE'_reg2_`x'.dta", clear 

		cap drop obid
		duplicates drop
		tostring kid2019, gen(RS) // RS = variable with AGS + zero in Kreis shape file
		replace RS = "0" + RS if strlen(RS) == 4
		merge 1:m RS using "${SHAPE_PATH}Kreis/2019/VG250_KRS" 
		drop if _merge < 3

		* drop if GF ==2
		drop _merge
		duplicates drop id, force
		
		* price index with anonymisation threshold
		gen pindex_o50 = pindex
		replace pindex_o50 =. if freq_kreisid < 50 

		format pindex_o50 %5.1f
		format pindex %5.1f

		spmap pindex_o50 using "${SHAPE_PATH}Kreis\2019\VG250_KRS_coords" ///
		, id(id)  clmethod(quantile) clnumber(5) fcolor (YlOrRd) legstyle(1) ocolor(Reds) ///
		title("`TYPE' in `x' reg 2, kreis") osize(none) ndocolor(erose)  ndsize(vvthin) ndlabel("no inhabitants") ///
		polygon(data("${SHAPE_PATH}Bundesland\2015\VG250_LAN_coords.dta") osize(thin) oc(black)) ///
		legend(pos(3) /// position of legend at 11 o'clock ///
		ring(1) /// legend within plot region 
		col(1)  /// legend has 1 column only
		size(vsmall)) // legend text size small

		* export
		graph save "${OUTPUT_PATH}kreis_`TYPE'_`x'", replace
		graph export "${OUTPUT_PATH}kreis_`TYPE'_`x'.png", replace
	}

	***************************************************************************
	* plots for cross-sectional development on municipality level
	forval x = 2018/$maxyear {
		* load data
		use "${DATA_PATH}gemeinde_pindex_`TYPE'_reg2_`x'.dta", clear 
		
		cap drop obid
		duplicates drop
		
		* adjust municipality ID and merge shape file information
		tostring gid2019, gen(RS)
		replace RS = "0" + RS if strlen(RS)== 8
		merge 1:m RS using "${SHAPE_PATH}Verwaltungsgemeinschaften/2019/VG250_VWG" 	
		drop if _merge < 3
		drop _merge
		
		duplicates drop id, force
		
		* price index with anonymisation
		gen pindex_o50 = pindex
		replace pindex_o50 =. if freq_gemeindeid < 50 
		format pindex_o50 %5.1f
		format pindex %5.1f

		* plot price index with anonymisation
		spmap pindex_o50 using "${SHAPE_PATH}Verwaltungsgemeinschaften/2019/VG250_VWG_coords" ///
		, id(id)  clmethod(quantile) clnumber(5) fcolor (YlOrRd) legstyle(1) ocolor(Reds) ///
		title("`TYPE' in `x', reg 2, vwg") osize(none) ndocolor(erose)  ndsize(vvthin) ndlabel("no inhabitants") ///
		polygon(data("${SHAPE_PATH}Kreis\2019\VG250_KRS_coords.dta") osize(vthin)) /// Kreisgrenze
		line(data("${SHAPE_PATH}Bundesland\2015\VG250_LAN_coords.dta") size(thin) color(black)) /// Bundeslandgrenze
		legend(pos(3) /// position of legend at 3 o'clock ///
		ring(1) /// legend within plot region   
		col(1) /// legend has 1 column only
		size(vsmall))  // legend text size small
		
		* export
		graph save "${OUTPUT_PATH}gemeinde_`TYPE'_`x'", replace
		graph export "${OUTPUT_PATH}gemeinde_`TYPE'_`x'.png", replace

		* plot price index
		spmap pindex using "${SHAPE_PATH}Verwaltungsgemeinschaften/2019/VG250_VWG_coords" ///
		, id(id)  clmethod(quantile) clnumber(5) fcolor (YlOrRd) legstyle(1) ocolor(Reds) ///
		title("`TYPE' in `x', reg 2, vwg") osize(none) ndocolor(erose)  ndsize(vvthin) ndlabel("no inhabitants") ///
		polygon(data("${SHAPE_PATH}Kreis\2019\VG250_KRS_coords.dta") osize(vthin)) /// Kreisgrenze
		line(data("${SHAPE_PATH}Bundesland\2019\VG250_LAN_coords.dta") size(thin) color(black)) /// Bundeslandgrenze
		legend(pos(3) /// position of legend at 3 o'clock ///
		ring(1) /// legend within plot region   
		col(1) /// legend has 1 column only
		size(vsmall))  // legend text size small

		* export
		graph save "${OUTPUT_PATH}gemeinde_SUF_`TYPE'_`x'", replace
		graph export "${OUTPUT_PATH}gemeinde_SUF_`TYPE'_`x'.png", replace
	}

	***************************************************************************
	* plots for Change in Interaction-Regression 3 on district level
	
	* read data
	use "${DATA_PATH}kreis_pindex_`TYPE'_ejahr3.dta", clear
	
	cap drop obid
	duplicates drop
	sort kid ejahr
	xtset kid ejahr, yearly
	
	* calculate price change
	gen pindex_change = pindex- L${diff}.pindex 
	gen freq_kreisid_ok = 1 if ejahr == $maxyear2
	
	* only include districts +50 observation in both researched years // help variable freq_kreisid_ok to check
	replace freq_kreisid_ok = 0 if (freq_kreisid < 50 | L${diff}.freq_kreisid < 50) & ejahr == $maxyear2
	keep if ejahr == $maxyear2
	tostring kid2019, gen(RS)
	replace RS = "0"+RS if strlen(RS)== 4
	gen pindex_changeo50 = pindex_change
	replace pindex_changeo50 =. if freq_kreisid_ok==0

	* merge shape information
	merge m:m RS  using "${SHAPE_PATH}Kreis\2019\VG250_KRS" //310 not used from using file
	drop if _merge < 3
	format pindex_changeo50 %5.1f
	
	* generate plot
	spmap pindex_changeo50 ///
	using "${SHAPE_PATH}Kreis\2019\VG250_KRS_coords" ///
	, id(id)  clmethod(quantile) clnumber(5) fcolor (YlOrRd) legstyle(1) ocolor(Reds) ///
	title("`TYPE' in `x', reg 3, kreis") osize(none) ndocolor(erose)  ndsize(vvthin) ndlabel("no inhabitants") ///
	polygon(data("${SHAPE_PATH}Bundesland\2015\VG250_LAN_coords.dta") osize(thin) ocolor(black)) /// Bundeslandgrenze
	legend(pos(3) /// position of legend at 11 o'clock ///
	ring(1) /// legend within plot region   
	col(1) /// legend has 1 column only
	size(vsmall)) // legend text size 

	* export
	graph save "${OUTPUT_PATH}kreis_change_`TYPE'", replace
	graph export "${OUTPUT_PATH}kreis_change_`TYPE'.png", replace

	***************************************************************************
	* plots for change in interaction-regression 3 on municipality level
	* same procedure as before
	
	* read data
	use "${DATA_PATH}gemeinde_pindex_`TYPE'_ejahr3.dta", clear
	
	cap drop obid
	duplicates drop
	sort gid ejahr
	xtset gid ejahr, yearly
	
	* calculate change
	gen pindex_change = pindex - L${diff}.pindex 
	
	gen freq_gemeindeid_ok = 1 if ejahr == $maxyear2
	replace freq_gemeindeid_ok = 0 if (freq_gemeindeid < 50 | L${diff}.freq_gemeindeid < 50) & ejahr == $maxyear2
	keep if ejahr == $maxyear2
	tostring gid2019, gen(RS)
	replace RS = "0"+RS if strlen(RS)== 8
	
	* merge shape files
	merge 1:m RS using "${SHAPE_PATH}Verwaltungsgemeinschaften/2019/VG250_VWG" 	
	gen pindex_changeo50 = pindex_change
	replace pindex_changeo50 =. if freq_gemeindeid_ok == 0
	drop if _merge <3
	drop _merge

	format pindex_changeo50 %5.1f
	
	* generate plot
	spmap pindex_changeo50 ///
	using "${SHAPE_PATH}Verwaltungsgemeinschaften/2019/VG250_VWG_coords" ///
	, id(id)  clmethod(quantile) clnumber(5) fcolor(YlOrRd) legstyle(1) ocolor(Reds) ///
	title("`TYPE' in `x', reg 3, vwg") osize(none) ndocolor(erose)  ndsize(vvthin) ndlabel("no inhabitants") ///
	polygon(data("${SHAPE_PATH}Kreis\2019\VG250_KRS_coords.dta") osize(vthin)) /// Kreisgrenze
	line(data("${SHAPE_PATH}Bundesland\2015\VG250_LAN_coords.dta") size(thin) color(black)) /// Bundeslandgrenze
	legend(pos(3) /// position of legend at 3 o'clock
	ring(1) /// legend within plot region   
	col(1) /// legend has 1 column only
	size(vsmall)) // legend text size small

	* export
	graph save "${OUTPUT_PATH}gemeinde_change_`TYPE'", replace 
	graph export "${OUTPUT_PATH}gemeinde_change_`TYPE'.png", replace 

	***************************************************************************
	* finish
	log close
}
