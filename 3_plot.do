
cap  log close
global TYPES "WK WM HK"


foreach TYPE of global TYPES {

global ORIG_PATH "M:/_FDZ/RWI-GEO/RWI-GEO-RED/daten/" 
global DATA_PATH "M:/_FDZ/RWI-GEO/RWI-GEO-REDX/output/`TYPE'/data/$month/" 
global LOG_PATH "M:/_FDZ/RWI-GEO/RWI-GEO-REDX/output/`TYPE'/log/$month/"
global PROG_PATH "M:/_FDZ/RWI-GEO/RWI-GEO-REDX/aufbereitung/$version/"
global OUTPUT_PATH "M:/_FDZ/RWI-GEO/RWI-GEO-REDX/output/`TYPE'/output/$month/"
global SHAPE_PATH "M:/_FDZ/interne Daten/Gebietseinheit/"



log using "${LOG_PATH}3_plot`TYPE'_`c(current_date)'.log", replace

*Auswertungsprogramm des Projektes "Immobilienpreisindices RWI-GEO-REDX"
*Inhalt: Erstellung der Indices über FE auf Kreis, Gemeinde und Gridebene
*Erstellung der Datensatze 
*Datum 18.1.2019

*generate Dta-Geo Files if they do not already exist
*do "${PROG_PATH}3a_prepare_plot.do"


*Plots for Cross-sectional Development on district level 
forval x = 2018/$maxyear {

use "${DATA_PATH}kreis_pindex_`TYPE'_reg2_`x'.dta", clear 

cap drop obid
duplicates drop
tostring kid2019, gen(RS) // RS = variable with AGS + zero in Kreis shape file
replace RS = "0" + RS if strlen(RS) == 4
merge 1:m RS using "${SHAPE_PATH}Kreis/2019/VG250_KRS" 
drop if _merge<3

*drop if GF ==2
drop _merge
duplicates drop id, force
gen pindex_o50 = pindex
replace pindex_o50 =. if freq_kreisid<50 

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
*graph play "map Germany ohne Titel" // legend "between" Brandenburg and Bavaria  at 4 o'clock
graph save "${OUTPUT_PATH}kreis_`TYPE'_`x'", replace
graph export "${OUTPUT_PATH}kreis_`TYPE'_`x'.png", replace
}

*Plots for Cross-sectional Development on municipality level 
forval x = 2018/$maxyear {
use "${DATA_PATH}gemeinde_pindex_`TYPE'_reg2_`x'.dta", clear 
cap drop obid
duplicates drop
tostring gid2019, gen(RS)
replace RS = "0"+RS if strlen(RS)== 8
merge 1:m RS using "${SHAPE_PATH}Verwaltungsgemeinschaften/2019/VG250_VWG" 	
drop if _merge<3
drop _merge
duplicates drop id, force
gen pindex_o50 = pindex
replace pindex_o50 =. if freq_gemeindeid<50 
format pindex_o50 %5.1f
format pindex %5.1f

spmap pindex_o50 using "${SHAPE_PATH}Verwaltungsgemeinschaften/2019/VG250_VWG_coords" ///
, id(id)  clmethod(quantile) clnumber(5) fcolor (YlOrRd) legstyle(1) ocolor(Reds) ///
title("`TYPE' in `x', reg 2, vwg") osize(none) ndocolor(erose)  ndsize(vvthin) ndlabel("no inhabitants") ///
polygon(data("${SHAPE_PATH}Kreis\2019\VG250_KRS_coords.dta") osize(vthin)) /// Kreisgrenze
line(data("${SHAPE_PATH}Bundesland\2015\VG250_LAN_coords.dta") size(thin) color(black)) /// Bundeslandgrenze
legend(pos(3) /// position of legend at 3 o'clock ///
ring(1) /// legend within plot region   
col(1) /// legend has 1 column only
size(vsmall))  // legend text size small
*graph play "map Germany ohne Titel" // legend "between" Brandenburg and Bavaria  at 4 o'clock
graph save "${OUTPUT_PATH}gemeinde_`TYPE'_`x'", replace
graph export "${OUTPUT_PATH}gemeinde_`TYPE'_`x'.png", replace

spmap pindex using "${SHAPE_PATH}Verwaltungsgemeinschaften/2019/VG250_VWG_coords" ///
, id(id)  clmethod(quantile) clnumber(5) fcolor (YlOrRd) legstyle(1) ocolor(Reds) ///
title("`TYPE' in `x', reg 2, vwg") osize(none) ndocolor(erose)  ndsize(vvthin) ndlabel("no inhabitants") ///
polygon(data("${SHAPE_PATH}Kreis\2019\VG250_KRS_coords.dta") osize(vthin)) /// Kreisgrenze
line(data("${SHAPE_PATH}Bundesland\2019\VG250_LAN_coords.dta") size(thin) color(black)) /// Bundeslandgrenze
legend(pos(3) /// position of legend at 3 o'clock ///
ring(1) /// legend within plot region   
col(1) /// legend has 1 column only
size(vsmall))  // legend text size small
*graph play "map Germany ohne Titel" // legend "between" Brandenburg and Bavaria  at 4 o'clock

graph save "${OUTPUT_PATH}gemeinde_SUF_`TYPE'_`x'", replace
graph export "${OUTPUT_PATH}gemeinde_SUF_`TYPE'_`x'.png", replace
}

*Plots for Change in Interaction-Regression 3 on district level
use "${DATA_PATH}kreis_pindex_`TYPE'_ejahr3.dta", clear
cap drop obid
duplicates drop
sort kid ejahr
xtset kid ejahr, yearly
gen pindex_change = pindex- L${diff}.pindex 
gen freq_kreisid_ok = 1 if ejahr==$maxyear2
*only include districts +50 observation in both researched years // help variable freq_kreisid_ok to check
replace freq_kreisid_ok = 0 if (freq_kreisid<50 | L${diff}.freq_kreisid<50) & ejahr==$maxyear2
keep if ejahr == $maxyear2
tostring kid2019, gen(RS)
replace RS = "0"+RS if strlen(RS)== 4
gen pindex_changeo50 = pindex_change
replace pindex_changeo50 =. if freq_kreisid_ok==0

merge m:m RS  using "${SHAPE_PATH}Kreis\2019\VG250_KRS" //310 not used from using file
drop if _merge < 3
format pindex_changeo50 %5.1f
spmap pindex_changeo50 ///
using "${SHAPE_PATH}Kreis\2019\VG250_KRS_coords" ///
, id(id)  clmethod(quantile) clnumber(5) fcolor (YlOrRd) legstyle(1) ocolor(Reds) ///
title("`TYPE' in `x', reg 3, kreis") osize(none) ndocolor(erose)  ndsize(vvthin) ndlabel("no inhabitants") ///
polygon(data("${SHAPE_PATH}Bundesland\2015\VG250_LAN_coords.dta") osize(thin) ocolor(black)) /// Bundeslandgrenze
legend(pos(3) /// position of legend at 11 o'clock ///
ring(1) /// legend within plot region   
col(1) /// legend has 1 column only
size(vsmall)) // legend text size 
*graph play "map Germany ohne Titel" // legend "between" Brandenburg and Bavaria  at 4 o'clock

graph save "${OUTPUT_PATH}kreis_change_`TYPE'", replace
graph export "${OUTPUT_PATH}kreis_change_`TYPE'.png", replace

*Plots for Change in Interaction-Regression 3 on municipality level
use "${DATA_PATH}gemeinde_pindex_`TYPE'_ejahr3.dta", clear
cap drop obid
duplicates drop
sort gid ejahr
xtset gid ejahr, yearly
gen pindex_change = pindex- L${diff}.pindex 
*see above, plot for districts
gen freq_gemeindeid_ok = 1 if ejahr==$maxyear2
replace freq_gemeindeid_ok = 0 if (freq_gemeindeid<50 | L${diff}.freq_gemeindeid<50) & ejahr==$maxyear2
keep if ejahr == $maxyear2
tostring gid2019, gen(RS)
replace RS = "0"+RS if strlen(RS)== 8
merge 1:m RS using "${SHAPE_PATH}Verwaltungsgemeinschaften/2019/VG250_VWG" 	
gen pindex_changeo50 = pindex_change
replace pindex_changeo50 =. if freq_gemeindeid_ok==0
drop if _merge <3
drop _merge

format pindex_changeo50 %5.1f
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
*graph play "map Germany ohne Titel" // legend "between" Brandenburg and Bavaria  at 4 o'clock

graph save "${OUTPUT_PATH}gemeinde_change_`TYPE'", replace 
graph export "${OUTPUT_PATH}gemeinde_change_`TYPE'.png", replace 

log close
}
