*******************************************************************************
* Description

/*
This file exports the price indices in excel file.
*/

*******************************************************************************
* set up

* close all log files
cap log close
clear all

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
	* loop through file type, regions, and timing
	foreach UF of newlist PUF SUF {
		foreach region of newlist kid2019 gid2019 AMR2{
			foreach time of newlist ejahr qudate{
				* for districts
				if "`region'"== "kid2019" {
					* Export Time-Effect from Regression 1kid2019_timeeff_HK_ejahr_reg1
					import delimited "${DATA_PATH}`region'_timeeff_`TYPE'_`time'_reg1.csv", delimiter(";") asfloat clear
					rename v1 `time'
					rename v2 timeeff
					rename v3 timeeff_p025
					rename v4 timeeff_p975
					
					keep if strpos(`time', "`time'")
					split `time', p(.)
					drop `time' `time'2
					rename `time'1 `time'
					order `time', first
					
					drop if timeeff == "b"
					drop if timeeff == "m1"
					
					* in case of public use file
					if "`UF'"== "PUF" {
						if "`time'" == "ejahr"{
							rename `time' year
							export excel using "${OUTPUT_PATH}RWIGEOREDX_${`TYPE'name}_${version}.xlsx", sheet("1__District_TimeEff_yearly") sheetreplace firstrow(variables)
						}
						else if "`time'" == "qudate"{
							rename `time' quarter
							destring quarter, replace
							format %tq quarter
							export excel using "${OUTPUT_PATH}RWIGEOREDX_${`TYPE'name}_${version}.xlsx", sheet("1__District_TimeEff_quarterly") sheetreplace firstrow(variables)
						}
					}
					
					* in case of scientific use file
					if "`UF'"== "SUF" {
						if "`time'" == "ejahr"{
							rename `time' year
							export excel using "${OUTPUT_PATH}RWIGEOREDX_${`TYPE'name}_${version}_SUF.xlsx", sheet("1__District_TimeEff_yearly") sheetreplace firstrow(variables)
						}
						else if "`time'" == "qudate"{
							rename `time' quarter
							destring quarter, replace
							format %tq quarter
							export excel using "${OUTPUT_PATH}RWIGEOREDX_${`TYPE'name}_${version}_SUF.xlsx", sheet("1__District_TimeEff_quarterly") sheetreplace firstrow(variables)
						}
					}
				}
			
				/* DELETE becuase its empty?
				else if "`region'"== "gid2019"{
				}
				else if "`region'"== "AMR2" {
				}
				*/

				* regression 2 
				* in case of yearly data
				if "`time'" == "ejahr"{

					* load data
					use "${DATA_PATH}${dname`region'}_pindex_`TYPE'_reg2_2008.dta", clear

					* in case of districts
					if "`region'" == "kid2019" {
						tostring kid2019, gen(RS)
						replace RS = "0" + RS if strlen(RS) == 4
						merge 1:m RS using "${SHAPE_PATH}Kreis/2019/VG250_KRS", keepusing(GEN GF)
						drop if GF == 2
						rename GEN name
						rename RS AGS_${ename`region'}
						drop _merge GF
					}
					* in case of municipalities
					else if "`region'"== "gid2019" {
						tostring gid2019, gen(RS)
						replace RS = "0" + RS if strlen(RS) == 8
						merge 1:m RS using "${SHAPE_PATH}Verwaltungsgemeinschaften/2019/VG250_VWG", keepusing(GEN GF)	
						drop if GF == 2
						rename GEN name
						rename RS AGS_${ename`region'}
						drop _merge GF
						destring AGS_${ename`region'}, gen(help)
						replace gid2019 = help if gid2019 == . & help > 0
						drop help
					}
					/* DELETE because its empty ?
					else if "`region'"== "AMR2" {
					}
					*/
					rename freq_ freq2008
					bys `region': replace pindex = . if freq < ${no`UF'}
					rename pindex pindex2008

					if "`region'" == "kid2019" {
						order AGS_${ename`region'}, after(`region')
						order name, after(AGS_${ename`region'})
						keep `region' AGS_${ename`region'} name pindex2008 freq2008
					}
					else if "`region'" == "gid2019" {
						order AGS_${ename`region'}, after(`region')
						order name, after(AGS_${ename`region'})
						keep `region' AGS_${ename`region'} name pindex2008 freq2008
					}
					else if "`region'" == "AMR2" {
						keep `region' pindex2008 freq2008
					}

					forval x = 2009/$maxyear{
						merge 1:1 `region' using ${DATA_PATH}${dname`region'}_pindex_`TYPE'_reg2_`x'.dta, keepusing(pindex freq_)
						drop _merge 
						bysort `region': replace pindex=. if freq_ < ${no`UF'}
						rename pindex pindex`x'
						rename freq_ freq`x'
					}

					if "`region'" == "kid2019" {
						drop kid2019
						local i = 2
					}
					else if "`region'" == "gid2019" {
						drop gid2019 
						local i = 4
					}

					else if "`region'"== "AMR2" {
						local i = 6
					}
					
					if "`UF'" == "PUF" {
						export excel using "${OUTPUT_PATH}RWIGEOREDX_${`TYPE'name}_${version}.xlsx", sheet("`i'__${ename`region'}_Pindex_yearly") sheetreplace firstrow(variables)
						save "${OUTPUT_PATH}`i'__${ename`region'}_Pindex_`TYPE'_yearly.dta", replace 
					}
					else if "`UF'" == "SUF" {
						export excel using "${OUTPUT_PATH}RWIGEOREDX_${`TYPE'name}_${version}_SUF.xlsx", sheet("`i'__${ename`region'}_Pindex_yearly") sheetreplace firstrow(variables)
						save "${OUTPUT_PATH}`i'__${ename`region'}_Pindex_`TYPE'_yearlyS.dta", replace 
					}
				}

				else if "`time'" == "qudate" & "`region'" == "AMR2" | "`region'" == "kid2019" {

					* load data	
					use "${DATA_PATH}${dname`region'}_pindex_`TYPE'_reg2_qudate_${maxqudate}.dta", clear


					drop pindex_ m${price`TYPE'}
					rename freq_ freq
					bysort `region': replace pindex=. if freq<${no`UF'}

					format %tq qudate
					rename qudate quarter

					if "`region'" == "kid2019" {
						tostring kid2019, gen(RS)
						replace RS = "0" + RS if strlen(RS) == 4
						merge 1:m RS using "${SHAPE_PATH}Kreis/2019/VG250_KRS", keepusing(GEN GF)
						drop if GF == 2
						drop if _merge < 3
						rename GEN name
						rename RS AGS_${ename`region'}

						order AGS_${ename`region'}, after(`region')
						order name, after(AGS_${ename`region'})
						drop _merge GF `region'

						local i "2"
					}


					if "`region'" == "AMR2" {
						local i "6"
					}

					if "`UF'" == "PUF" {
						export excel using "${OUTPUT_PATH}RWIGEOREDX_${`TYPE'name}_${version}.xlsx", sheet("`i'__${ename`region'}_Pindex_lastquarter") sheetreplace firstrow(variables)
						save "${OUTPUT_PATH}`i'__${ename`region'}_Pindex_lastquarter", replace 
					}
					if "`UF'" == "SUF" {
						export excel using "${OUTPUT_PATH}RWIGEOREDX_${`TYPE'name}_${version}_SUF.xlsx", sheet("`i'__${ename`region'}_Pindex_lastquarter") sheetreplace firstrow(variables)
						save "${OUTPUT_PATH}`i'__${ename`region'}_Pindex_lastquarterS", replace 
					}
				}

				if "`time'" == "ejahr"{

					*Regression 3 Kreise AMR2_pindex_WM_ejahr3
					use "${DATA_PATH}${dname`region'}_pindex_`TYPE'_`time'3.dta", clear
					drop pindex_ m${price`TYPE'}
					rename freq_ freq
					bysort `region' ejahr: replace pindex = . if freq < ${no`UF'}
					drop if ejahr > $maxyear
					duplicates drop
					reshape wide freq pindex, i(`region') j(ejahr)

					forval x = 2008/$maxyear{
						by `region': gen pindex_change`x' = pindex`x' - pindex2008
					}

					drop pindex2*

					if "`region'"== "kid2019" {
						tostring kid2019, gen(RS)
						replace RS = "0" + RS if strlen(RS) == 4
						merge 1:m RS using "${SHAPE_PATH}Kreis/2019/VG250_KRS", keepusing(GEN GF)
						drop if GF == 2

						rename GEN name
						rename RS AGS_${ename`region'}
						order AGS_${ename`region'}, after(`region')
						order name, after(AGS_${ename`region'})
						drop _merge `region' GF

						local i = 3
					}
					else if "`region'" == "gid2019" {
						tostring gid2019, gen(RS)
						replace RS = "0"+RS if strlen(RS) == 8
						merge 1:m RS using "${SHAPE_PATH}Verwaltungsgemeinschaften/2019/VG250_VWG", keepusing(GEN GF)	
						drop if GF == 2
						rename GEN name
						rename RS AGS_${ename`region'}
						order AGS_${ename`region'}, after(`region')
						order name, after(AGS_${ename`region'})
						drop _merge `region' GF


						local i = 5
					}
					else if "`region'" == "AMR2" {
						local i = 7
					}

					forval x = 2008/$maxyear{
						order pindex_change`x', after(freq`x')
					}
					if "`UF'" == "PUF" {
						export excel using "${OUTPUT_PATH}RWIGEOREDX_${`TYPE'name}_${version}.xlsx", sheet("`i'__${ename`region'}_Change_yearly") sheetreplace firstrow(variables)
						save "${OUTPUT_PATH}`i'__${ename`region'}_Change_`TYPE'_yearly", replace 
					}
					if "`UF'" == "SUF" {
						export excel using "${OUTPUT_PATH}RWIGEOREDX_${`TYPE'name}_${version}_SUF.xlsx", sheet("`i'__${ename`region'}_Change_yearly") sheetreplace firstrow(variables)
						save "${OUTPUT_PATH}`i'__${ename`region'}_Change_`TYPE'_yearlyS", replace 
					}
				}
				/* DELETE because its empty
				else if "`time'" == "qudate"{
				}
				*/
				clear
			}
		}
	}
}