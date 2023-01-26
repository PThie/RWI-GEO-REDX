* Master - Datei des Projektes "Immobilienpreisindices RWI-GEO-REDX"
* erschienen als Datensatz RWI-GEO-REDX
* veröffentlicht unter dem Titel "Real Estate Indices (RWI-GEO-REDX)" als RWI Projektbericht: Datenbeschreibung
* Autoren: Larissa Klick, Sandra Schaffner, Patrick Thiel
* Datum: 24.03.2020, updated 17.02.2021

version 14.2
clear all
set more off, permanently

set matsize 10000
capture log close

global ROOT "M:\_FDZ\RWI-GEO\RWI-GEO-REDX\"
cd "${ROOT}" 

***************** with new wave, needs to be updated!!!
***************** lines 14 to 29 need to be adjusted with new wave

global maxyear 2022 //givex the maximum year of the data 

global maxyear2 2022 //gives year in which the last full quarter lies

global maxmonth 06 // gives the maximum month in the data (in the last available year?)

global month "Jul2022"
global diff = $maxyear2 - 2008 
global version "v9"
global maxqudate 249 // gives the maximum quarter

global red_version "v7" // latest RED version which should be used here

sysdir set PERSONAL "M:\_FDZ\RWI-GEO\RWI-GEO-REDX\Immobilienpreisindizes\ado\"


global varWM "i.constr first_occupancy  balkon garten einbaukueche gaestewc keller i.ausstattung zimmeranzahl_full"
global depWM "ln_rent_sqm"
global varHK "i.constr first_occupancy gaestewc einliegerwohnung i.ausstattung zimmeranzahl_full i.plotarea_size typ_*"
global depHK "ln_houseprice_sqm"
global varWK "i.constr first_occupancy  balkon garten einbaukueche gaestewc aufzug keller betreut i.ausstattung declared_wohngeld zimmeranzahl_full nofloors catfloors"
global depWK "ln_flatprice_sqm"
global priceHK "houseprice"
global priceWK "flatprice"
global priceWM "rent"



global dnamekid2019 "kreis"
global dnamegid2019 "gemeinde"
global dnamegrid "grid"
global dnameAMR2 "AMR2"

global enamekid2019 "District"
global enamegid2019 "Municip"
global enamegrid "Grid"
global enameAMR2 "AMR2"

global WMname "ApRent"
global HKname "HouPurc"
global WKname "ApPurc"

global noSUF = 5
global noPUF = 50



global TYPES "WK WM HK"

do "aufbereitung\\${version}\1_prepare.do"
do "aufbereitung\\${version}\2_regression.do"
do "aufbereitung\\${version}\3_plot.do"
do "aufbereitung\\${version}\4_export.do"



** needed?
* do "M:\_FDZ\RWI-GEO\RWI-GEO-REDX\daten\aufbereitung\prog\2_regression_reg2only_PT.do"
* do "M:\_FDZ\RWI-GEO\RWI-GEO-REDX\daten\aufbereitung\prog\2_regression_reg1only_PT.do"
* do "M:\_FDZ\RWI-GEO\RWI-GEO-REDX\daten\aufbereitung\prog\3_plot_LK.do"
do "M:\_FDZ\RWI-GEO\RWI-GEO-REDX\aufbereitung\prog\3_plot_changes_new_categories.do"
do "M:\_FDZ\RWI-GEO\RWI-GEO-REDX\aufbereitung\prog\2_temp.do"






