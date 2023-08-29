*******************************************************************************
* Description

/*
This file generates the Real Estate Indices (RWI-GEO-REDX).

*/

*******************************************************************************
* general set up

version 14.2
clear all
set more off, permanently
set matsize 10000
capture log close

* set directory
global ROOT "M:\_FDZ\RWI-GEO\RWI-GEO-REDX\"
cd "${ROOT}" 

*******************************************************************************
* globals //!!!! TO DO NEW DELIVERY !!!!

* maximum year in the data
global maxyear 2023

* maximum year with the last full quarter
global maxyear2 2023

* maximum month in the data
global maxmonth 6

* maximum quarter in the data
global maxqudate 253
/* 
NOTE way to find the max quarter:
	gen qdate = qofd(dofm(ym(ejahr, emonat)))
	tab qdate
*/

* time range
global diff = $maxyear2 - 2008 

* define month of delivery (for folder structure)
global month "Jun2023"

* define version of the wave
global version "v11"

* define version of the latest RED version
global red_version "v9"

*******************************************************************************
* paths

* ado path
sysdir set PERSONAL "${ROOT}output/ado/"

*******************************************************************************
* variables for regression

global varWM "i.constr first_occupancy  i.balkon i.garten i.einbaukueche i.gaestewc i.keller i.ausstattung zimmeranzahl_full"
global depWM "ln_rent_sqm"
global varHK "i.constr first_occupancy i.gaestewc einliegerwohnung i.ausstattung zimmeranzahl_full i.plotarea_size typ_*"
global depHK "ln_houseprice_sqm"
global varWK "i.constr first_occupancy  i.balkon i.garten i.einbaukueche i.gaestewc i.aufzug i.keller i.betreut i.ausstattung declared_wohngeld zimmeranzahl_full nofloors catfloors"
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

*******************************************************************************
* Workflow steps

* STEP 1: Preparation of the data
do "aufbereitung/1_prepare.do"

* STEP 2: Running regression
do "aufbereitung/2_regression.do"

* STEP 3: Plotting outcome
do "aufbereitung/3_plot.do"

* STEP 4: Exporting the data
do "aufbereitung/4_export.do"
