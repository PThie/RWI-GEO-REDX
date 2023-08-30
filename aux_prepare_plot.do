*3a_prepare_plot.do

* Shapefile-Vorbereitung für Projekt "Immobilienpreisindices RWI-GEO-REDX"
* aus Shapefile -- Statakompatible-Geo-Dateien
* 
* Autoren: Larissa Klick, Sandra Schaffner
* Datum: 24.03.2020

*set path 
*global SHAPE_PATH "M:/_FDZ/interne Daten/Gebietseinheit/"

*Bundesland Shapefile to dta
shp2dta using "${SHAPE_PATH}Bundesland/2019/VG250_LAN", database("${SHAPE_PATH}Bundesland/2019/VG250_LAN") coordinates("${SHAPE_PATH}Bundesland/2019/VG250_LAN_coords") genid(id) replace


*Kreis Shapefile to dta
shp2dta using "${SHAPE_PATH}Kreis/2019/VG250_KRS", database("${SHAPE_PATH}Kreis/2019/VG250_KRS") coordinates("${SHAPE_PATH}Kreis/2019/VG250_KRS_coords") genid(id) replace


*VWG-Shapefile to dta
shp2dta using "${SHAPE_PATH}Verwaltungsgemeinschaften/2019/VG250_VWG", database("${SHAPE_PATH}Verwaltungsgemeinschaften/2019/VG250_VWG") coordinates("${SHAPE_PATH}Verwaltungsgemeinschaften/2019/VG250_VWG_coords") genid(id) replace

