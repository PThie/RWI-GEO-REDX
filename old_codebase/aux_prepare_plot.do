*******************************************************************************
* Description

/*
This file generates the shape files needed for plotting. If they already exist,
there is no need to run this file.
*/

*******************************************************************************
* state shapefile to dta

shp2dta using "${SHAPE_PATH}Bundesland/2019/VG250_LAN", database("${SHAPE_PATH}Bundesland/2019/VG250_LAN") coordinates("${SHAPE_PATH}Bundesland/2019/VG250_LAN_coords") genid(id) replace

*******************************************************************************
* district shapefile to dta

shp2dta using "${SHAPE_PATH}Kreis/2019/VG250_KRS", database("${SHAPE_PATH}Kreis/2019/VG250_KRS") coordinates("${SHAPE_PATH}Kreis/2019/VG250_KRS_coords") genid(id) replace

*******************************************************************************
* VWG-shapefile (municipality) to dta

shp2dta using "${SHAPE_PATH}Verwaltungsgemeinschaften/2019/VG250_VWG", database("${SHAPE_PATH}Verwaltungsgemeinschaften/2019/VG250_VWG") coordinates("${SHAPE_PATH}Verwaltungsgemeinschaften/2019/VG250_VWG_coords") genid(id) replace